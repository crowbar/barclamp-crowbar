% Copyright 2013 Dell 
% 
% Licensed under the Apache License, Version 2.0 (the "License"); 
% you may not use this file except in compliance with the License. 
% You may obtain a copy of the License at 
% 
%  eurl://www.apache.org/licenses/LICENSE-2.0 
% 
% Unless required by applicable law or agreed to in writing, software 
% distributed under the License is distributed on an "AS IS" BASIS, 
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
% See the License for the specific language governing permissions and 
% limitations under the License. 
% 
% 
-module(bdd_webrat).
-export([step/2, step/3]).
-include("bdd.hrl").

% helper routine
click_link(URL, Link) ->
	Result = case URL of
		[]  -> bdd_utils:log(warn, bdd_webrat, click_link, "CANNOT FIND LINK ~p", [Link]), error;
		_   -> eurl:get_http(URL)
	end,
	Result.

% DEPRICATE!
step(_Config, B, C) -> bdd_utils:depricate({2013, 12, 1}, bdd_restrat, step, bdd_restrat, step, [B, C]).

step(_Global, {step_given, _N, ["I am on the home page"]}) -> 
	eurl:get_http([]);

step(Global, {step_given, _N, ["I am on the", Page, "page"]}) ->
  step(Global, {step_given, _N, ["I went to the", Page, "page"]});
  
step(_Global, {step_given, _N, ["I went to the", Page, "page"]}) ->
	eurl:get_http(Page);

step(_Global, {step_given, {Scenario, _N}, ["I am on the", Page, "page with parameter", Key]}) ->
  Param = bdd_utils:scenario_retrieve(Scenario, Key, ""),
  URL = Page ++ "?" ++ Key ++ "=" ++ Param,
  bdd_utils:log(debug, bdd_webrat, step, "Getting ~p for page ~p + ~p=~p",[URL, Page, Key, Param]),
  eurl:get_http(URL);
	
step(_Global, {step_given, {Scenario, _N}, ["parameter",Key,"is",Value]}) ->
  bdd_utils:log(debug, bdd_webrat, step, "Store parameter ~p = ~p", [Key, Value]),
  bdd_utils:scenario_store(Scenario, Key, Value),
  [];

step(_Given, {step_when, _N, ["I go to the home page"]}) -> 
	eurl:get_http([]);

step(_Given, {step_when, _N, ["I go to the", Page, "page"]}) -> 
	eurl:get_http(Page);

step(_Given, {step_when, _N, ["I try to go to the", Page, "page"]}) ->
	eurl:get_http(Page);

step(Given, {step_when, _N, ["I click on the",Link,"link"]}) -> 
	[URL | _] = [eurl:find_link(Link, HTML) || HTML <- (Given), HTML =/= []],
	click_link(URL, Link);

step(Given, {step_when, _N, ["I click on the", Menu, "menu item"]}) -> 
  [Block] = eurl:find_block("<li", "</li>", Given, ">"++Menu++"</a>", 250),
  URL = eurl:find_link(Menu, Block),
  click_link(URL, Menu);

step(Given, {step_when, _N, ["I fill in", Fields, "and submit using the",ButtonText,"button"]}) ->
  % assume a single form element
  Form = eurl:find_form(Given, ButtonText),
  bdd_utils:log(trace, "bdd_webrat:step When I fill in is Given ~p using ~p",[Form, Fields]),
  {fields, FormFields} = lists:keyfind(fields, 1, Form),
  SubmitFields = {fields, [eurl:form_fields_merge(F, Fields) || F <- FormFields]},
  NewForm = lists:keyreplace(fields, 1, Form, SubmitFields),
  bdd_utils:log(trace, "bdd_webrat:step When I fill in submitting ~p",[NewForm]),
  eurl:form_submit(NewForm);
  
step(Result, {step_then, _N, ["I should not see", Text]}) -> 
	bdd_utils:log(trace, "step_then result ~p should NOT have ~p on the page", [Result, Text]),
	eurl:search(Text,Result, false);

step(Result, {step_then, _N, ["I should not see", Text, "in section", Id]}) -> 
	bdd_utils:log(trace, "step_then result ~p should NOT have ~p on the page", [Result, Text]),
	Body = [eurl:html_body(R) || R <- Result],
	Section = [eurl:find_div(B, Id) || B <- Body],
	eurl:search(Text,Section, false);

step(Result, {step_then, N, ["I should see a heading", Text]}) -> 
  step(Result, {step_then, N, ["I should see heading", Text]});
step(Result, {step_then, _N, ["I should see heading", Text]}) -> 
	bdd_utils:log(debug, bdd_webrat, step, "see heading ~p", [Text]),
  R = eurl:get_result(Result, http),
	eurl:find_heading(R#http.data,Text);

step(Result, {step_then, _N, ["I should see", Text]}) -> 
  R = eurl:get_result(Result, http), 
	bdd_utils:log(trace, bdd_restrat, step, "I should see ~p on ~p", [Text, R#http.url]),
	eurl:search(Text,R);

step(Result, {step_then, _N, ["I should see", Text, "in section", Id]}) -> 
  R = eurl:get_result(Result, http),
  bdd_utils:log(trace, bdd_restrat, step, "~p should have ~p on the page~n", [Result#http.url, Text]),
	Body = R#http.data,
	Section = eurl:find_div(Body, Id),
	eurl:search(Text,Section);

step(Result, {step_then, _N, ["there are no i18n errors"]}) -> 
  step(Result, {step_then, _N, ["there should be no translation errors"]}); 
step(Result, {step_then, _N, ["there are no localization errors"]}) -> 
  step(Result, {step_then, _N, ["there should be no translation errors"]});
step(Result, {step_then, _N, ["there should be no translation errors"]}) -> 
  TransError = bdd_utils:config(translation_error),
  R = eurl:get_result(Result, http),
  eurl:search(TransError,R, false);

step(Result, {step_then, _N, ["I should see a link to", Link]}) ->
  bdd_utils:assert([ 
  	try eurl:find_link(Link,R) of
  		_ -> true
  	catch
  	  error: E -> bdd_utils:log(error, bdd_webrat, step, "Did not find link to ~p (Error: ~p)", [Link, E]), false
  	end
  	|| R <- Result]
	);
  
step(Result, {step_then, _N, ["I should see a button with", Button]}) -> 
  R = eurl:get_result(Result, http),
  try eurl:find_button(Button,R) of
    _ -> true
	catch
	  error: E -> bdd_utils:log(warn, bdd_webrat, step, "Did not find ~p in page (Error: ~p)", [Button, E]), false
	end;
	
step(Result, {step_then, _N, ["I should download a PDF"]}) -> 
  bdd_utils:assert([
    case string:substr(R, 1, 4) of 
      "%PDF" -> true;
      _ -> false
    end
    || R <- Result ]
    );

step(Result, {step_then, _N, ["I should download text with", Text]}) -> 
	RE = case re:compile(Text, [multiline, dotall, {newline , anycrlf}]) of
	  {ok, R} -> R;
	  Error -> io:format("ERROR: Could not parse regex: '~p'.", [Error]), []
	end,
	case re:run(Result, RE) of
	  {match, _} -> true;
	  _ -> false
	end;

step(_Result, {step_then, _N, ["I should see a menu for", Menu]}) -> 
  bdd_utils:assert([eurl:find_block("<li", "</li>", R, Menu) =/= [] || R <- _Result]);

step(Result, {step_then, _N, ["we should get a 404 return"]}) -> 
  bdd_utils:log(puts, "404 return had ~p", Result),
  Result =:= [not_found];

                                                                
step(_Given, {step_when, _N, ["AJAX requests the",Page,"page"]}) ->
  bdd_utils:depricated({2013,6,1}, bdd_webrat, step, bdd_restrat, step, [_Given, {step_when, _N, ["REST requests the",Page,"page"]}]);

step(_Result, {_Type, _N, ["END OF WEBRAT"]}) ->
  false.
