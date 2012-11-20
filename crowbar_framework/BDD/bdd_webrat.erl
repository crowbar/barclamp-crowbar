% Copyright 2012, Dell 
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
% Author: RobHirschfeld 
% 
-module(bdd_webrat).
-export([step/3]).

% helper routine
click_link(Config, URL, Link) ->
	Result = case URL of
		[] -> io:format("CANNOT FIND LINK ~s~n", [Link]), error;
		_ -> eurl:get(Config, URL, ok)
	end,
	Result.

step(_Config, _Global, {step_given, _N, ["I am on the home page"]}) -> 
	eurl:get(_Config, []);

step(_Config, _Global, {step_given, _N, ["I am on the", Page, "page"]}) ->
  step(_Config, _Global, {step_given, _N, ["I went to the", Page, "page"]});

step(_Config, _Global, {step_given, _N, ["I went to the", Page, "page"]}) ->
	eurl:get(_Config, Page);
	
step(_Config, _Given, {step_when, _N, ["I go to the home page"]}) -> 
	eurl:get(_Config, []);

step(_Config, _Given, {step_when, _N, ["I go to the", Page, "page"]}) -> 
	eurl:get(_Config, Page);

step(_Config, _Given, {step_when, _N, ["I try to go to the", Page, "page"]}) ->
	eurl:get(_Config, Page, not_found);

step(Config, Given, {step_when, _N, ["I click on the",Link,"link"]}) -> 
	[URL | _] = [eurl:find_link(Link, HTML) || HTML <- (Given), HTML =/= []],
	click_link(Config, URL, Link);

step(Config, Given, {step_when, _N, ["I click on the", Menu, "menu item"]}) -> 
  [Block] = eurl:find_block("<li", "</li>", Given, ">"++Menu++"</a>", 250),
  URL = eurl:find_link(Menu, Block),
  click_link(Config, URL, Menu);

step(Config, Result, {step_then, _N, ["I should not see", Text]}) -> 
	bdd_utils:log(Config, trace, "step_then result ~p should NOT have ~p on the page~n", [Result, Text]),
	eurl:search(Text,Result, false);

step(Config, Result, {step_then, _N, ["I should not see", Text, "in section", Id]}) -> 
	bdd_utils:log(Config, trace, "step_then result ~p should NOT have ~p on the page~n", [Result, Text]),
	Body = [eurl:html_body(R) || R <- Result],
	Section = [eurl:find_div(B, Id) || B <- Body],
	eurl:search(Text,Section, false);
	
step(Config, _Result, {step_then, _N, ["I should see", Text]}) -> 
	bdd_utils:log(Config, trace,"step_then result ~p should have ~p on the page~n", [_Result, Text]),
	eurl:search(Text,_Result);

step(Config, Result, {step_then, _N, ["I should see", Text, "in section", Id]}) -> 
	bdd_utils:log(Config, trace, "step_then result ~p should have ~p on the page~n", [Result, Text]),
	Body = [eurl:html_body(R) || R <- Result],
	Section = [eurl:find_div(B, Id) || B <- Body],
	eurl:search(Text,Section);

step(Config, _Result, {step_then, _N, ["there should be no translation errors"]}) -> 
  TransError = bdd_utils:config(Config, translation_error),
  eurl:search(TransError,_Result, false);

step(_Config, Result, {step_then, _N, ["I should see a link to", Link]}) ->
  bdd_utils:assert([ 
  	try eurl:find_link(Link,R) of
  		_ -> true
  	catch
  	  error: E -> io:format("FAIL: Did not find ~p in page (Error: ~p)~n", [Link, E]), false
  	end
  	|| R <- Result]
	);
  
step(_Config, Result, {step_then, _N, ["I should see a button with", Button]}) -> 
  try eurl:find_button(Button,Result) of
    _ -> true
	catch
	  error: E -> io:format("FAIL: Did not find ~p in page (Error: ~p)~n", [Button, E]), false
	end;
	
step(_Config, Result, {step_then, _N, ["I should download a PDF"]}) -> 
  bdd_utils:assert([
    case string:substr(R, 1, 4) of 
      "%PDF" -> true;
      _ -> false
    end
    || R <- Result ]
    );

step(_Config, Result, {step_then, _N, ["I should download text with", Text]}) -> 
	RE = case re:compile(Text, [multiline, dotall, {newline , anycrlf}]) of
	  {ok, R} -> R;
	  Error -> io:format("ERROR: Could not parse regex: '~p'.", [Error]), []
	end,
	case re:run(Result, RE) of
	  {match, _} -> true;
	  _ -> false
	end;

step(_Config, _Result, {step_then, _N, ["I should see a menu for", Menu]}) -> 
  bdd_utils:assert([eurl:find_block("<li", "</li>", R, Menu) =/= [] || R <- _Result]);

step(Config, Result, {step_then, _N, ["we should get a 404 return"]}) -> 
  bdd_utils:log(Config, puts, "404 return had ~p", Result),
  Result =:= [not_found];

                                                                
step(Config, _Given, {step_when, _N, ["AJAX requests the",Page,"page"]}) ->
  % depricated
  bdd_restrat:step(Config, _Given, {step_when, _N, ["REST requests the",Page,"page"]});

step(_Config, _Result, {_Type, _N, ["END OF WEBRAT"]}) ->
  false.
