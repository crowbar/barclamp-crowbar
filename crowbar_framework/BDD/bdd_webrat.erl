% Copyright 2012, Dell 
% 
% Licensed under the Apache License, Version 2.0 (the "License"); 
% you may not use this file except in compliance with the License. 
% You may obtain a copy of the License at 
% 
%  http://www.apache.org/licenses/LICENSE-2.0 
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
-import(bdd_util).

% helper routine
click_link(Config, URL, Link) ->
  _Debug = false,
  bdd_utils:debug(_Debug, "URL ~p~n",[URL]),
	Result = case URL of
		[] -> io:format("CANNOT FIND LINK ~s~n", [Link]), error;
		_ -> bdd_utils:http_get(Config, URL, ok)
	end,
	bdd_utils:debug(_Debug, "when I click result ~p~n", [Result]),
	Result.

step(_Config, _Global, {step_given, _N, ["I am on the home page"]}) -> 
	bdd_utils:debug("start from home.~n"), 
	bdd_utils:http_get(_Config, []);

step(_Config, _Global, {step_given, _N, ["I am on the", Page, "page"]}) ->
  step(_Config, _Global, {step_given, _N, ["I went to the", Page, "page"]});

step(_Config, _Global, {step_given, _N, ["I went to the", Page, "page"]}) ->
	bdd_utils:http_get(_Config, Page);
	
step(_Config, _Given, {step_when, _N, ["I go to the home page"]}) -> 
	bdd_utils:debug("go home.~n"), 
	bdd_utils:http_get(_Config, []);

step(_Config, _Given, {step_when, _N, ["I go to the", Page, "page"]}) -> 
	bdd_utils:debug("go to the ~p page~n", [Page]), 
	bdd_utils:http_get(_Config, Page);

step(_Config, _Given, {step_when, _N, ["I try to go to the", Page, "page"]}) ->
	%bdd_utils:debug("expect FAIL when going to the ~p page~n", [Page]), 
	bdd_utils:http_get(_Config, Page, not_found);

step(_Config, _Given, {step_when, _N, ["AJAX requests the",Page,"page"]}) ->
  JSON = bdd_utils:http_get(_Config, Page),
  {ajax, json:parse(JSON), Page};

step(Config, Given, {step_when, _N, ["I click on the",Link,"link"]}) -> 
	[URL | _] = [bdd_utils:html_find_link(Link, HTML) || HTML <- (Given), HTML =/= []],
	click_link(Config, URL, Link);

step(Config, Given, {step_when, _N, ["I click on the", Menu, "menu item"]}) -> 
  [Block] = bdd_utils:html_find_block("<li", "</li>", Given, ">"++Menu++"</a>"),
  URL = bdd_utils:html_find_link(Menu, Block),
  click_link(Config, URL, Menu);

step(_Config, _Result, {step_then, _N, ["I should not see", Text]}) -> 
	bdd_utils:debug("step_then result ~p should NOT have ~p on the page~n", [_Result, Text]),
	bdd_utils:html_search(Text,_Result, false);
	
step(_Config, _Result, {step_then, _N, ["I should see", Text]}) -> 
	bdd_utils:debug("step_then result ~p should have ~p on the page~n", [_Result, Text]),
	bdd_utils:html_search(Text,_Result);

step(Config, _Result, {step_then, _N, ["there should be no translation errors"]}) -> 
  TransError = bdd_utils:config(Config, translation_error),
  bdd_utils:html_search(TransError,_Result, false);

step(_Config, Result, {step_then, _N, ["I should see a link to", Link]}) ->
  bdd_utils:assert([ 
  	try bdd_utils:html_find_link(Link,R) of
  		_ -> true
  	catch
  	  error: E -> io:format("FAIL: Did not find ~p in page (Error: ~p)~n", [Link, E]), false
  	end
  	|| R <- Result]
	);
  
step(_Config, Result, {step_then, _N, ["I should see a button with", Button]}) -> 
  try bdd_utils:html_find_button(Button,Result) of
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

step(_Config, Results, {step_then,_N, ["key",Key,"should be",Value]}) ->
  {ajax, JSON, _} = lists:keyfind(ajax, 1, Results),     % ASSUME, only 1 ajax result per feature
  Value =:= json:value(JSON, Key);

step(_Config, Results, {step_then, _N, ["key",Key,"should contain at least",Count,"items"]}) ->
  {C, _} = string:to_integer(Count),
  {ajax, JSON, _} = lists:keyfind(ajax, 1, Results),     % ASSUME, only 1 ajax result per feature
  List = json:value(JSON, Key),
  Items = length(List),
  Items >= C;

step(_Config, _Result, {step_then, _N, ["I should see a menu for", Menu]}) -> 
  bdd_utils:assert([bdd_utils:html_find_block("<li", "</li>", R, Menu) =/= [] || R <- _Result]);
                                                                
step(_Config, _Result, {_Type, _N, ["END OF WEBRAT"]}) ->
  false.