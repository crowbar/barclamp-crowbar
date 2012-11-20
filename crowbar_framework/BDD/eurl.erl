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
-module(eurl).
-export([post/3, put/3, delete/3, post_params/1, post/5, put_post/4, uri/2, path/2]).
-export([get/2, get/3, get_page/3, peek/2, search/2, search/3]).
-export([find_button/2, find_link/2, find_block/4, find_block/5, find_div/2, html_body/1, html_head/1]).

search(Match, Results, Test) ->
	F = fun(X) -> case {X, Test} of 
	  {true, true} -> true; 
	  {true, false} -> false;
	  {_, false} -> true;
	  {_, true} -> false end end,
	lists:any(F, ([peek(Match,Result) || Result <- Results, Result =/= [no_op]])).
search(Match, Results) ->
	search(Match, Results, true).

html_peek(Input, RegEx) ->
	{ok, RE} = re:compile(RegEx, [caseless, multiline, dotall, {newline , anycrlf}]),
	bdd_utils:log(trace,"html:peek compile: ~p on ~p~n", [RegEx, Input]),
	Result = re:run(Input, RE),
	bdd_utils:log(trace, "html:peek match: ~p~n", [Result]),
	%{ match, [ {_St, _Ln} | _ ] } = Result,
	%bdd_utils:debug("html_peek substr: ~p~n", [string:substr(Input, _St, _Ln)]),
	case Result of
		{match, [_A, _B, _C, _D, {Start, Length} | _Tail ]} -> string:substr(Input, Start-5, Length+13);
		_ -> false
	end.
  
html_body(Input) ->
  RegEx = "<(html|HTML)(.*)<(body|BODY)>(.*)</(body|BODY)>(.*)</(html|HTML)>",
  html_peek(Input, RegEx).
  
html_head(Input) ->
  RegEx = "<(html|HTML)(.*)<(head|HEAD)>(.*)</(head|HEAD)>(.*)</(html|HTML)>",
  html_peek(Input, RegEx).
  
peek(Match, Input) ->
  RegEx = Match,
	{ok, RE} = re:compile(RegEx, [caseless, multiline, dotall, {newline , anycrlf}]),
	bdd_utils:log(trace, "html:peek compile: ~p on ~p~n", [RegEx, Input]),
	Result = re:run(Input, RE),
	bdd_utils:log(trace, "html:peek match: ~p~n", [Result]),
	%{ match, [ {_St, _Ln} | _ ] } = Result,
	%bdd_utils:debug("html_peek substr: ~p~n", [string:substr(Input, _St, _Ln)]),
	case Result of
		{match, _} -> true;
		_ -> Result
	end.
	
	
find_button(Match, Input) ->
	Form = find_block("<form ", "</form>", Input, "value='"++Match++"'"),
	Button = find_block("<input ", ">", Form,  "value='"++Match++"'"),
	{ok, RegEx} = re:compile("type='submit'"),
	case re:run(Button, RegEx) of
	  {match, _} -> Button;
	  _ -> io:format("ERROR: Could not find button with value  '~p'.  HTML could have other components encoded in a tag~n", [Match]), throw("could not find_button")
	end.
	
% return the HREF part of an anchor tag given the content of the link
find_link(Match, Input) ->
	RegEx = "(\\<(a|A)\\b(/?[^\\>]+)\\>"++Match++"\\<\\/(a|A)\\>)",
	RE = case re:compile(RegEx, [multiline, dotall, {newline , anycrlf}]) of
	  {ok, R} -> R;
	  Error -> io:format("ERROR: Could not parse regex '~p' given '~p'.~n", [Error, RegEx])
	end,
	AnchorTag = case re:run(Input, RE) of
	  {match, [{AStart, ALength} | _]} -> string:substr(Input, AStart+1,AStart+ALength);
	  nomatch -> io:format("ERROR: Could not find ~s in request  (you may need to escape characters).", [Match]);
	  {_, _} -> io:format("ERROR: Could not find Anchor tags enclosing '~p'.  HTML could have other components encoded in a tag~n", [Match]), throw("could not find_link")
	end,
	{ok, HrefREX} = re:compile("\\bhref=(['\"])([^\\s]+?)(\\1)", [multiline, dotall, {newline , anycrlf}]),
	Href = case re:run(AnchorTag, HrefREX) of
	  {match, [_1, _2, {HStart, HLength} | _]} -> string:substr(AnchorTag, HStart+1,HLength);
	  nomatch -> io:format("ERROR: Could not find ~s in request (you may need to escape characters)", [Match]);
	  {_, _} -> io:format("ERROR: Could not find href= information in substring '~p'~n", [AnchorTag]), throw("could not html_find_link")
	end,
	bdd_utils:log(trace, "bdd_utils: find_link anchor ~p~n", [AnchorTag]),
	%bdd_utils:debug(, "html_find_link href regex~p~n", [re:run(AnchorTag, HrefREX)]),
	bdd_utils:log(trace, "bdd_utils: find_link found path ~p~n", [Href]),
	Href.

find_div([], _)       -> not_found;
find_div(Input, Id)   ->
  Start = string:str(Input,"<div"),
  case Start of
    0 -> not_found;
    _ -> Block = string:substr(Input, Start+5),
         Close = string:str(Block,">"),
         Tag = string:substr(Block, 1, Close-1),
         Next = string:substr(Block, Close+1),
         RegEx = "[id|ID]=['|\""++Id++"['|\"]",
         case re:run(Tag, RegEx) of
           {match, _} -> Next;
           nomatch -> find_div(Next, Id)
         end
  end.

% we allow for a of open tags (nesting) but only the inner close is needed
find_block(OpenTag, CloseTag, Input, Match)         -> find_block(OpenTag, CloseTag, Input, Match, 1000).
find_block(OpenTag, CloseTag, Input, Match, MaxLen) ->
  {ok, RE} = re:compile([Match]),
  CandidatesNotTested = re:split(Input, OpenTag, [{return, list}]),
  Candidates = [ find_block_helper(C, RE) || C <- CandidatesNotTested ],
  Block = case [ C || C <- Candidates, C =/= false ] of
    [B] -> B;
    [B | _] -> B;
    _ -> []
  end,
  case re:split(Block, CloseTag, [{parts, 2}, {return, list}]) of
    [Inside] -> [Inside];
    [Inside | _ ] -> [Inside];
    [] -> [string:substr(Block,0,MaxLen)]  % we need a fall back limit just in case
  end.

find_block_helper(Test, RE) ->
	case re:run(Test, RE) of
		{match, _} -> Test;
		_ -> false
	end.

uri(Config, Path) ->
	{url, Base} = lists:keyfind(url,1,Config),
	path(Base,Path).
	
path(Base, Path) ->
  case {string:right(Base,1),string:left(Path,1)} of
    {"/", "/"}-> Base ++ string:substr(Path,2);
    {_, "/"}  -> Base ++ Path;
    {"/", _}  -> Base ++ Path;
    {_, _}    -> Base ++ "/" ++ Path
  end.
  
% get a page from a server
get_page(Config, Page, Codes) -> get(Config, uri(Config, Page), Codes).
get(Config, Page)             -> get_page(Config, Page, []).
get(Config, Page, ok)         -> get_page(Config, Page, []);
get(Config, Page, not_found)  -> get_page(Config, Page, [{404, not_found}]);
get(Config, URL, OkReturnCodes) ->
  bdd_utils:log(Config, info, "Getting ~p~n", [URL]),
	Result = simple_auth:request(Config, URL),
	{_, {{_HTTP, Code, _CodeWord}, _Header, Body}} = Result,
  bdd_utils:log(Config, trace, "bdd_utils:get Result ~p: ~p~n", [Code, Body]),
	{ok, {{"HTTP/1.1",_ReturnCode,State}, _Head, Body}} = Result,
  case _ReturnCode of
    200 -> Body;
    _ -> case lists:keyfind(_ReturnCode, 1, OkReturnCodes) of
           false -> throw({errorWhileGetting, _ReturnCode, "ERROR: get attempt at " ++ URL ++ " failed.  Return code: " ++ integer_to_list(_ReturnCode) ++ " (" ++ State ++ ")\nResult: " ++ Body});
           {_, _ReturnAtom} -> _ReturnAtom
         end
  end.
  
post_params(ParamsIn) -> post_params(ParamsIn, []).
post_params([], Params) -> Params;
post_params([{K, V} | P], ParamsOrig) -> 
  ParamsAdd = case ParamsOrig of
    [] -> "?"++K++"="++V;
    _ -> "&"++K++"="++V
  end,
  post_params(P, ParamsOrig++ParamsAdd).

% Post using Parameters to convey the values
post(Config, URL, Parameters, _ReturnCode, StateRegEx) ->
  Post = URL ++ post_params(Parameters),
  {ok, {{"HTTP/1.1",_ReturnCode, State}, _Head, Body}} = simple_auth:request(Config, post, {Post, "application/json", "application/json", "body"}, [{timeout, 10000}], []),  
 	{ok, StateMP} = re:compile(StateRegEx),
	case re:run(State, StateMP) of
		{match, _} -> Body;
    _ -> throw({errorWhilePosting, _ReturnCode, "ERROR: post attempt at " ++ Post ++ " failed.  Return code: " ++ integer_to_list(_ReturnCode) ++ " (" ++ State ++ ")\nBody: " ++ Body})
	end. 

% Post using JSON to convey the values
post(Config, Path, JSON) ->
  put_post(Config, Path, JSON, post).

put(Config, Path, JSON) ->
  put_post(Config, Path, JSON, put).
  
% Put using JSON to convey the values
put_post(Config, Path, JSON, Action) ->
  put_post(Config, Path, JSON, Action, []).
put_post(Config, Path, JSON, Action, _OkReturnCodes) ->
  URL = uri(Config, Path),
  bdd_utils:log(Config, info, "~ping to ~p~n", [atom_to_list(Action), URL]),
  Result = simple_auth:request(Config, Action, {URL, [], "application/json", JSON}, [{timeout, 10000}], []),  
	{_, {{_HTTP, Code, _CodeWord}, _Header, Body}} = Result,
  bdd_utils:log(Config, trace, "bdd_utils:put_post Result ~p: ~p~n", [Code, Body]),
  {ok, {{"HTTP/1.1",_ReturnCode, State}, _Head, Body}} = Result,
  case _ReturnCode of
    200 -> json:parse(Body);
    _ -> case lists:keyfind(_ReturnCode, 1, _OkReturnCodes) of
           false -> throw({errorWhilePuttingOrPosting, _ReturnCode, "ERROR: " ++ atom_to_list(Action) ++ " attempt at " ++ URL ++ " failed.  Return code: " ++ integer_to_list(_ReturnCode) ++ " (" ++ State ++ ")\nBody: " ++ Body});
           {_, _ReturnAtom} -> _ReturnAtom
         end
  end.

delete(Config, Path, Id) ->
  delete(Config, Path, Id, []).
delete(Config, Path, Id, _OkReturnCodes) ->
  URL = uri(Config, Path) ++ "/" ++ Id,
  bdd_utils:log(Config, info, "Deleting ~p~n", [URL]),
  Result = simple_auth:request(Config, delete, {URL}, [{timeout, 10000}], []),  
	{_, {{_HTTP, Code, _CodeWord}, _Header, Body}} = Result,
  bdd_utils:log(Config, trace, "bdd_utils:delete Result ~p: ~p~n", [Code, Body]),
  {ok, {{"HTTP/1.1",_ReturnCode, State}, _Head, Body}} = Result,
  case _ReturnCode of
    200 -> true;
    _ -> case lists:keyfind(_ReturnCode, 1, _OkReturnCodes) of
           false -> throw({errorWhileDeleting, _ReturnCode, "ERROR: deletion attempt at " ++ URL ++ " failed.  Return code: " ++ integer_to_list(_ReturnCode) ++ " (" ++ State ++ ")\nBody: " ++ Body});
           {_, _ReturnAtom} -> _ReturnAtom
         end
  end.
