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
-export([post/3, delete/3, post_params/1, post/5]).
-export([get/2, get/3, get/4, peek/2, search/2, search/3]).
-export([find_button/2, find_link/2, find_block/4]).

search(Match, Results, Test) ->
	F = fun(X) -> case {X, Test} of 
	  {true, true} -> true; 
	  {true, false} -> false;
	  {_, false} -> true;
	  {_, true} -> false end end,
	lists:any(F, ([peek(Match,Result) || Result <- Results, Result =/= [no_op]])).
search(Match, Results) ->
	search(Match, Results, true).
  
peek(Match, Input) ->
  RegEx = "<(html|HTML)(.*)"++Match++"(.*)</(html|HTML)>",
	{ok, RE} = re:compile(RegEx, [caseless, multiline, dotall, {newline , anycrlf}]),
	bdd_utils:debug("html:peek compile: ~p on ~p~n", [RegEx, Input]),
	Result = re:run(Input, RE),
	bdd_utils:debug("html:peek match: ~p~n", [Result]),
	%{ match, [ {_St, _Ln} | _ ] } = Result,
	%bdd_utils:debug("html_peek substr: ~p~n", [string:substr(Input, _St, _Ln)]),
	case Result of
		{match, _} -> true;
		_ -> Result
	end.
	
	
find_button(Match, Input) ->
  %<form.. <input class="button" name="submit" type="submit" value="Save"></form>
  %debug(puts,Match),
	Form = find_block("<form ", "</form>", Input, "value='"++Match++"'"),
	%debug(puts,Form),
	Button = find_block("<input ", ">", Form,  "value='"++Match++"'"),
	%debug(puts,Button),
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
	  Error -> io:format("ERROR: Could not parse regex: '~p'.", [Error])
	end,
	AnchorTag = case re:run(Input, RE) of
	  {match, [{AStart, ALength} | _]} -> string:substr(Input, AStart+1,AStart+ALength);
	  {_, _} -> io:format("ERROR: Could not find Anchor tags enclosing '~p'.  HTML could have other components encoded in a tag~n", [Match]), throw("could not find_link")
	end,
	{ok, HrefREX} = re:compile("\\bhref=(['\"])([^\\s]+?)(\\1)", [multiline, dotall, {newline , anycrlf}]),
	Href = case re:run(AnchorTag, HrefREX) of
	  {match, [_1, _2, {HStart, HLength} | _]} -> string:substr(AnchorTag, HStart+1,HLength);
	  {_, _} -> io:format("ERROR: Could not find href= information in substring '~p'~n", [AnchorTag]), throw("could not html_find_link")
	end,
	bdd_utils:debug("find_link anchor ~p~n", [AnchorTag]),
	%bdd_utils:debug(, "html_find_link href regex~p~n", [re:run(AnchorTag, HrefREX)]),
	bdd_utils:debug("find_link found path ~p~n", [Href]),
	Href.

% we allow for a of open tags (nesting) but only the inner close is needed
find_block(OpenTag, CloseTag, Input, Match) ->
  {ok, RE} = re:compile([Match]),
  CandidatesNotTested = re:split(Input, OpenTag, [{return, list}]),
  Candidates = [ find_block_helper(C, RE) || C <- CandidatesNotTested ],
  Block = case [ C || C <- Candidates, C =/= false ] of
    [B] -> B;
    [B, _] -> B;
    _ -> []
  end,
  [Inside | _ ] = re:split(Block, CloseTag, [{parts, 2}, {return, list}]),
  [Inside].

find_block_helper(Test, RE) ->
	case re:run(Test, RE) of
		{match, _} -> Test;
		_ -> false
	end.

uri(Config, Path) ->
	{url, Base} = lists:keyfind(url,1,Config),
  case {string:right(Base,1),string:left(Path,1)} of
    {"/", "/"}-> Base ++ string:substr(Path,2);
    {_, "/"}  -> Base ++ Path;
    {"/", _}  -> Base ++ Path;
    {_, _}    -> Base ++ "/" ++ Path
  end.
  
% get a page from a server
get(Config, Page) ->
	get(Config, Page, ok).
get(Config, Page, not_found) ->
	get(uri(Config,Page), 404, "Not Found");
get(Config, Page, ok) ->
	get(Config, uri(Config,Page), 200, "OK(.*)").
get(Config, URL, ReturnCode, StateRegEx) ->
	{ok, {{"HTTP/1.1",ReturnCode,State}, _Head, Body}} = digest_auth:request(Config, URL),
	{ok, StateMP} = re:compile(StateRegEx),
	%bdd_utils:debug(true, "hppt_get has: URL ~p = ~s~n", [URL, Body]),
	case re:run(State, StateMP) of
		{match, _} -> Body;
		_ -> "ERROR, return of " ++ URL ++ " result was not 200 OK"
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
post(Config, URL, Parameters, ReturnCode, StateRegEx) ->
  Post = URL ++ post_params(Parameters),
  {ok, {{"HTTP/1.1",ReturnCode, State}, _Head, Body}} = digest_auth:request(Config, post, {Post, "application/json", "application/json", "body"}, [{timeout, 10000}], []),  
 	{ok, StateMP} = re:compile(StateRegEx),
	case re:run(State, StateMP) of
		{match, _} -> Body;
		_ -> "ERROR, return of " ++ URL ++ " result was not 200 OK"
	end. 

% Post using JSON to convey the values
post(Config, Path, JSON) ->
  URL = uri(Config, Path),
  R = digest_auth:request(Config, post, {URL, [], "application/json", JSON}, [{timeout, 10000}], []),  
  {ok, {{"HTTP/1.1",_ReturnCode, State}, _Head, Body}} = R,
	{ok, StateMP} = re:compile("OK"),
	case re:run(State, StateMP) of
		{match, _} -> json:parse(Body);
		_ -> "ERROR, return of " ++ URL ++ " result was not 200 OK. " ++ Body
	end. 

delete(Config, Path, Id) ->
  URL = uri(Config, Path) ++ "/" ++ Id,
  R = digest_auth:request(Config, delete, {URL}, [{timeout, 10000}], []),  
  {ok, {{"HTTP/1.1",_ReturnCode, State}, _Head, Body}} = R,
	{ok, StateMP} = re:compile("OK"),
	case re:run(State, StateMP) of
		{match, _} -> true;
		_ -> "ERROR, return of " ++ URL ++ " result was not 200 OK. " ++ Body
	end. 
	