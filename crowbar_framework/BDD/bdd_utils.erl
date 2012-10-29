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
% 
-module(bdd_utils).
-export([assert/1, assert/2, assert_atoms/1, config/2, config/3, tokenize/1, clean_line/1]).
-export([puts/1, puts/2, debug/3, debug/2, debug/1, trace/6, untrace/3]).
-export([setup_create/5, teardown_destroy/3]).  % DEPRICATED by 12/12/12
-export([features/1, features/2, feature_name/2]).
-export([is_site_up/1, is_a/2]).

assert(Bools) ->
	assert(Bools, true).
assert(Bools, Test) ->
	F = fun(X) -> case X of Test -> true; _ -> false end end,
	lists:all(F, Bools).
assert_atoms(Atoms) ->
  assert([B || {B, _} <- Atoms] ).

% for quick debug that you want to remove later (like ruby puts)
puts(Format) -> debug(true, Format).  
puts(Format, Data) -> debug(true, Format, Data).

% for debug statements that you want to leave in
debug(Format) -> debug(Format, []).
debug(puts, Format) -> debug(true, Format++"~n", []);
debug(true, Format) -> debug(true, Format, []);
debug(false, Format) -> debug(false, Format, []);
debug(Format, Data) -> debug(false, Format, Data).
debug(Show, Format, Data) ->
  case Show of
    true -> io:format("DEBUG: " ++ Format, Data);
    _ -> noop
  end.

% return the list of feature to test
features(Config) ->
  filelib:wildcard(features(Config, "*")).

% return the path to a feature to test
features(Config, Feature) ->
  config(Config,feature_path,"features/") ++ Feature ++ "." ++ config(Config,extension,"feature").
  
% helper that finds the feature from the FileName
feature_name(Config, FileName) ->
  RegEx = bdd_utils:config(Config,feature_path,"features/") ++ "(.*)." ++ bdd_utils:config(Config,extension,"feature"),
	{ok, RE} = re:compile(RegEx, [caseless]),
	case re:run(FileName, RE) of 
	  {match,[{0,_},{Start,Length}]} -> string:sub_string(FileName, Start+1, Start+Length);
	  _ -> FileName
	end.

% Return the file name for the test.  
trace_setup(Config, Name, nil) ->
  trace_setup(Config, Name, 0);

trace_setup(Config, Name, N) ->
  SafeName = clean_line(Name),
  string:join(["trace_", config(Config,feature), "-", string:join(string:tokens(SafeName, " "), "_"), "-", integer_to_list(N), ".txt"], "").
  
trace(Config, Name, N, Steps, Given, When) ->
  File = trace_setup(Config, Name, N),
  {ok, S} = file:open(File, write),
  lists:foreach(fun(X) -> io:format(S, "~n==== Step ====~n~p", [X]) end, Steps),
  lists:foreach(fun(X) -> io:format(S, "~n==== Given ====~n~p", [X]) end, Given),
  [io:format(S, "~n==== When ====~n~p",[X]) || X <- (When), X =/= []],
  io:format(S, "~n==== End of Test Dump (~p) ====", [N]),
  file:close(S).

untrace(Config, Name, N) ->
  File = trace_setup(Config, Name, N),
  file:delete(File).
  
% test for types
is_a(Type, Value) ->
  case Type of 
    number -> nomatch =/= re:run(Value, "^[\-0-9\.]*$");
    integer -> nomatch =/= re:run(Value, "^[\-0-9]*$");
    whole -> nomatch =/= re:run(Value, "^[0-9]*$");
    name -> nomatch =/= re:run(Value, "^[A-Za-z][\-_A-Za-z0-9.]*$");
    boolean -> lists:member(Value,[true,false,"true","false"]);
    string -> is_list(Value);
    empty -> "" =:= Value;
    _ -> false
  end.
	
% Web Site Cake Not Found - GLaDOS cannot test
is_site_up(Config) ->
  URL = sc:url(Config),
  io:format("~nBDD TESTING SITE: ~p.~n", [URL]),
  AzConfig = simple_auth:header(Config, URL),
  case proplists:get_value(auth_error,AzConfig) of
    undefined -> AzConfig; % success
    Reason -> 
      io:format("ERROR! Web site '~p' is not responding! Remediation: Check server.  Message: ~p~n", [URL, Reason]),
      Config
  end.

% returns value for key from Config (error if not found)
config(Config, Key) ->
	case lists:keyfind(Key,1,Config) of
	  {Key, Value} -> Value;
	  false -> throw("Could not find requested key in config file");
	  _ -> throw("Unexpected return from keyfind")
	end.

% returns value for key from Config (returns default if missing)
config(Config, Key, Default) ->
	case lists:keyfind(Key,1,Config) of
	  {Key, Value} -> Value;
	  _ -> Default
	end.
	
clean_line(Raw) ->
	CleanLine0 = string:strip(Raw),
	CleanLine1 = string:strip(CleanLine0, left, $\t),
	CleanLine11 = string:strip(CleanLine1, right, $\r),
	CleanLine2 = string:strip(CleanLine11),
	string:strip(CleanLine2, right, $.).

tokenize(Step) ->
	Tokens = string:tokens(Step,"\""),
	[string:strip(X) || X<- Tokens].
	

% MOVED! DELETE AFTER 12/12/12 helper common to all setups using REST
setup_create(Config, Path, Atom, Name, JSON) ->
  io:format("** PLEASE MOVE ** setup_create moved from bdd_utils to create:crowbar_rest.  Called with ~p, ~p, ~p.",[Path, Atom, Name]),
  crowbar_rest:create(Config, Path, Atom, Name, JSON).
  
% MOVED! DELETE AFTER 12/12/12 helper common to all setups using REST
teardown_destroy(Config, Path, Atom) ->
  io:format("** PLEASE MOVE ** setup_destroy moved from bdd_utils to destroy:crowbar_rest.  Called with ~p, ~p.",[Path, Atom]),
  crowbar_rest:destroy(Config, Path, Atom).
