% Copyright 2013, Dell 
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
-export([assert/1, assert/2, assert_atoms/1, tokenize/2, tokenize/6, clean_line/1]).
-export([config/1, config/2, config/3, config_set/2, config_set/3, config_unset/1, config_unset/2]).
-export([scenario_store/3, scenario_retrieve/3]).
-export([puts/0, puts/1, puts/2, debug/3, debug/2, debug/1, trace/6, untrace/3]).
-export([log/4, log/3, log/2, log/1, log_level/1]).
-export([features/1, features/2, feature_name/2]).
-export([setup_create/5, setup_create/6, teardown_destroy/3]).
-export([is_site_up/1, is_a/2, is_a/3]).
-define(NORMAL_TOKEN, 1).
-define(ESCAPED_TOKEN, 2).
-define(SUBSTITUTE_TOKEN, 3).
-define(LOG_LEVELS, [true, puts, dump, trace, debug, info, warn, error]).
-define(LOG_DEFAULT, [true, puts, info, warn, error]).
-define(LOG_TITLES, [pass, fail, skip, header, result, feature, step, step_pass, step_fail]).

assert(Bools) ->
	assert(Bools, true).
assert(Bools, debug) ->
  case assert(Bools, true) of
    true -> true;
    X -> puts("Testing result ~p", [Bools]), X
  end;
assert(Bools, Test) ->
	F = fun(X) -> case X of Test -> true; _ -> false end end,
	lists:all(F, Bools).
assert_atoms(Atoms) ->
  assert([B || {B, _} <- Atoms] ).
check(Bools) ->
  F = fun(X) -> case X of true -> true; _ -> false end end,
  lists:any(F, Bools).

% for quick debug that you want to remove later (like ruby puts)
puts()              -> log(puts, "*** HERE! ***").  
puts(Format)        -> log(puts, Format).  
puts(Format, Data)  -> log(puts, Format, Data).

% DEPRICATED! for debug statements that you want to leave in
debug(Format)             -> log(debug, Format, []).
debug(Format, Data)       -> log(debug, Format, Data).
debug(Show, Format, Data) -> log(Show, Format, Data, "DEBUG").

% FOR PERFORMANCE, always call with Config if available!
log(Format)                       -> log(info, Format, []).
log(Format, Data)                 -> log(info, Format, Data).
log(Config, puts, Format)         -> log(Config, puts, Format, []);
log(Config, Level, Format) when is_atom(Level) -> log(Config, Level, Format, []);
log(Level, Format, Data)          -> log([], Level, Format, Data).
log(Config, Level, Format, Data)  ->
  Logs = config(Config, log, ?LOG_DEFAULT),
  Titles = config(Config, titles, ?LOG_TITLES),
  Levels = Logs ++ Titles,
  Show = lists:member(Level, Levels), 
  case {Show, Level} of
    % Log methods for test results
    {true, header}    -> io:format("~nBDD TEST: " ++ Format, Data);
    {true, feature}   -> io:format("~n~nFEATURE: " ++ Format, Data);
    {true, result}    -> io:format("~n  RESULT: " ++ Format, Data);
    {true, pass}      -> io:format("~n  Passed: " ++ Format, Data);
    {true, fail}      -> io:format("~n  FAILED: " ++ Format, Data);
    {true, skip}      -> io:format("~n  ..skip: " ++ Format, Data);
    {true, step}      -> io:format("~n    Step: " ++ Format, Data);
    {true, step_pass} -> io:format("~n    Step Pass: " ++ Format, Data);
    {true, step_fail} -> io:format("~n    Step FAIL: " ++ Format, Data);
    % General Logging Ouptut
    {true, _}     -> Prefix = "   " ++ string:to_upper(atom_to_list(Level)),
                     {Module, Method, Params} = try erlang:get_stacktrace() of 
                        [{erl_parse, yecctoken_end_location, 1} | _] -> {no, trace, 0}; 
                        [ST | _] -> ST; 
                        [] -> {unknown, 0, 0} 
                      catch _ -> [{module, unknown, 0}] end,
                      Arity = case Params of [] -> 0; X when is_number(X) -> X; X -> length(X) end,
                      case Arity of
                        0 ->  io:format("~n" ++ Prefix ++ ": " ++ Format, Data);
                        A when is_number(A) -> 
                              io:format("~n" ++ Prefix ++ ": " ++ Format ++ " <~p:~p/~p>", Data ++ [Module, Method, Arity]);
                        A ->  io:format("~n" ++ Prefix ++ ": " ++ Format ++ " <unexpected ~p>", Data++[A])
                      end;
    _ -> no_log
  end.

log_level(dump)       -> put(log, [dump, trace, debug, info, warn, error, puts]);
log_level(trace)      -> put(log, [trace, debug, info, warn, error, puts]);
log_level(debug)      -> put(log, [debug, info, warn, error, puts]);
log_level(info)       -> put(log, [info, warn, error, puts]);
log_level(depricate)  -> put(log, [depricate, info, warn, error, puts]);
log_level(warn)       -> put(log, [warn, error, puts]);
log_level(all)        -> put(log, all).

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
is_a(JSON, Type, Key) ->
  Value = json:keyfind(JSON, Key), 
  case is_a(Type, Value) of
    true  -> true;
    X     -> 
      log(warn, "Key ~p with Value ~p did not pass is_a(~p) test.  Result was ~p.", [Key, Value, Type, X]),
      false
  end.
  
is_a(Type, Value) ->
  case Type of 
    number  -> nomatch =/= re:run(Value, "^[\-0-9\.]*$");    % really STRING TEST
    num     -> is_number(Value);
    integer -> nomatch =/= re:run(Value, "^[\-0-9]*$");     % really STRING TEST
    int when is_list(Value) -> is_a(Type, list_to_integer(Value));
    int     -> is_integer(Value);
    whole   -> nomatch =/= re:run(Value, "^[0-9]*$");
    dbid    -> lists:member(true, [nomatch =/= re:run(Value, "^[0-9]*$"), "null" =:= Value]);
    name    -> nomatch =/= re:run(Value, "^[A-Za-z][\-_A-Za-z0-9.]*$");
    boolean -> lists:member(Value,[true,false,"true","false"]);
    str     -> case Value of V when is_list(V) -> check([is_list(V), length(V)=:=0]); _ -> false end; 
    string  -> is_list(Value);                              % cannot be empty
    cidr    -> nomatch =/= re:run(Value, "^([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\.([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\.([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\.([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])(\/([0-9]|1[0-9]|2[0-9]|3[0-2]))?$");
    ip      -> nomatch =/= re:run(Value, "^([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\.([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\.([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\.([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])$");
    empty   -> "" =:= Value;
    RE when is_list(RE) -> 
      log(trace, "bdd_utils:is_a falling back to RE match for ~p ~p", [Type, Value]),
      nomatch =/= re:run(Value, RE);    % fall through lets you pass in a regex (pretty cool!)
    _       -> 
      log(warn, "bdd_utils:is_a no matching type for ~p found.  Value was ~p", [Type, Value]),
      false
  end.
	
% Web Site Cake Not Found - GLaDOS cannot test
is_site_up(Config) ->
  URL = bdd_utils:config(Config, url),
  log(Config, header, "Site ~p", [URL]),
  AzConfig = simple_auth:authenticate_session(Config, URL),
  case config(AzConfig, auth_error, undefined) of
    undefined -> AzConfig; % success
    Reason -> 
      log(Config, error, "bdd_utils: Web site '~p' is not responding! Remediation: Check server.  Message: ~p", [URL, Reason]),
      Config
  end.

% config using BIFs
config(Key) -> config(Key, undefined).
config(Key, Default) when is_atom(Key) -> 
  case get(Key) of
    undefined -> put(Key, Default), Default;
    V         -> V
  end;
  
% DEPRICATING returns value for key from Config (error if not found)
config(Config, Key) ->
  case config(Config, Key, undefined) of
    undefined -> throw("bdd_utils:config Could not find requested key '"++atom_to_list(Key)++"' in config file");
    V -> V
  end.

% returns value for key from Config (returns default if missing)
config(Config, Key, Default) ->
  % TODO - this should use the get first, but we're transistioning so NOT YET
  case lists:keyfind(Key,1,Config) of
    {Key, undefined} -> config(Key, Default);
    {Key, Value} -> 
          % this if helps find items that are not using config_set
          case get(Key) of
            undefined -> 
                  put(Key, Default), 
                  log(Config, depricate, "Depricating Config! Please use bdd_utils:config_set(Config, ~p, ~p) for Key ~p",[Key, Default, Key]);
            _ -> all_good
          end,
          Value;
    _ ->  case get(Key) of   	     
  	        undefined -> put(Key, Default), Default;
  	        V -> V
  	      end
	end.

config_set(Key, Value) ->
  put(Key, Value),
  {Key, Value}.
config_set([], Key, Value)      -> config_set(Key, Value);
config_set(Config, Key, Value)  ->
  C = case lists:keyfind(Key,1,Config) of
    undefined -> Config;
    Item -> lists:delete(Item, Config)
  end,
  C ++ [config_set([], Key, Value)].
  
config_unset(Key)     -> put(Key, undefined).
config_unset([], Key) -> config_unset(Key); 
config_unset(Config, Key) ->
  config_unset([], Key),
  case lists:keyfind(Key,1,Config) of
    false -> Config;
    Item  -> lists:delete(Item, Config)
  end.

% stores values used inside a scenario
scenario_store(ID, Key, Value) ->
  Scenario = get({scenario, ID}),
  Store = case Scenario of
    undefined -> [{Key, Value}];
    List      ->  % remove existing if any
                  L = case lists:keyfind(Key, 1, List) of
                    false -> List;
                    _     -> lists:keydelete(Key, 1, List)
                  end,
                  L ++ [{Key, Value}]
  end,
  put({scenario, ID}, Store),
  log(trace, "bdd_utils:scenario_store for ~p storing ~p as ~p", [ID, Key, Value]),
  Store.

% retieves values used inside a scenario
scenario_retrieve(ID, Key, Default) ->
  Scenario = get({scenario, ID}),
  Return = case Scenario of 
    undefined -> Default;
    List      -> case lists:keyfind(Key, 1, List) of
                    false     -> Default;
                    {Key, R}  -> R
                 end
  end,
  log(trace, "bdd_utils:scenario_retrieve for ~p retrieving ~p as ~p", [ID, Key, Return]),
  Return.
  
% removes whitespace 
clean_line(Raw) ->
	CleanLine0 = string:strip(Raw),
	CleanLine1 = string:strip(CleanLine0, left, $\t),
	CleanLine11 = string:strip(CleanLine1, right, $\r),
	CleanLine2 = string:strip(CleanLine11),
	string:strip(CleanLine2, right, $.).

% converts quoted text into a list
tokenize(Config, Step) -> tokenize(Config, Step, false, ?NORMAL_TOKEN, [], "").

tokenize(Config, [], _IgnoreNext, TokenType, TokenList, Token ) ->
  FinalTokenList = if
    Token /= [] ->
      FinalToken = if
        TokenType == ?SUBSTITUTE_TOKEN -> token_substitute(Config, string:strip(Token));
        true -> string:strip(Token)
      end,
      [FinalToken|TokenList];
    true -> TokenList
  end,
  lists:reverse(FinalTokenList);

tokenize(Config, Step, IgnoreNext, TokenType, TokenList, Token ) ->
  Char = string:substr(Step,1,1),
  if
    % If this character is escaped, then just add it, even if it is a double quote
    IgnoreNext -> tokenize(Config, string:substr(Step,2), false, TokenType, TokenList, Token ++ Char);
    % The next character is escaped, so don't add the escape
    Char == "\\" -> tokenize(Config, string:substr(Step,2), true, TokenType, TokenList, Token);
    % Handle the first character being a double quote
    Char == "\"", Token == "" -> tokenize(Config, string:substr(Step,2), false, ?ESCAPED_TOKEN, TokenList, "");
    % Start or end of quotes terminates the last token, whatever it was
    Char == "\"", Token /= "" ->
      NewTokenType = case TokenType of
        ?NORMAL_TOKEN -> ?ESCAPED_TOKEN;
        ?ESCAPED_TOKEN -> ?NORMAL_TOKEN;
        ?SUBSTITUTE_TOKEN -> ?SUBSTITUTE_TOKEN;
        _ -> ?NORMAL_TOKEN
      end,
      tokenize(Config, string:substr(Step,2), false, NewTokenType, add_token(Token, TokenList), "");
    Char == "{", TokenType /= ?ESCAPED_TOKEN, Token == "" ->
      tokenize(Config, string:substr(Step,2), false, ?SUBSTITUTE_TOKEN, TokenList, "");
    Char == "{", TokenType /= ?ESCAPED_TOKEN, Token /= "" ->
      tokenize(Config, string:substr(Step,2), false, ?SUBSTITUTE_TOKEN, add_token(Token, TokenList), "");
    Char == "}", TokenType == ?SUBSTITUTE_TOKEN ->
      SubToken = token_substitute(Config, string:strip(Token)),
      tokenize(Config, string:substr(Step,2), false, ?NORMAL_TOKEN, [SubToken|TokenList], "");
    % Default action is to add to the current token
    true -> tokenize(Config, string:substr(Step,2), false, TokenType, TokenList, Token ++ Char)
  end.

add_token(Token, TokenList) ->
  NewToken = string:strip(Token),
  case NewToken of
    "" -> TokenList;
    _ -> [NewToken|TokenList]
  end.

% This routine is used for special subtitutions in steps that run functions or turn strings into atoms
token_substitute(_Config, [$a, $p, $p, $l, $y, $: | Apply]) -> [File, Method | Params] = string:tokens(Apply, "."),
                                                              apply(list_to_atom(File), list_to_atom(Method), Params);
token_substitute(Config,  [$b, $d, $d, $: | Apply])          -> [File, Method | Params] = string:tokens(Apply, "."),
                                                              apply(list_to_atom(File), list_to_atom(Method), [Config | Params]);
token_substitute(_Config, [$a, $t, $o, $m, $: | Apply])     -> list_to_atom(Apply);
token_substitute(_Config, [$f, $i, $e, $l, $d, $s, $: | Apply]) 
                                                            -> Pairs = string:tokens(Apply, "&"),
                                                               Params = [ string:tokens(KV,"=") || KV <- Pairs],
                                                               [ {K, V} || [K, V | _] <- Params];
token_substitute(_Config, [$o, $b, $j, $e, $c, $t, $: | Apply])  
                                                           -> list_to_atom(Apply);
token_substitute(_Config, [$i, $n, $t, $e, $g, $e, $r, $: | Apply])
                                                           -> {Num, []} = string:to_integer(Apply),
                                                              Num;
token_substitute(_Config, Token)                           -> Token.


% MOVED! DELETE AFTER 12/12/12 helper common to all setups using REST
setup_create(Config, Path, Atom, Name, JSON) ->
  log(Config, depricate, "** PLEASE MOVE ** setup_create moved from bdd_utils to create:crowbar_rest.  Called with ~p, ~p, ~p.",[Path, Atom, Name]),
  crowbar_rest:create(Config, Path, Atom, Name, JSON).

% MOVED! DELETE AFTER 12/12/12 helper common to all setups using REST
setup_create(Config, Path, Atom, Name, JSON, Action) ->
  log(Config, depricate, "** PLEASE MOVE ** setup_create moved from bdd_utils to create:crowbar_rest.  Called with ~p, ~p, ~p, ~p.",[Path, Atom, Name, Action]),
  crowbar_rest:create(Config, Path, Atom, Name, JSON, Action).
  
% MOVED! DELETE AFTER 12/12/12 helper common to all setups using REST
teardown_destroy(Config, Path, Atom) ->
  log(Config, depricate, "** PLEASE MOVE ** setup_destroy moved from bdd_utils to destroy:crowbar_rest.  Called with ~p, ~p.",[Path, Atom]),
  crowbar_rest:destroy(Config, Path, Atom).
