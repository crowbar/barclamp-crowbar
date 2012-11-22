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
-module(bdd_restrat).
-export([step/3]).
-export([get_id/2, get_id/3, create/3, create/4, create/5, create/6, destroy/3]).


% ASSUME, only 1 ajax result per feature
get_JSON(Results) ->
  {ajax, JSON, _} = lists:keyfind(ajax, 1, Results),  
  JSON.   

% HELPERS ============================

% given a path + key, returns the ID of the object
get_id(Config, Path, Key) -> get_id(Config, eurl:path(Path,Key)).
get_id(Config, Path) ->
  R = eurl:get_page(Config, Path, all),
  bdd_utils:log(Config, trace, "bdd_restrat:get_id R: ~p~n", [R]),
  {"id", ID} = case R of
    {200, Result}  -> lists:keyfind("id", 1, json:parse(Result));
    _              -> {"id", "-1"}
  end,  
  ID.
  
% helper common to all setups using REST
create(Config, Path, JSON)         -> create(Config, Path, JSON, post).
create(Config, Path, JSON, Action) ->
  % just in case - cleanup to prevent collision
  destroy(Config, Path, json:keyfind(json:parse(JSON), name)),
  % create node(s) for tests
  eurl:put_post(Config, Path, JSON, Action).
  
create(Config, Path, Atom, Name, JSON) ->
  create(Config, Path, Atom, Name, JSON, post).

create(Config, Path, Atom, Name, JSON, Action) ->
  bdd_utils:log(Config, trace, "Entering bdd_restrat:create Path: ~p, Name: ~p, JSON: ~p~n", [Path, Name, JSON]),
  Result = create(Config, Path, JSON, Action),
  % get the ID of the created object
  Key = json:keyfind(Result, id),
  % friendly message
  bdd_utils:log(Config, debug, "Created ~s (key=~s & id=~s) for testing.~n", [Name, Atom, Key]),
  % add the new ID to the config list
  [{Atom, Key} | Config].

% helper common to all setups using REST
destroy(Config, Path, Atom) when is_atom(Atom) ->
  Item = lists:keyfind(Atom, 1, Config),
  {Atom, Key} = Item,
  destroy(Config, Path, Key),
  lists:delete(Item, Config);

% helper common to all setups using REST
destroy(Config, Path, Key) ->
  case get_id(Config, Path, Key) of
    "-1" -> bdd_utils:log(Config, trace, "\tRemoval of key ~s skipped: not found.", [Key]);
    ID   -> eurl:delete(Config, Path, ID),
            bdd_utils:log(Config, debug, "\tRemoved key ~s & id ~s.", [Key, ID])
  end,
  Config.
  
% STEPS ======================
step(Config, _Given, {step_when, _N, ["REST requests the",Page,"page"]}) ->
  JSON = eurl:get(Config, Page),
  {ajax, json:parse(JSON), {get, Page}};

step(Config, _Given, {step_when, _N, ["REST creates the",Object,Name]}) -> 
  JSON = apply(Object, json, [Name, apply(Object, g, [description]), apply(Object, g, [order])]),
  Path = apply(Object, g, [path]),
  {Code, Result} = eurl:put_post(Config, Path, JSON, post, all),
  Key = json:keyfind(Result, id),
  bdd_utils:log(Config, debug, "bdd_restrat:create: ~p, Name: ~p, ID: ~p", [Path, Name, Key]),
  case Code of
    200 -> {ajax, Result, {post, Path}};
    _   -> {ajax, Code, {post, Path}}
  end;

step(Config, _Given, {step_when, _N, ["REST updates the",Object,Name]}) -> 
  JSON = apply(Object, json, [Name, apply(Object, g, [description]), apply(Object, g, [order])]),
  Path = eurl:path(apply(Object, g, [path]), Name),
  {Code, Result} = eurl:put_post(Config, Path, JSON, put, all),
  bdd_utils:log(Config, debug, "bdd_restrat:update: ~p, Name: ~p", [Path, Name]),
  case Code of
    200 -> {ajax, Result, {put, Path}};
    _   -> {ajax, Code, {put, Path}}
  end;


step(Config, _Given, {step_when, _N, ["REST deletes the",Object, Name]}) -> 
  Path = apply(Object, g, [path]),
  R = eurl:delete(Config, Path, Name, all),
  bdd_utils:log(Config, debug, "bdd_restrat step delete ~p ~p result ~p",[Object,Name, R]),
  {Code, _} = R,
  {ajax, Code, {delete, Path}};
  
step(Config, _Given, {step_when, _N, ["REST gets the",Object,"list"]}) -> 
  % This relies on the pattern objects providing a g(path) value mapping to their root information
  URI = apply(Object, g, [path]),
  bdd_utils:log(Config, trace, "REST get ~p path", [URI]),
  case eurl:get_page(Config, URI, all) of
    {200, JSON} -> {ajax, json:parse(JSON), {get, URI}};
    {Code, _}   -> {ajax, Code, {get, URI}}
  end;

step(Config, _Given, {step_when, _N, ["REST gets the",Object,Key]}) ->
  % This relies on the pattern objects providing a g(path) value mapping to their root information
  URI = eurl:path(apply(Object, g, [path]), Key),
  case eurl:get_page(Config, URI, all) of
    {200, "null"} -> [];
    {200, JSON}   -> 
        bdd_utils:log(Config, trace, "bdd_restrat:get object ~p key ~p uri ~p result ~p...",[Object, Key, URI, string:left(JSON,50)]), 
        {ajax, json:parse(JSON), {get, URI}};
    {Num, _}      -> {ajax, Num, {get, URI}}
  end;

step(Config, Results, {step_then, _N, ["the", Object, "is properly formatted"]}) ->
  {ajax, JSON, URI} = lists:keyfind(ajax, 1, Results),     % ASSUME, only 1 ajax result per feature
  % This relies on the pattern objects providing a g(path) value mapping to their root information
  case JSON of 
    not_found -> bdd_utils:log(Config, warn, "bdd_restrat: Object ~p not found at ~p", [Object, URI]), false;
    error -> bdd_utils:log(Config, error, "bdd_restrat: Object ~p threw error at ~p", [Object, URI]), false;
    J -> apply(Object, validate, [J])
  end;
    
step(Config, Results, {step_then, _N, ["there should be a key",Key]}) -> 
  {ajax, JSON, _} = lists:keyfind(ajax, 1, Results),     % ASSUME, only 1 ajax result per feature
  bdd_utils:log(Config, trace, "JSON list ~p should have ~p~n", [JSON, Key]),
  length([K || {K, _} <- JSON, K == Key])==1;
                                                                
step(_Config, Results, {step_then,_N, ["key",Key,"should be",Value]}) ->
  Value =:= json:value(get_JSON(Results), Key);

step(_Config, Results, {step_then, _N, ["key",Key, "should contain",Count,"items"]}) -> 
  {C, _} = string:to_integer(Count),
  List = json:value(get_JSON(Results), Key),
  Items = length(List),
  Items =:= C;
                                                                
step(_Config, Results, {step_then, _N, ["key",Key,"should contain at least",Count,"items"]}) ->
  {C, _} = string:to_integer(Count),
  List = json:value(get_JSON(Results), Key),
  Items = length(List),
  Items >= C;

step(_Config, Results, {step_then, _N, ["key",Key,"should be a number"]}) -> 
  bdd_utils:is_a(number, json:value(get_JSON(Results), Key));
                                                       
step(_Config, Results, {step_then, _N, ["key",Key, "should be an empty string"]}) -> 
  bdd_utils:is_a(empty, json:value(get_JSON(Results), Key));
                                                      
step(_Config, Result, {step_then, _N, ["there should be a value",Value]}) -> 
  Test = lists:keyfind(Value,2,get_JSON(Result)),
  Test =/= false;

step(_Config, Result, {step_then, _N, ["I get a",Number,"result"]}) -> 
  step(_Config, Result, {step_then, _N, ["I get a",Number,"error"]});
            
step(Config, Results, {step_then, _N, ["I get a",Number,"error"]}) -> 
  Result = lists:keyfind(ajax, 1, Results),
  bdd_utils:log(Config, trace, "bdd_restrat step then ~p error result ~p",[Number, Result]),
  case Result of 
    {ajax, Number, _}  -> true;
    _                  -> false
  end;

step(_Config, _Result, {_Type, _N, ["END OF RESTRAT"]}) ->
  false.
