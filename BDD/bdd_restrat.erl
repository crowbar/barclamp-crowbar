% Copyright 2013, Dell 
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
-export([get_id/2, get_id/3, create/3, create/4, create/5, create/6, destroy/3, update/5, validate/1]).
-export([get_JSON/1, get_REST/1, get_Object/1, get_Object/2, api_wrapper/3, alias/1, ajax_return/4]).
-include("bdd.hrl").

% HELPERS ============================

% ASSUME, only 1 ajax result per feature
get_JSON(Results)      ->      
  {ajax, JSON, _} = get_JSON(Results, all),  
  JSON.   
get_JSON(Results, all) -> 
  try lists:keyfind(ajax, 1, Results) of
    {ajax, 200, A}-> bdd_utils:log(debug, bdd_restrat, get_JSON, "This may be OK, found empty list instead of data in JSON in ajax result ~p",[A]),  
                     {ajax, 200, A};
    {ajax, J, Ex} -> {ajax, J, Ex};
    false         -> get_REST(Results)
  catch
    X -> bdd_utils:log(warn, "bdd_restrat:get_JSON error ~p in result ~p",[X,Results]),
         {ajax, 500, "bdd_restrat:get_JSON error"}
  end.

% This is new - should replace ajax tuple with the rest record
get_REST(Results) ->
  try lists:keyfind(rest, 1, Results) of
     % for now, just remap rest into ajax
     {rest, Data, Code, Url, _Datatype, _Version}
                  -> {ajax, Data, {Code, Url}};
     false        -> bdd_utils:log(debug, "bdd_restrat:get_REST/JSON did not find expected to ajax result",[]),
                     bdd_utils:log(trace, "more.... in ~p",[Results]),
                     #rest{data="bdd_restrat ajax not found"}
  catch
    X -> bdd_utils:log(warn, bdd_restrat, get_REST, "error ~p in result ~p",[X,Results]),
    #rest{data="bdd_restrat:get_REST error"}
  end.
  
% handles cases where objects use names that conflict w/ internal namespaces
alias(Object) ->  bdd_utils:config(alias_map,{Object, Object}).
  
% Determines the verseion of the API and calls the right REST wrappers
get_Object(Results, URL) ->
  case get(URL) of
    undefined ->  get_Object(Results);
    % work in progress
    _         ->  get_Object(Results) %, bdd_utils:config(api_map,{Meta#meta_api.datatype, bdd_restrat})        
  end.
%get_Object(Results, {meta_api, DataType, Version}) ->
%  API_wrapper = crowbar_rest, %work in progress, bdd_utils:config(api_map, DataType, bdd_restrat})
%  apply(API_wrapper, api_wrapper, [Results, DataType, Version]);
%get_Object(Results, _) ->
%  Results.
  
api_wrapper(Results, _Datatype, _Version) ->
  Results.
  
% old way - depricate!
get_Object(Results) ->
  Obj = crowbar_rest:api_wrapper_raw(Results),
  bdd_utils:log(debug, bdd_restrat, get_Object, "DEPRICATED API wrapper type ~p for url ~p",[Obj#item.type, Obj#item.link]),
  Obj.

% this should NOT be called here, only in the objects
validate(_) -> bdd_utils:log(warn, "bdd_restrat:validate should not be called. Use your platform specific validator").

% given a path + key, returns the ID of the object
get_id(Config, Path, Key) -> get_id(Config, eurl:path(Path,Key)).
get_id(Config, Path) ->
  R = eurl:get_page(Config, Path, all),
  bdd_utils:log(trace, bdd_restrat, get_id, "path ~p Result: ~p", [Path, R]),
  {"id", ID} = try R of
    {200, []}      -> {"id", "-1"};
    {200, "null"}  -> {"id", "-1"};
    {200, Result}  -> Obj = get_Object(Result, Path), 
                      lists:keyfind("id", 1, Obj#item.data);
    _              -> {"id", "-1"}
  catch 
    _ -> {"id", "-1"}
  end,  
  ID.
  
% standard pattern for returning ajax from a when
ajax_return(Path, Method, Code, Result) ->
  case {Method, Code, Result} of
   {delete, 200, _} -> {ajax, 200, {Method, Path}};
   {_, 200, "null"} -> [];
   {_, 200, []}     -> {ajax, 200, {Method, Path}};
   {_, 200, R}      -> {ajax, json:parse(R), {Method, Path}};
    _               -> {ajax, Code, {Method, Path}}
  end.
  

% helper common to all setups using REST 


%_Config, g(path),username, User
create(Config, Path, JSON)               -> create(Config, Path, name, JSON, post).
create(Config, Path, KeyName, JSON)         -> create(Config, Path, KeyName, JSON, post).
create(Config, Path, KeyName, JSON, Action) when is_atom(Action) ->
  % just in case - cleanup to prevent collision
  Key = json:keyfind(JSON, KeyName),
  bdd_utils:log(debug,"bdd_restrat create object key ~p=~p on Path ~p using ID from ~p",[KeyName,Key,Path,Action]),
  destroy(Config, Path, Key),
  % create node(s) for tests
  eurl:put_post(Config, Path, JSON, Action);
create(Scenario, Type, Path, Name, JSON) when is_number(Scenario), is_atom(Type) ->
  bdd_utils:log(debug,"bdd_restrat create for Scenario ~p of ~p ~p with path ~p",[Scenario,Type,Name,Path]),
  destroy([], Path, Name),
  Result = eurl:put_post([], Path, JSON, post),
  R = json:parse(Result),
  Key = json:keyfind(R, id),
  bdd_utils:scenario_store(Scenario, Type, Key),
  Key;

create(Config, Path, Atom, KeyName, JSON) ->
  create(Config, Path, Atom, KeyName, JSON, post).

create(Config, Path, Atom, KeyName, JSON, Action) ->
  bdd_utils:log(Config, trace, "bdd_restrat:create Path: ~p, Atom: ~p, KeyName: ~p, Action: ~p", [Path, Atom, KeyName, Action]),
  Result = json:parse(create(Config, Path, KeyName, JSON, Action)),
  % get the ID of the created object
  Key = json:keyfind(Result, id),
  % friendly message
  bdd_utils:log(Config, debug, "Created ~s (key=~s & id=~s) for testing.", [KeyName, Atom, Key]),
  % add the new ID to the config list
  bdd_utils:config_set(Config, Atom, Key).

update(Config, Path, Atom, KeyName, JSON) ->
  bdd_utils:log(Config, trace, "bdd_restrat:update Path: ~p, Atom: ~p, KeyName: ~p, JSON: ~p", [Path, Atom, KeyName, JSON]),
  PutResult = eurl:put_post(Config, Path, JSON, put),
  bdd_utils:log(Config, debug, "Update done, result: ~p !!!",[PutResult]),
  PutResult.

% helper common to all setups using REST
destroy(Config, Path, Atom) when is_atom(Atom) ->
  Item = bdd_utils:config(Config, Atom, not_found),
  bdd_utils:log(Config, trace, "bdd_restrat:destroy(atom) deleting ~p with id ~p using path ~p",[Atom, Item, Path]),
  case Item of
    not_found -> 
        bdd_utils:log(Config, debug, "bdd_restrat:destroy(atom) could not find ID for atom ~p (likely OK)",[Atom]);
    Key       -> 
        destroy(Config, Path, Key),
        lists:delete(Item, Config)
  end;

% helper common to all setups using REST
destroy(Config, Path, Key) ->
  bdd_utils:log(Config, trace, "Entering bdd_restrat:destroy Path: ~p, Key: ~p", [Path, Key]),
  case get_id(Config, Path, Key) of
    "-1" -> bdd_utils:log(Config, trace, "Removal of key ~s skipped: not found.", [Key]);
    ID   -> eurl:delete(Config, Path, ID),
            bdd_utils:log(Config, debug, "Removed key ~s & id ~s.", [Key, ID])
  end,
  Config.
  

% GIVEN STEPS ======================
step(Config, Global, {step_given, _N, ["there is not a",Object, Name]}) -> 
  step(Config, Global, {step_finally, _N, ["REST deletes the",Object, Name]});

step(Config, Global, {step_given, _N, ["there is a",Object, Name]}) -> 
  step(Config, Global, {step_when, _N, ["REST creates the",Object,Name]});

step(Config, _Global, {step_given, _N, ["I require a",Object, Key]}) -> 
  URI = eurl:path(apply(alias(Object), g, [path]), Key),
  case eurl:get_page(Config, URI, all) of
    {200, "null"} -> bdd_utils:log(warn, bdd_restrat, step, "Required ~p object ~p was not found.  Test will likely fail",[Object, Key]);
    {200, _}      -> bdd_utils:log(debug, bdd_restrat, step, "Required ~p object ~p found.",[Object, Key]);
    {404, _}      -> bdd_utils:log(warn, bdd_restrat, step, "Required ~p object ~p not found (404).",[Object, Key]);
    {Num, _}      -> bdd_utils:log(error, bdd_restrat, step, "Required ~p object ~p error code ~p.",[Object, Key, Num])
  end;

% WHEN STEPS ======================
step(Config, _Given, {step_when, _N, ["REST cannot find the",Page,"page"]}) ->
  bdd_utils:log(Config, debug, "REST cannot find the... START"),
  Return = eurl:get(Config, Page,not_found),
  case Return of 
    "200" -> false; 
    _Other -> true
  end;

step(Config, _Given, {step_when, _N, ["REST requests the",Page,"page"]}) ->
  JSON = eurl:get(Config, Page),
  ajax_return(Page, get, 200, JSON);

step(Config, _Global, {step_when, _N, ["REST creates a",Object,Name]}) -> 
  step(Config, _Global, {step_when, _N, ["REST creates the",Object,Name]});
step(Config, _Global, {step_given, _N, ["REST creates a",Object,Name]}) -> 
  step(Config, _Global, {step_when, _N, ["REST creates the",Object,Name]});
step(Config, _Global, {step_given, _N, ["REST creates the",Object,Name]}) -> 
  step(Config, _Global, {step_when, _N, ["REST creates the",Object,Name]});

step(Config, _Given, {step_when, {ScenarioID, _N}, ["REST creates the",Object,Name]}) -> 
  bdd_utils:log(Config, debug, "bdd_restrat: step REST creates the ~p ~p", [Object, Name]),
  JSON = apply(alias(Object), json, [Name, apply(alias(Object), g, [description]), apply(alias(Object), g, [order])]),
  Path = apply(alias(Object), g, [path]),
  PutPostResult = eurl:put_post(Config, Path, JSON, post, all),
  bdd_utils:log(Config, trace, "bdd_restrat:REST creates the step: PutPostResult: ~p", [PutPostResult]),
  {Code, Result} = PutPostResult,
  bdd_utils:log(Config, trace, "bdd_restrat:REST creates the step: Code: ~p, Result: ~p",[Code, Result]),
  ReturnResult = case Code of
    200 ->
      ReturnJSON = json:parse(Result),
      bdd_utils:log(Config, trace, "bdd_restrat:REST creates the step: ReturnJSON: ~p",[ReturnJSON]),
      Key = json:keyfind(ReturnJSON, id),
      bdd_utils:scenario_store(ScenarioID, Object, Key),
      bdd_utils:log(Config, debug, "bdd_restrat:create: ~p, Name: ~p, ID: ~p", [Path, Name, Key]),
      ajax_return(Path, post, 200, ReturnJSON);
    C   -> 
      bdd_utils:log(Config, debug, "bdd_restrat:create NOT OK on ~p, Name: ~p", [C, Path, Name]),
      ajax_return(Path, post, C, Result)
  end,
  bdd_utils:log(Config, trace, "bdd_restrat:step:ReturnResult: ~p",[ReturnResult]),
  ReturnResult;

step(Config, _Given, {step_when, _N, ["REST updates the",Object,Name]}) when is_atom(Object) -> 
  JSON = apply(alias(Object), json, [Name, apply(alias(Object), g, [description]), apply(alias(Object), g, [order])]),
  Path = eurl:path(apply(alias(Object), g, [path]), Name),
  step(Config, _Given, {step_when, _N, ["REST updates an object at",Path,"with",JSON]});

step(Config, _Given, {step_when, _N, ["REST updates an object at",Path,"with",JSON]}) ->
  bdd_utils:log(Config, trace, "REST updates an object at ~p with ~p", [Path,JSON]),
  {Code, Result} = eurl:put_post(Config, Path, JSON, put, all),
  bdd_utils:log(Config, debug, "bdd_restrat:REST updates an object at step: Code: ~p, Result: ~p",[Code, Result]),
  ajax_return(Path, put, Code, Result);

step(Config, _Then, {step_finally, _N, ["REST deletes the", Object, Name]}) -> 
  step(Config, _Then, {step_when, _N, ["REST deletes the",Object, Name]});

step(Config, _Given, {step_when, _N, ["REST deletes the",Object, Name]}) when is_atom(Object)-> 
  Path = apply(alias(Object), g, [path]),
  R = eurl:delete(Config, Path, Name, all),
  {Code, Result} = R,
  bdd_utils:log(Config, debug, "bdd_restrat step delete ~p ~p = ~p ~p",[Object,Name, Code, Result]),
  ajax_return(Path, delete, Code, Result);
  
step(Config, Given, {step_finally, _N, ["REST removes the",Object, Name]}) when is_atom(Object)-> 
  step(Config, Given, {step_when, _N, ["REST deletes the",Object, Name]});
step(Config, Given, {step_finally, _N, ["REST removes",Object, Name]}) when is_atom(Object)-> 
  step(Config, Given, {step_when, _N, ["REST deletes the",Object, Name]});

step(Config, _Given, {step_when, _N, ["REST gets the",Object,"list"]}) when is_atom(Object) -> 
  % This relies on the pattern objects providing a g(path) value mapping to their root information
  URI = apply(alias(Object), g, [path]),
  bdd_utils:log(debug, bdd_restrat, step, "REST get ~p list for ~p path", [Object, URI]),
  {Code, JSON} = eurl:get_page(Config, URI, all),
  Wrapper = crowbar_rest:api_wrapper_raw(JSON),
  bdd_utils:log(trace, bdd_restrat, step, "REST get ~p list result ~p", [Object, Wrapper]),
  ajax_return(URI, get, Code, Wrapper#list.ids);

step(Config, _Given, {step_when, _N, ["REST gets the",Object,Key]})  when is_atom(Object) ->
  % This relies on the pattern objects providing a g(path) value mapping to their root information
  URI = eurl:path(apply(alias(Object), g, [path]), Key),
  case eurl:get_page(Config, URI, all) of
    {200, "null"} -> [];
    {200, JSON}   -> 
        bdd_utils:log(Config, trace, "bdd_restrat:get object ~p key ~p uri ~p result ~p...",[Object, Key, URI, string:left(JSON,50)]), 
        {ajax, json:parse(JSON), {get, URI}};
    {Num, _}      -> {ajax, Num, {get, URI}}
  end;

step(_Config, Results, {step_then, _N, ["REST call returned success"]}) ->
  [Head|_] = Results,
  {ajax, Code,_} = Head,
  case Code of
    200 -> true;        % catches new format
    C when is_list(C) ->     % catches old format (DEPRICATE!)
           bdd_utils:log(debug,bdd_restrat,step, "note, step before REST call return success called with old ajax results format.",[]), 
           true;
    _   -> bdd_utils:log(debug,bdd_restrat, step, "REST call DID NOT return success! with ~p",[Head]), 
           false
  end;


step(Config, _Results, {step_then, _N, ["there is a", Object, Key]}) ->
  URI = apply(alias(Object), g, [path]),
  {Code, _} =eurl:get_page(Config, eurl:path(URI, Key), all),
  200 =:= Code;

step(Config, _Results, {step_then, _N, ["there is not a", Object, Key]}) ->
  URI = apply(alias(Object), g, [path]),
  {Code, _} =eurl:get_page(Config, eurl:path(URI, Key), all),
  404 =:= Code;
  
step(Config, Results, {step_then, _N, ["the", Object, "is properly formatted"]}) when is_atom(Object) ->
  % This relies on the pattern objects providing a g(path) value mapping to their root information 
  case get_JSON(Results, all) of 
    {ajax, Code, {_, URI}} when is_number(Code) -> 
        bdd_utils:log(Config, warn, "bdd_restrat: Object ~p code ~p at ~p", [Object, Code, URI]), 
        false;
    {ajax, J, _}          -> apply(alias(Object), validate, [J])
  end;
    
step(Config, Results, {step_then, _N, ["there should be a key",Key]}) -> 
  step(Config, Results, {step_then, _N, ["there is a key",Key]});
step(_Config, Results, {step_then, _N, ["there is a key",Key]}) -> 
  Obj = get_Object(get_JSON(Results)),
  bdd_utils:log(debug, bdd_restrat, step, "There should be a Key ~p",[Key]),
  bdd_utils:log(trace, bdd_restrat, step, "in ~p",[Obj#item.data]),
  lists:keyfind(Key, 1, Obj#item.data) =/= false;
                                                                
step(_Config, Results, {step_then, {_Scenario, _N}, ["key",Key,"should be",Value]}) ->
  bdd_utils:log(debug, bdd_restrat, step, "Key ~p should be ~p",[Key, Value]),
  Obj = get_Object(get_JSON(Results)),
  bdd_utils:log(trace, bdd_restrat, step, "...with data ~p",[Obj#item.data]),
  Value =:= json:value(Obj#item.data, Key);

step(_Config, Results, {step_then, {_Scenario, _N}, ["key",Key,"should not be",Value]}) -> 
  true =/= step(_Config, Results, {step_then, {_Scenario, _N}, ["key",Key,"should be",Value]});

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

step(_Config, Results, {step_then, {_Scenario, _N}, ["key",Key,"should be a number"]}) -> 
  Obj = get_Object(get_JSON(Results)),
  bdd_utils:log(debug, bdd_restrat, step, "Key ~p should be a number",[Key]),
  bdd_utils:is_a(number, json:value(Obj#item.data, Key));
                                                       
step(_Config, Results, {step_then, {_Scenario, _N}, ["key",Key, "should be an empty string"]}) -> 
  bdd_utils:is_a(empty, json:value(get_JSON(Results), Key));
                                                      
step(C, R, {step_then, {S, N}, ["there should not be a value",Value]}) -> 
  step(C, R, {step_then, {S, N}, ["there should be a value",Value]}) =/= true;

step(_Config, Result, {step_then, {_Scenario, _N}, ["there should be a value",Value]}) -> 
  Obj = get_Object(get_JSON(Result)),
  J = Obj#item.data,
  bdd_utils:log(debug, bdd_restrat, step, "there should be a value ~p got ~p", [Value, J]),
  Test = lists:keyfind(Value,2,J),
  Test =/= false;

step(_Config, Results, {step_then, {Scenario, _N}, ["id",ID,"should have value",Value]}) -> 
  I = bdd_utils:scenario_retrieve(Scenario, ID, undefined),
  Result = get_JSON(Results),
  R = json:value(Result, I),
  bdd_utils:log(debug, "bdd_restrat Then ID ~p (~p) with expected value ~p should be match result ~p", [ID, I, Value, R]),
  R =:= Value;

step(_Config, Result, {step_then, _N, ["the page returns",Number]}) -> 
  step(_Config, Result, {step_then, _N, ["I get a",Number,"error"]});
step(_Config, Result, {step_then, _N, ["I get a",Number,"result"]}) -> 
  step(_Config, Result, {step_then, _N, ["I get a",Number,"error"]});
            
step(Config, Results, {step_then, _N, ["I get a",Number,"error"]}) when is_list(Number) -> 
  Numb = list_to_integer(Number),
  step(Config, Results, {step_then, _N, ["I get a",Numb,"error"]});
  
step(Config, Results, {step_then, _N, ["I get a",Number,"error"]}) -> 
  Result = get_JSON(Results, all),
  {ajax, Code, _} = Result,
  case Result of 
    {ajax, Number, _} when is_integer(Number) -> 
                          bdd_utils:log(Config, debug, "bdd_restrat step then expected ~p got result code ~p",[Number, Code]),
                          bdd_utils:log(Config, trace, "(cont)..... results ~p",[Result]),
                          true;
    {ajax, N, _} when is_list(N) -> 
                          bdd_utils:log(Config, debug, "bdd_restrat step then expected ~p assuming 200",[Number]),
                          true;
    {html, Number, _}  -> true;
    Number             -> true;
    _                  -> false
  end;

step(_Config, _Result, {_Type, _N, ["END OF RESTRAT"]}) ->
  false.
