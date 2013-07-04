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
-export([step/2]).
-export([get_id/1, get_id/2, create/2, create/3, create/4, create/5, destroy/2, update/4, validate/1]).
-export([get_object/1, get_result/2, parse_object/1, alias/1]).
-export([step/3]).  % depricate
-include("bdd.hrl").

% HELPERS ============================

% Find records from the results
get_result(Results, Type) ->  bdd_utils:depricate({2014, 1, 1}, bdd_restrat, get_result, eurl, get_result, [Results, Type]).
  
% handles cases where objects use names that conflict w/ internal namespaces
alias(Object)                 -> bdd_utils:config(alias_map,{Object, Object}).
alias(Object, Method, Params) -> apply(alias(Object), Method, Params).

% wrapper looks at header to see if there is meta data to help with override parsing
% if it's json then use the parser from bdd_utils
% if it's a vendor namespace then use naming convention to resolve
get_object(Result) when is_record(Result, http) -> 
  apply(Result#http.namespace, parse_object, [Result]).

% rest response for generic JSON parsing by inspecting the text
parse_object(Results) ->
  case Results#http.data of 
    [${ | _] -> JSON = json:parse(Results#http.data),
                {"id", ID} = lists:keyfind("id", 1, JSON),
                #obj{namespace = rest, data=JSON, id= ID, type = json, url = Results#http.url };
    [$[ | _] -> JSON = json:parse(Results#http.data),
                #list{namespace = rest, data=JSON, type = json, url = Results#http.url };
    D        -> bdd_utils:log(debug, "JSON API returned non-JSON result.  Returned ~p", [Results]),
                #obj{namespace = rest, data=D, url=Results#http.url, id = -1 }
  end.

% this should NOT be called here, only in the objects
validate(_) -> bdd_utils:log(warn, "bdd_restrat:validate should not be called. Use your platform specific validator").

% given a path + key, returns the ID of the object
get_id(Path, Key) -> get_id(eurl:path(Path,Key)).
get_id(Path) ->
  R = eurl:get(Path),
  O = get_object(R),
  bdd_utils:log(trace, bdd_restrat, get_id, "path ~p Result: ~p", [Path, R]),
  if 
    is_record(O, obj) -> O#obj.id;
    true -> "-1" 
  end.

% helper common to all setups using REST 

%_Config, g(path),username, User
create(Path, JSON)                  -> create(Path, name, JSON, post).
create(Path, KeyName, JSON)         -> create(Path, KeyName, JSON, post).
create(Path, Atom, KeyName, JSON)   when is_atom(Atom)   -> create(Path, Atom, KeyName, JSON, post);
create(Path, KeyName, JSON, Action) when is_atom(Action) ->
  % just in case - cleanup to prevent collision
  Key = json:keyfind(JSON, KeyName),
  bdd_utils:log(debug,"bdd_restrat create object key ~p=~p on Path ~p using ID from ~p",[KeyName,Key,Path,Action]),
  destroy(Path, Key),
  % create node(s) for tests
  eurl:put_post(Path, JSON, Action).
create(Scenario, Type, Path, Name, JSON) when is_number(Scenario), is_atom(Type) ->
  bdd_utils:log(debug,"bdd_restrat create for Scenario ~p of ~p ~p with path ~p",[Scenario,Type,Name,Path]),
  destroy(Path, Name),
  Result = eurl:put_post(Path, JSON, post),
  O = get_object(Result), 
  bdd_utils:scenario_store(Scenario, Type, O#obj.id),
  Result;
create(Path, Atom, KeyName, JSON, Action) when is_atom(Atom) ->
  bdd_utils:log(trace, "bdd_restrat:create Path: ~p, Atom: ~p, KeyName: ~p, Action: ~p", [Path, Atom, KeyName, Action]),
  Result = json:parse(create(Path, KeyName, JSON, Action)),
  % get the ID of the created object
  Key = json:keyfind(Result, id),
  % friendly message
  bdd_utils:log(debug, "Created ~s (key=~s & id=~s) for testing.", [KeyName, Atom, Key]),
  % add the new ID to the config list
  bdd_utils:config_set(Atom, Key).

update(Path, Atom, KeyName, JSON) ->
  bdd_utils:log(trace, "bdd_restrat:update Path: ~p, Atom: ~p, KeyName: ~p, JSON: ~p", [Path, Atom, KeyName, JSON]),
  PutResult = eurl:put_post(Path, JSON, put),
  bdd_utils:log(debug, "Update done, result: ~p !!!",[PutResult]),
  PutResult.

% helper common to all setups using REST
destroy(Path, Atom) when is_atom(Atom) ->
  Item = bdd_utils:config(Atom, not_found),
  bdd_utils:log(trace, "bdd_restrat:destroy(atom) deleting ~p with id ~p using path ~p",[Atom, Item, Path]),
  case Item of
    not_found -> 
        bdd_utils:log(debug, "bdd_restrat:destroy(atom) could not find ID for atom ~p (likely OK)",[Atom]);
    Key       -> 
        destroy(Path, Key),
        bdd_utils:unset(Item)
  end;

% helper common to all setups using REST
destroy(Path, Key) ->
  bdd_utils:log(trace, "Entering bdd_restrat:destroy Path: ~p, Key: ~p", [Path, Key]),
  case get_id(Path, Key) of
    "-1" -> bdd_utils:log(trace, "Removal of key ~s skipped: not found.", [Key]);
    ID   -> eurl:delete(Path, ID),
            bdd_utils:log(debug, "Removed key ~s & id ~s.", [Key, ID])
  end,
  done.
  
% DEPRICATE!
step(_Config, B, C) -> bdd_utils:depricate({2013, 10, 1}, bdd_restrat, step, bdd_restrat, step, [B, C]).

% GIVEN STEPS ======================
step(Global, {step_given, _N, ["there is not a",Object, Name]}) -> 
  step(Global, {step_finally, _N, ["REST deletes the",Object, Name]});

step(Global, {step_given, _N, ["there is a",Object, Name]}) -> 
  step(Global, {step_when, _N, ["REST creates the",Object,Name]});

step(_Global, {step_given, _N, ["I require a",Object, Key]}) -> 
  URI = eurl:path(alias(Object, g, [path]), Key),
  case eurl:get_page([], URI, all) of
    {200, "null"} -> bdd_utils:log(warn, bdd_restrat, step, "Required ~p object ~p was not found.  Test will likely fail",[Object, Key]);
    {200, _}      -> bdd_utils:log(debug, bdd_restrat, step, "Required ~p object ~p found.",[Object, Key]);
    {404, _}      -> bdd_utils:log(warn, bdd_restrat, step, "Required ~p object ~p not found (404).",[Object, Key]);
    {Num, _}      -> bdd_utils:log(error, bdd_restrat, step, "Required ~p object ~p error code ~p.",[Object, Key, Num])
  end;

% WHEN STEPS ======================
step(_Given, {step_when, _N, ["REST requests the",Page,"page"]}) ->
  R = eurl:get_http(Page),
  [R, get_object(R)];

step(_Given, {step_when, _N, ["REST gets the",Object,"list"]}) when is_atom(Object) -> 
  % This relies on the pattern objects providing a g(path) value mapping to their root information
  URI = alias(Object, g, [path]),
  bdd_utils:log(debug, bdd_restrat, step, "REST get ~p list for ~p path", [Object, URI]),
  step(_Given, {step_when, _N, ["REST requests the",URI,"page"]});

step(_Given, {step_when, _N, ["REST gets the",Object,Key]})  when is_atom(Object) ->
  % This relies on the pattern objects providing a g(path) value mapping to their root information
  URI = eurl:path(alias(Object, g, [path]), Key),
  bdd_utils:log(debug, bdd_restrat, step, "REST get the object ~p for ~p path", [Object, URI]),
  step(_Given, {step_when, _N, ["REST requests the",URI,"page"]});

step(_Given, {step_when, _N, ["REST cannot find the",Page,"page"]}) ->
  bdd_utils:log(debug, "REST cannot find the... START"),
  R = eurl:get_http(Page),
  200 =/= R#http.code;

step(_Global, {step_when, _N, ["REST creates a",Object,Name]}) -> 
  step(_Global, {step_when, _N, ["REST creates the",Object,Name]});
step(_Global, {step_given, _N, ["REST creates a",Object,Name]}) -> 
  step(_Global, {step_when, _N, ["REST creates the",Object,Name]});
step(_Global, {step_given, _N, ["REST creates the",Object,Name]}) -> 
  step(_Global, {step_when, _N, ["REST creates the",Object,Name]});

step(_Given, {step_when, {ScenarioID, _N}, ["REST creates the",Object,Name]}) -> 
  bdd_utils:log(debug, "bdd_restrat: step REST creates the ~p ~p", [Object, Name]),
  JSON = alias(Object, json, [Name, alias(Object, g, [description]), alias(Object, g, [order])]),
  Path = alias(Object, g, [path]),
  Result = eurl:put_post(Path, JSON, post),
  bdd_utils:log(trace, "bdd_restrat:REST creates the step: PutPostResult: ~p", [Result]),
  O = get_object(Result),
  bdd_utils:scenario_store(ScenarioID, Object, O#obj.id),
  [Result, O];

step(_Given, {step_when, _N, ["REST updates the",Object,Name]}) when is_atom(Object) -> 
  JSON = alias(Object, json, [Name, alias(Object, g, [description]), alias(Object, g, [order])]),
  Path = eurl:path([alias(Object, g, [path]), Name]),
  step(_Given, {step_when, _N, ["REST updates an object at",Path,"with",JSON]});

step(_Given, {step_when, _N, ["REST updates an object at",Path,"with",JSON]}) ->
  bdd_utils:log(trace, "REST updates an object at ~p with ~p", [Path,JSON]),
  Result = eurl:put_post(Path, JSON, put),
  [Result, get_object(Result)];

step(_Then, {step_finally, _N, ["REST deletes the", Object, Name]}) -> 
  step(_Then, {step_when, _N, ["REST deletes the",Object, Name]});

step(_Given, {step_when, _N, ["REST deletes the",Object, Name]}) when is_atom(Object)-> 
  Path = alias(Object, g, [path]),
  R = eurl:delete(Path, Name),
  bdd_utils:log(debug, bdd_restrat, step, "delete ~p ~p = ~p",[Object,Path, R]),
  R;
  
step(Given, {step_finally, _N, ["REST removes the",Object, Name]}) when is_atom(Object)-> 
  step(Given, {step_when, _N, ["REST deletes the",Object, Name]});
step(Given, {step_finally, _N, ["REST removes",Object, Name]}) when is_atom(Object)-> 
  step(Given, {step_when, _N, ["REST deletes the",Object, Name]});

step(Results, {step_then, _N, ["REST call returned success"]}) ->
  R = get_result(Results, http),
  case R#http.code of
    200 -> true;        % catches new format
    _   -> bdd_utils:log(debug,bdd_restrat, step, "REST call DID NOT return success! with ~p",[R]), 
           false
  end;

step(_Results, {step_then, _N, ["there is a", Object, Key]}) ->
  URI = alias(Object, g, [path]),
  R =eurl:get(eurl:path(URI, Key)),
  200 =:= R#http.code;

step(_Results, {step_then, _N, ["there is not a", Object, Key]}) ->
  URI = alias(Object, g, [path]),
  R =eurl:get(eurl:path(URI, Key)),
  404 =:= R#http.code;
  
step(Results, {step_then, _N, ["the", Object, "is properly formatted"]}) when is_atom(Object) ->
  % This relies on the pattern objects providing a g(path) value mapping to their root information 
  case get_result(Results, obj) of 
    not_found ->  Http = get_result(Results, http),
                  bdd_utils:log(warn, "bdd_restrat: code ~p from ~p", [Http#http.code, Http#http.url]), 
                  false;
    Obj       -> alias(Object, validate, [Obj])
  end;
    
step(Results, {step_then, _N, ["there should be a key",Key]}) -> 
  step(Results, {step_then, _N, ["there is a key",Key]});
step(Results, {step_then, _N, ["there is a key",Key]}) -> 
  Obj = get_result(Results, obj),
  bdd_utils:log(debug, bdd_restrat, step, "There should be a Key ~p",[Key]),
  bdd_utils:log(trace, bdd_restrat, step, "in ~p",[Obj#item.data]),
  lists:keyfind(Key, 1, Obj#item.data) =/= false;
                                                                
step(Results, {step_then, {_Scenario, _N}, ["key",Key,"should be",Value]}) ->
  bdd_utils:log(debug, bdd_restrat, step, "Key ~p should be ~p",[Key, Value]),
  Obj = get_object(eurl:get(Results)),
  bdd_utils:log(trace, bdd_restrat, step, "...with data ~p",[Obj#item.data]),
  Value =:= json:value(Obj#item.data, Key);

step(Results, {step_then, {_Scenario, _N}, ["key",Key,"should not be",Value]}) -> 
  true =/= step(Results, {step_then, {_Scenario, _N}, ["key",Key,"should be",Value]});

step(Results, {step_then, _N, ["key",Key, "should contain",Count,"items"]}) -> 
  {C, _} = string:to_integer(Count),
  List = json:value(get_result(Results, obj), Key),
  Items = length(List),
  Items =:= C;
                                                                
step(Results, {step_then, _N, ["key",Key,"should contain at least",Count,"items"]}) ->
  {C, _} = string:to_integer(Count),
  List = json:value(get_result(Results, obj), Key),
  Items = length(List),
  Items >= C;

step(Results, {step_then, {_Scenario, _N}, ["key",Key,"should be a number"]}) -> 
  Obj = get_result(Results, obj),
  bdd_utils:log(debug, bdd_restrat, step, "Key ~p should be a number",[Key]),
  bdd_utils:is_a(number, json:value(Obj#item.data, Key));
                                                       
step(Results, {step_then, {_Scenario, _N}, ["key",Key, "should be an empty string"]}) -> 
  bdd_utils:is_a(empty, json:value(get_result(Results, obj), Key));
                                                      
step(R, {step_then, {S, N}, ["there should not be a value",Value]}) -> 
  step(R, {step_then, {S, N}, ["there should be a value",Value]}) =/= true;

step(Result, {step_then, {_Scenario, _N}, ["there should be a value",Value]}) -> 
  Obj = get_result(Result, obj),
  J = Obj#obj.data,
  bdd_utils:log(debug, bdd_restrat, step, "there should be a value ~p got ~p", [Value, J]),
  Test = lists:keyfind(Value,2,J),
  Test =/= false;

step(Result, {step_then, {_Scenario, _N}, ["the list should have an object with key", Key, "value", Value]}) ->
  List = get_result(Result, list),
  Values = [proplists:get_value(Key, O) || O <- List#list.data],
  Matches = [V || V <- Values, V =:= Value],
  length(Matches) > 0;
   
step(Results, {step_then, {Scenario, _N}, ["id",ID,"should have value",Value]}) -> 
  I = bdd_utils:scenario_retrieve(Scenario, ID, undefined),
  Result = get_result(Results, obj),
  R = json:value(Result, I),
  bdd_utils:log(debug, "bdd_restrat Then ID ~p (~p) with expected value ~p should be match result ~p", [ID, I, Value, R]),
  R =:= Value;

step(Result, {step_then, _N, ["the page returns",Number]}) -> 
  step(Result, {step_then, _N, ["I get a",Number,"error"]});
step(Result, {step_then, _N, ["I get a",Number,"result"]}) -> 
  step(Result, {step_then, _N, ["I get a",Number,"error"]});
            
step(Results, {step_then, _N, ["I get a",Number,"error"]}) when is_list(Number) -> 
  Numb = list_to_integer(Number),
  step(Results, {step_then, _N, ["I get a",Numb,"error"]});
  
step(Results, {step_then, _N, ["I get a",Numb,"error"]}) -> 
  Result = get_result(Results, http),
  bdd_utils:log(debug, bdd_restrat, step, "I get a ~p error from ~p request", [Result#http.code, Result#http.url]),
  Result#http.code =:= Numb;

step(_Result, {_Type, _N, ["END OF RESTRAT"]}) ->
  false.