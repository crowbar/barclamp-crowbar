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
-export([get_object/1, get_result/2, parse_object/1, alias/1, alias/3]).
-include("bdd.hrl").

% HELPERS ============================

% Find records from the results
get_result(Results, Type) ->  bdd_utils:depricate({2014, 1, 1}, bdd_restrat, get_result, eurl, get_result, [Results, Type]).
  
% handles cases where objects use names that conflict w/ internal namespaces
alias(Object)                 -> bdd_utils:alias(Object).
alias(Object, Method, Params) -> 
  try apply(bdd_utils:alias(Object), Method, Params) of
    R -> R
  catch
    error: undef  -> bdd_utils:log(error, bdd_restrat, alias, "Missing ~p:~p([params]).  Object Abstraction fails.", [Object, Method]), false;
    Cause: Reason -> bdd_utils:log(error, bdd_restrat, alias, "Unexpected ~p error due to ~p.  Object ~p:~p Abstraction fails.", [Cause, Reason, Object, Method]), false
  end.

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
  R = eurl:get_http(URI),
  [R, get_object(R)];

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
  bdd_utils:log(debug, bdd_restrat, step, "REST creates the ~p ~p", [alias(Object), Name]),
  JSON = alias(Object, json, [Name, alias(Object, g, [description]), alias(Object, g, [order])]),
  Path = alias(Object, g, [path]),
  Result = eurl:put_post(Path, JSON, post),
  bdd_utils:log(trace, bdd_restrat, step, "REST creates the step: PutPostResult: ~p", [Result]),
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
  R = bdd_crud:delete(Path, Name),
  bdd_utils:log(debug, bdd_restrat, step, "delete ~p ~p = ~p",[Object,Path, R]),
  R;
  
step(Given, {step_finally, _N, ["REST removes the",Object, Name]}) when is_atom(Object)-> 
  step(Given, {step_when, _N, ["REST deletes the",Object, Name]});
step(Given, {step_finally, _N, ["REST removes",Object, Name]}) when is_atom(Object)-> 
  step(Given, {step_when, _N, ["REST deletes the",Object, Name]});

step(Results, {step_then, _N, ["REST call returned success"]}) ->
  R = eurl:get_result(Results, http),
  case R#http.code of
    200 -> true;        % catches new format
    _   -> bdd_utils:log(debug,bdd_restrat, step, "REST call DID NOT return success! with ~p",[R]), 
           false
  end;

step(_Results, {step_then, _N, ["there is a", Object, Key]}) ->
  URI = alias(Object, g, [path]),
  R =eurl:get_http(eurl:path(URI, Key)),
  200 =:= R#http.code;

step(_Results, {step_then, _N, ["there is not a", Object, Key]}) ->
  URI = alias(Object, g, [path]),
  R =eurl:get_http(eurl:path(URI, Key)),
  404 =:= R#http.code;
  
step(Results, {step_then, _N, ["the", Object, "is properly formatted"]}) when is_atom(Object) ->
  % This relies on the pattern objects providing a g(path) value mapping to their root information 
  case eurl:get_result(Results, obj) of 
    not_found ->  Http = eurl:get_result(Results, http),
                  bdd_utils:log(warn, bdd_restrat, step, "code ~p from ~p", [Http#http.code, Http#http.url]), 
                  false;
    Obj       -> alias(Object, validate, [Obj])
  end;
    
step(Results, {step_then, _N, ["there should be a key",Key]}) -> 
  step(Results, {step_then, _N, ["there is a key",Key]});
step(Results, {step_then, _N, ["there is a key",Key]}) -> 
  Obj = eurl:get_result(Results, obj),
  bdd_utils:log(debug, bdd_restrat, step, "There should be a Key ~p",[Key]),
  bdd_utils:log(trace, bdd_restrat, step, "in ~p",[Obj#obj.data]),
  lists:keyfind(Key, 1, Obj#obj.data) =/= false;
                                                                
step(Results, {step_then, {_Scenario, _N}, ["key",Key,"should be",Value]}) ->
  bdd_utils:log(debug, bdd_restrat, step, "Key ~p should be ~p",[Key, Value]),
  Obj = get_object(eurl:get(Results)),
  bdd_utils:log(trace, bdd_restrat, step, "...with data ~p",[Obj#obj.data]),
  Value =:= json:value(Obj#obj.data, Key);

step(Results, {step_then, {_Scenario, _N}, ["key",Key,"should not be",Value]}) -> 
  true =/= step(Results, {step_then, {_Scenario, _N}, ["key",Key,"should be",Value]});

step(Results, {step_then, _N, ["key",Key, "should contain",Count,"items"]}) -> 
  {C, _} = string:to_integer(Count),
  List = json:value(eurl:get_result(Results, obj), Key),
  Items = length(List),
  Items =:= C;
                                                                
step(Results, {step_then, _N, ["key",Key,"should contain at least",Count,"items"]}) ->
  {C, _} = string:to_integer(Count),
  List = json:value(eurl:get_result(Results, obj), Key),
  Items = length(List),
  Items >= C;

step(Results, {step_then, {_Scenario, _N}, ["key",Key,"should be a number"]}) -> 
  Obj = eurl:get_result(Results, obj),
  bdd_utils:log(debug, bdd_restrat, step, "Key ~p should be a number",[Key]),
  bdd_utils:is_a(number, json:value(Obj#obj.data, Key));
                                                       
step(Results, {step_then, {_Scenario, _N}, ["key",Key, "should be an empty string"]}) -> 
  bdd_utils:is_a(empty, json:value(eurl:get_result(Results, obj), Key));
                                                      
step(R, {step_then, {S, N}, ["there should not be a value",Value]}) -> 
  step(R, {step_then, {S, N}, ["there should be a value",Value]}) =/= true;

step(Result, {step_then, {_Scenario, _N}, ["there should be a value",Value]}) -> 
  Obj = eurl:get_result(Result, obj),
  J = Obj#obj.data,
  bdd_utils:log(debug, bdd_restrat, step, "there should be a value ~p got ~p", [Value, J]),
  Test = lists:keyfind(Value,2,J),
  Test =/= false;

step(Result, {step_then, {_Scenario, _N}, ["the list should have an object with key", Key, "value", Value]}) ->
  List = eurl:get_result(Result, list),
  Values = [proplists:get_value(Key, O) || O <- List#list.data],
  Matches = [V || V <- Values, V =:= Value],
  length(Matches) > 0;
   
step(Results, {step_then, {Scenario, _N}, ["id",ID,"should have value",Value]}) -> 
  I = bdd_utils:scenario_retrieve(Scenario, ID, undefined),
  Result = eurl:get_result(Results, obj),
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
  Result = eurl:get_result(Results, http),
  bdd_utils:log(debug, bdd_restrat, step, "I get a ~p error from ~p request", [Result#http.code, Result#http.url]),
  Result#http.code =:= Numb;

step(_Result, {_Type, _N, ["END OF RESTRAT"]}) ->
  false.