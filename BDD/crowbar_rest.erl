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
-module(crowbar_rest).
-export([step/3, g/1, validate_core/1, validate/1, api_wrapper/1, api_wrapper_raw/1, api_wrapper/2, inspector/2]).
-export([get_id/2, get_id/3, create/3, create/4, create/5, create/6, destroy/3]).
-import(bdd_utils).
-import(json).
-include("bdd.hrl").

g(Item) ->
  case Item of
    _ -> crowbar:g(Item)
  end.
% validates JSON in a generic way common to all objects
validate_core(JSON) ->
  R = [bdd_utils:is_a(JSON, string, created_at), % placeholder for createdat
       bdd_utils:is_a(JSON, string, updated_at), % placgit eholder for updatedat
       bdd_utils:is_a(JSON, name, name), 
       bdd_utils:is_a(JSON, dbid, id)],
  bdd_utils:assert(R, debug). 

validate(JSON) ->
  R = [
       bdd_utils:is_a(JSON, str, description),
       bdd_utils:is_a(JSON, int, order),
       validate_core(JSON)],
  bdd_utils:assert(R, debug). 

api_wrapper_raw(J) ->  
  JSON = json:parse(J),
  api_wrapper(JSON).
api_wrapper(JSON) ->  
  Type = json:keyfind(JSON, "type"),
  api_wrapper(Type, JSON).
api_wrapper(not_found, JSON) -> #item{data=JSON};
api_wrapper(Type, JSON) when is_list(Type) 
                             -> api_wrapper(list_to_atom(Type), JSON);
api_wrapper(Type, JSON)      ->
  Link = json:keyfind(JSON, "link"),
  List = json:keyfind(JSON, "list"),
  case List of
    not_found -> #item{type=Type, data=json:keyfind(JSON, "item"), link=Link};
    [[]]      -> #list{type=Type, data=[], link=Link, ids=[], count=0};
    _         -> IDs = [{json:keyfind(R,"id"),json:keyfind(R,"name")} || R <- List],
                % note: the ids field is for backward compatability against the legacy 2.0 api
                 #list{type=Type, data=List, link=Link, count=json:keyfind(JSON, "count"), ids = IDs}
  end.

% Common Routine - returns a list of items from the system, used for house keeping
inspector(Config, Feature) ->
  Raw = eurl:get(Config, apply(Feature, g, [path])),
  JSON = json:parse(Raw),
  List = api_wrapper(JSON),
  [{Feature, ID, Name} || {ID, Name} <- List#list.data].
  
% given a path + key, returns the ID of the object
get_id(Config, Path, Key) -> 
  bdd_utils:depricate({2013,4,1}, crowbar_rest, get_id, bdd_restrat, get_id, [Config, Path, Key]).
   
% given a path, returns the ID of the object
get_id(Config, Path) ->
  bdd_utils:depricate({2013,4,1}, crowbar_rest, get_id, bdd_restrat, get_id, [Config, Path]).

% helper common to all setups using REST
create(Config, Path, JSON)         -> 
  bdd_utils:depricate({2013,4,1}, crowbar_rest, create, bdd_restrat, create, [Config, Path, JSON]).
  
create(Config, Path, JSON, Action) -> 
  bdd_utils:depricate({2013,4,1}, crowbar_rest, create, bdd_restrat, create, [Config, Path, JSON, Action]).
  
create(Config, Path, Atom, Name, JSON) ->
  bdd_utils:depricate({2013,4,1}, crowbar_rest, create, bdd_restrat, create, [Config, Path, Atom, Name, JSON]).

create(Config, Path, Atom, Name, JSON, Action) ->
  bdd_utils:depricate({2013,4,1}, crowbar_rest, create, bdd_restrat, create, [Config, Path, Atom, Name, JSON, Action]).

% helper common to all setups using REST
destroy(Config, Path, Atom) when is_atom(Atom) ->
  bdd_utils:depricate({2013,4,1}, crowbar_rest, destroy, bdd_restrat, destroy, [Config, Path, Atom]);

% helper common to all setups using REST
destroy(Config, Path, Key) ->
  bdd_utils:depricate({2013,4,1}, crowbar_rest, destroy, bdd_restrat, destroy, [Config, Path, Key]).

% NODES 
step(Config, _Global, {step_given, _N, ["there is a node",Node]}) -> 
  JSON = node:json(Node, node:g(description), 200),
  bdd_restrat:create(Config, node:g(path), JSON);

% remove the node
step(Config, _Given, {step_finally, _N, ["throw away node",Node]}) -> 
  eurl:delete(Config, node:g(path), Node);

% GROUPS
step(Config, _Global, {step_given, _N, ["there is a",Category,"group",Group]}) -> 
  JSON = groups:json(Group, groups:g(description), 200, Category),
  bdd_restrat:create(Config, groups:g(path), JSON);

% remove the group
step(Config, _Given, {step_finally, _N, ["throw away group",Group]}) -> 
  bdd_restrat:destroy(Config, groups:g(path), Group);

% ============================  WHEN STEPS =========================================

step(Config, _Given, {step_when, {_Scenario, _N}, ["REST gets the",barclamp,Barclamp,Resource,"list"]}) -> 
  Path = eurl:path([Barclamp,g(version),apply(bdd_restrat:alias(Resource),g,[resource])]),
  bdd_utils:log(debug, crowbar, step, "REST get ~p list for ~p barclamp", [Resource, Barclamp]),
  {Code, JSON} = eurl:get_page(Config, Path, all),
  Wrapper = crowbar_rest:api_wrapper_raw(JSON),
  bdd_utils:log(trace, bdd_restrat, step, "REST get ~p list: ~p", [Resource, Wrapper]),
  bdd_restrat:ajax_return(Path, get, Code, Wrapper#list.ids);  

% ============================  THEN STEPS =========================================

% validate object based on basic rules for Crowbar
step(_Config, Result, {step_then, _N, ["the object is properly formatted"]}) -> 
  {ajax, JSON, _} = lists:keyfind(ajax, 1, Result),     % ASSUME, only 1 ajax result per feature
  validate(JSON);
  
% validate object based on it the validate method in it's ERL file (if any)
% expects an ATOM for the file
step(_Config, Result, {step_then, _N, ["the", Feature, "object is properly formatted"]}) -> 
  {ajax, JSON, _} = lists:keyfind(ajax, 1, Result),     % ASSUME, only 1 ajax result per feature
  apply(Feature, validate, [JSON]);

% validates a list of object IDs
step(_Config, Result, {step_then, _N, ["the object id list is properly formatted"]}) ->
  {ajax, JSON, _} = lists:keyfind(ajax, 1, Result),     % ASSUME, only 1 ajax result per feature
  NumberTester = fun(Value) -> bdd_utils:is_a(integer, Value) end,
  lists:all(NumberTester, JSON);

% ============================  LAST RESORT =========================================
step(_Config, _Given, {step_when, _N, ["I have a test that is not in Crowbar_Rest"]}) -> true;
                                    
step(_Config, _Result, {step_then, _N, ["I should use my special step file"]}) -> true.
