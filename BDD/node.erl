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
-module(node).
-export([step/2, json/3, validate/1, inspector/1, g/1, create/3]).
-include("bdd.hrl").

% Commont Routine
% Provide Feature scoped strings to DRY the code
g(Item) ->
  case Item of
    path -> "api/v2/nodes";
    status_path -> "/api/status/nodes";
    name -> "bdd1.example.com";
    atom -> node1;
    _ -> crowbar:g(Item)
  end.
  
% Common Routine
% Makes sure that the JSON conforms to expectations (only tests deltas)
validate(JSON) ->
  Wrapper = crowbar_rest:api_wrapper(JSON),
  J = Wrapper#item.data,
  R =[Wrapper#item.type == node,
      bdd_utils:is_a(J, boolean, allocated), 
      bdd_utils:is_a(J, boolean, admin), 
      bdd_utils:is_a(J, string, alias), 
      crowbar_rest:validate(J)],
  bdd_utils:assert(R).

create(ID, Name, Extras) ->
  % for now, we are ignoring the extras
  JSON = json(Name, 
              proplists:get_value(description, Extras, g(description)), 
              proplists:get_value(order, Extras, g(order))),
  bdd_restrat:create(ID, node, g(path), Name, JSON).
  
% Common Routine
% Returns list of nodes in the system to check for bad housekeeping
inspector(Config) -> 
  bdd_restrat:inspector(Config, nodes).  % shared inspector works here, but may not always

% Common Routine
% Creates JSON used for POST/PUT requests
json(Name, Description, Order) ->
  json:output([{"name",Name},{"description", Description}, {"order", Order}]).

     
% Common Routine
% Validates the JSON returned by a test as part of general health tests
% Uses Feature validate, but through central routine     

% TEMPORARY REMAPPING
% -include("bdd.hrl").
step(In, Out) -> step([], In, Out).


step(Config, _Given, {step_when, {_Scenario, _N}, ["AJAX requests node status on",ID]}) ->
  Page = case ID of
    "all" -> g(status_path);
    X     -> eurl:path(g(status_path), X) 
  end,
  {200, JSON} = eurl:get_page(Config, Page, all),
  {ajax, json:parse(JSON), {get, Page}};
  
% Common Routine
% Cleans up nodes that are created during tests                         
step(Config, _Given, {step_finally, _N, ["REST removes the node",Node]}) -> 
  bdd_restrat:destroy(Config, g(path), Node);

                   
step(Config, _Global, {step_setup, _N, _}) -> 
  bdd:log(trace, "Entering Node setup step", []),

  % create node(s) for tests
  Node = json(g(name), g(description), 100),
  bdd_restrat:create([], g(path), g(atom), name, Node),
  Config;

step(_Config, _Global, {step_teardown, _N, _}) -> 
  % find the node from setup and remove it
  bdd_restrat:destroy([], g(path), g(atom)).  
