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
-export([step/3, json/3, validate/1, inspector/1, g/1]).
-export([node_add_attrib/3]).

% Commont Routine
% Provide Feature scoped strings to DRY the code
g(Item) ->
  case Item of
    path -> "2.0/crowbar/2.0/node";
    name -> "bdd1.example.com";
    atom -> node1;
    _ -> crowbar:g(Item)
  end.
  
% Common Routine
% Makes sure that the JSON conforms to expectations (only tests deltas)
validate(JSON) ->
  R =[bdd_utils:is_a(integer, json:keyfind(JSON, fingerprint)), 
      bdd_utils:is_a(boolean, json:keyfind(JSON, allocated)), 
      bdd_utils:is_a(string, json:keyfind(JSON, state)), 
      bdd_utils:is_a(boolean, json:keyfind(JSON,admin)), 
      bdd_utils:is_a(dbid, json:keyfind(JSON,os_id)), 
      crowbar_rest:validate(JSON)],
  bdd_utils:assert(R).

validate_node_attribute(JSON) ->
  R =[bdd_utils:is_a(JSON, dbid, node_id), 
      bdd_utils:is_a(JSON, dbid, attrib_id), 
      bdd_utils:is_a(JSON, str, value), 
      bdd_utils:is_a(JSON, string, state), 
      % standard checks (have to do because name is NOT standard)
      bdd_utils:is_a(JSON, string, created_at), % placeholder for createdat
      bdd_utils:is_a(JSON, string, updated_at), % placgit eholder for updatedat
      bdd_utils:is_a(JSON, "^([a-zA-Z0-9\\-]*)@([a-zA-Z0-9\\-\\.]*$)", name), 
      bdd_utils:is_a(JSON, str, description),
      bdd_utils:is_a(JSON, integer, order),
      bdd_utils:is_a(JSON, dbid, id),
      length(JSON) =:= 10],
  bdd_utils:assert(R, debug). 
  
% Common Routine
% Returns list of nodes in the system to check for bad housekeeping
inspector(Config) -> 
  bdd_restrat:inspector(Config, nodes).  % shared inspector works here, but may not always

% helpers
node_attribute_path(Node, Attribute) ->
  NodePath = eurl:path(g(path), Node),
  AttribPath = eurl:path(NodePath,"attrib"),
  eurl:path(AttribPath,Attribute).
  
node_add_attrib(Config, Node, Attribute)    -> node_add_attribute(Config, Node, Attribute).
node_add_attribute(Config, Node, Attribute) ->
  Path = node_attribute_path(Node, Attribute),
  bdd_utils:log(Config, debug, "Node connect node+attributes ~p", [Path]),
  eurl:put_post(Config, Path, [], post).

% Common Routine
% Creates JSON used for POST/PUT requests
json(Name, Description, Order) ->
  json:output([{"name",Name},{"description", Description}, {"order", Order}]).

step(Config, _Given, {step_when, _N, ["REST gets the node list"]}) -> 
  bdd_restrat:step(Config, _Given, {step_when, _N, ["REST requests the",eurl:path(g(path),""),"page"]});

step(Config, _Given, {step_when, _N, ["REST gets the node",Name]}) -> 
  bdd_restrat:step(Config, _Given, {step_when, _N, ["REST requests the",eurl:path(g(path),Name),"page"]});
     
% Common Routine
% Validates the JSON returned by a test as part of general health tests
% Uses Feature validate, but through central routine     
step(_Config, Result, {step_then, _N, ["the node is properly formatted"]}) -> 
  crowbar_rest:step(_Config, Result, {step_then, _N, ["the", nodes, "object is properly formatted"]});

step(Config, _Given, {step_when, _N, ["REST gets the node-attribute list for",Node]}) -> 
  Path = eurl:path(g(path), eurl:path(Node,"attrib")),
  bdd_utils:log(Config, debug, "Nodes get node-attributes path ~p", [Path]),
  eurl:get_ajax(Config, Path);

step(Config, _Given, {step_when, _N, ["REST assigns",attrib,Attribute,"to",node,Node]}) -> 
  R = node_add_attribute(Config, Node, Attribute),
  {ajax, json:parse(R), {200, "not attribute assign step"}};

step(Config, _Global, {step_given, {Scenario, _N}, [node,Node,"has",attrib, Attrib]}) -> 
  R = node_add_attribute(Config, Node, Attrib),
  {"node_id", NodeID} = lists:keyfind("node_id", 1, json:parse(R)),
  {"attrib_id", AttribID} = lists:keyfind("attrib_id", 1, json:parse(R)),
  {"id", NodeAttrib} = lists:keyfind("id", 1, json:parse(R)),
  bdd_utils:scenario_store(Scenario, nodeattrib, NodeAttrib),
  bdd_utils:scenario_store(Scenario, node, NodeID),
  bdd_utils:scenario_store(Scenario, attrib, AttribID),
  bdd_utils:scenario_store(Scenario, {nodeattrib, Node, Attrib}, NodeAttrib),
  bdd_utils:log(Config, debug, "node: given node ~p (~p) has attrib ~p (~p).  Result ~p",[Node, NodeID, Attrib, AttribID, R]),
  {ajax, R, {200, "node attrib assign step"}};

step(Config, Result, {step_then, _N, ["the result is a valid node-attribute json"]}) ->
  bdd_utils:log(Config, x, "node: valid node-attribute? result ~p",[Result]),
  JSON = bdd_restrat:get_JSON(Result),
  bdd_utils:log(Config, debug, "node: valid node-attribute? json ~p",[JSON]),
  validate_node_attribute(JSON);

step(Config, _Result, {step_then, _N, [node,Node,"has",attrib,Attribute]}) -> 
  Path = node_attribute_path(Node, Attribute),
  R = eurl:get_page(Config, Path, all),
  bdd_utils:log(Config, debug, "node node ~p has attribute ~p result ~p",[Node, Attribute, R]),
  case R of
    {200, _} -> true;
    _ -> false
  end;

% Common Routine
% Cleans up nodes that are created during tests                         
step(Config, _Given, {step_finally, _N, ["REST removes the node",Node]}) -> 
  bdd_restrat:destroy(Config, g(path), Node);

step(Config, _Given, {step_finally, _N, ["REST unassigns",attrib,Attribute,"from",node,Node]}) -> 
  NodePath = eurl:path(g(path), Node),
  Path = eurl:path(NodePath,"attrib"),
bdd_utils:log(Config, debug, "Node disconnect node+attributes ~p ~p", [Path, Attribute]),
  eurl:delete(Config, Path, Attribute);
                   
step(Config, _Global, {step_setup, _N, _}) -> 
  bdd:log(trace, "Entering Node setup step", []),
  % create attribute for tests
  Attrib0 = attrib:json("bddtestglobal", g(description), 100),
  bdd_restrat:create([], attrib:g(path), nodeattrib0, "bddtestglobal", Attrib0),
  Attrib1 = attrib:json("bddtest1", g(description), 101),
  bdd_restrat:create([], attrib:g(path), nodeattrib1, "bddtest1", Attrib1),
  % create node(s) for tests
  Node = json(g(name), g(description), 100),
  bdd_restrat:create([], g(path), g(atom), g(name), Node),
  node_add_attribute([], g(name), "bddtestglobal"),
  Config;

step(_Config, _Global, {step_teardown, _N, _}) -> 
  % find the node from setup and remove it
  bdd_restrat:destroy([], g(path), g(atom)),
  bdd_restrat:destroy([], attrib:g(path), nodeattrib0),
  bdd_restrat:destroy([], attrib:g(path), nodeattrib1).
  
