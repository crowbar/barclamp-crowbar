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
-export([step/3, json/3, validate/1, inspector/1, g/1, create/3]).
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
      bdd_utils:is_a(J, string, state), 
      bdd_utils:is_a(J, boolean, admin), 
      bdd_utils:is_a(J, dbid, os_id), 
      bdd_utils:is_a(J, string, alias), 
      bdd_utils:is_a(J, length, 12),
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

step(Config, _Global, {step_given, {_Scenario, _N}, [node,Node,"adds",attrib_type,Attrib,"with value",Value]}) -> 
  R = attrib:node_add_attrib(Config, Node, Attrib, Value),
  utils:log(debug, "node: node adds .... ~p result ~p",[Node, R]);

step(Config, _Global, {step_given, {Scenario, _N}, [node,Node,"with",attrib_type,Attrib,"has value",Value]}) -> 
  step(Config, _Global, {step_given, {Scenario, _N}, [node,Node,"adds",attrib_type,Attrib,"with value",Value]});

step(Config, Given, {step_when, {_Scenario, _N}, [node,Node,"with",attrib_type,Attrib,"is set to value",Value]}) -> 
  step(Config, Given, {step_given, {_Scenario, _N}, [node,Node,"adds",attrib_type,Attrib,"with value",Value]});

step(Config, _Given, {step_when, _N, ["REST gets the node-attribute list for",Node]}) -> 
  Path = eurl:path([g(path), Node, 'attribs']),
  bdd_utils:log(Config, debug, "Nodes get node-attributes path ~p", [Path]),
  eurl:get_ajax(Config, Path);

step(Config, _Given, {step_when, {_Scenario, _N}, ["REST gets the",node,Node,"with",attrib_type,Attrib]}) -> 
  Path = attrib:path(Node,Attrib),
  bdd_utils:log(trace,"node: Getting NodeAttrib: ~p", [Path]),
  eurl:get_ajax(Config, Path);

step(Config, _Given, {step_when, _N, ["REST assigns",attrib_type,Attrib,"to",node,Node]}) -> 
  R = attrib:node_add_attrib(Config, Node, Attrib, 'no_value'),
  bdd_restrat:ajax_return(attrib:path(Node, Attrib), post, 200, R);

step(Config, _Given, {step_when, {_Scenario, _N}, ["AJAX requests node status on",ID]}) ->
  Page = case ID of
    "all" -> g(status_path);
    X     -> eurl:path(g(status_path), X) 
  end,
  {200, JSON} = eurl:get_page(Config, Page, all),
  {ajax, json:parse(JSON), {get, Page}};
  
step(Config, _Global, {step_given, {Scenario, _N}, [node,Node,"has",attrib_type, Attrib]}) -> 
  Result = attrib:node_add_attrib(Config, Node, Attrib),
  R = crowbar_rest:api_wrapper_raw(Result),
  {"node_id", NodeID} = lists:keyfind("node_id", 1, json:parse(R)),
  {"attrib_id", AttribID} = lists:keyfind("attrib_id", 1, json:parse(R)),
  {"id", NodeAttrib} = lists:keyfind("id", 1, json:parse(R)),
  bdd_utils:scenario_store(Scenario, nodeattrib, NodeAttrib),
  bdd_utils:scenario_store(Scenario, node, NodeID),
  bdd_utils:scenario_store(Scenario, attrib, AttribID),
  bdd_utils:scenario_store(Scenario, {nodeattrib, Node, Attrib}, NodeAttrib),
  bdd_utils:log(Config, debug, "node: given node ~p (~p) has attrib ~p (~p).  Result ~p",[Node, NodeID, Attrib, AttribID, R]),
  bdd_restrat:ajax_return(attrib:path(NodeID, AttribID), post, 200, R);

step(Config, _Result, {step_then, {Scenario, _N}, [node,Node,"has no",attrib_type,Attrib]}) -> 
  true =/= step(Config, _Result, {step_given, {Scenario, _N}, [node,Node,"has",attrib_type, Attrib]});

step(Config, Result, {step_then, _N, ["the result is a valid node-attribute json"]}) ->
  bdd_utils:log(Config, x, "node: valid node-attribute? result ~p",[Result]),
  JSON = bdd_restrat:get_JSON(Result),
  bdd_utils:log(Config, debug, "node: valid node-attribute? json ~p",[JSON]),
  attrib:validate(JSON);

step(Config, _Result, {step_then, _N, [node,Node,"has",attrib_type,Attribute]}) -> 
  Path = attrib:path(Node, Attribute),
  R = eurl:get_page(Config, Path, all),
  bdd_utils:log(Config, debug, "node node ~p has attribute ~p",[Node, Attribute]),
  bdd_utils:log(Config, trace, "node .... ~p result ~p",[Node, R]),
  case R of
    {200, _} -> true;
    _ -> false
  end;

% Common Routine
% Cleans up nodes that are created during tests                         
step(Config, _Given, {step_finally, _N, ["REST removes the node",Node]}) -> 
  bdd_restrat:destroy(Config, g(path), Node);

step(Config, _Given, {step_when, _N, ["REST unassigns",attrib,Attribute,"from",node,Node]}) -> 
  step(Config, _Given, {step_finally, _N, ["REST unassigns",attrib,Attribute,"from",node,Node]}); 
step(Config, _Given, {step_finally, _N, ["REST unassigns",attrib,Attribute,"from",node,Node]}) -> 
  NodePath = eurl:path(g(path), Node),
  Path = eurl:path(NodePath,attrib:g(path)),
  bdd_utils:log(Config, debug, "Node disconnect node+attributes ~p ~p", [Path, Attribute]),
  {Code, Info} = eurl:delete(Config, Path, Attribute, all),
  {ajax, Code, Info};
                   
step(Config, _Global, {step_setup, _N, _}) -> 
  bdd:log(trace, "Entering Node setup step", []),
  % create attribute for tests
  Attrib0 = attrib_type:json("bddtestglobal", g(description), 100),
  bdd_restrat:create([], attrib_type:g(path), nodeattrib0, name, Attrib0),
  Attrib1 = attrib_type:json("bddtest1", g(description), 101),
  bdd_restrat:create([], attrib_type:g(path), nodeattrib1, name, Attrib1),
  % create node(s) for tests
  Node = json(g(name), g(description), 100),
  bdd_restrat:create([], g(path), g(atom), name, Node),
  attrib_type:node_add_attribute([], g(name), "bddtestglobal"),
  Config;

step(_Config, _Global, {step_teardown, _N, _}) -> 
  % find the node from setup and remove it
  bdd_restrat:destroy([], g(path), g(atom)),
  bdd_restrat:destroy([], attrib_type:g(path), nodeattrib0),
  bdd_restrat:destroy([], attrib_type:g(path), nodeattrib1).
  
