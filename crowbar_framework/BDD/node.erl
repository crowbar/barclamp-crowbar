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
  try
    _Description = json:keyfind(JSON, description), % ADD CHECK!,
    R =[bdd_utils:is_a(number, json:keyfind(JSON, order)),
        bdd_utils:is_a(integer, json:keyfind(JSON, fingerprint)), 
        bdd_utils:is_a(boolean, json:keyfind(JSON, allocated)), 
        bdd_utils:is_a(string, json:keyfind(JSON, state)), 
        bdd_utils:is_a(boolean, json:keyfind(JSON,admin)), 
        bdd_utils:is_a(dbid, json:keyfind(JSON,os_id)), 
        crowbar_rest:validate(JSON)],
    bdd_utils:assert(R)
  catch
    X: Y -> io:format("ERROR: parse error ~p:~p~n", [X, Y]),
		false
	end. 

% Common Routine
% Returns list of nodes in the system to check for bad housekeeping
inspector(Config) -> 
  bdd_restrat:inspector(Config, nodes).  % shared inspector works here, but may not always

% helpers
node_add_attribute(Config, Node, Attribute) ->
  NodePath = eurl:path(g(path), Node),
  AttribPath = eurl:path(NodePath,"attrib"),
  Path = eurl:path(AttribPath,Attribute),
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

step(Config, _Global, {step_given, _N, [node,Node,"has",attribute, Attribute]}) -> 
  node_add_attribute(Config, Node, Attribute);

% Common Routine
% Cleans up nodes that are created during tests                         
step(Config, _Given, {step_finally, _N, ["REST removes the node",Node]}) -> 
  bdd_restrat:destroy(Config, g(path), Node);

step(Config, _Given, {step_finally, _N, ["REST unassigns",attribute,Attribute,"from",node,Node]}) -> 
  NodePath = eurl:path(g(path), Node),
  Path = eurl:path(NodePath,"attrib"),
bdd_utils:log(Config, debug, "Node disconnect node+attributes ~p ~p", [Path, Attribute]),
  eurl:delete(Config, Path, Attribute);
                   
step(Config, _Global, {step_setup, _N, _}) -> 
  % create attribute for tests
  Attrib0 = attribute:json("bddtestglobal", g(description), 100),
  C0 = bdd_restrat:create(Config, attribute:g(path), nodeattrib0, "bddtestglobal", Attrib0),
  Attrib1 = attribute:json("bddtest1", g(description), 101),
  C1 = bdd_restrat:create(C0, attribute:g(path), nodeattrib1, "bddtest1", Attrib1),
  % create node(s) for tests
  Node = json(g(name), g(description), 100),
  C2 = bdd_restrat:create(C1, g(path), g(atom), g(name), Node),
  node_add_attribute(C2, g(name), "bddtestglobal"),
  C2;

step(Config, _Global, {step_teardown, _N, _}) -> 
  % find the node from setup and remove it
  bdd_restrat:destroy(Config, g(path), g(atom)),
  bdd_restrat:destroy(Config, attrib:g(path), nodeattrib0),
  bdd_restrat:destroy(Config, attrib:g(path), nodeattrib1).
  
