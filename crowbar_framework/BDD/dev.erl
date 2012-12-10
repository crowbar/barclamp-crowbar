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

-module(dev).
-export([pop/0, pop/1, unpop/0, unpop/1]).  
-import(bdd_utils).
-import(digest_auth).

% create a base system
pop()           -> pop(default).
pop(ConfigName) when is_atom(ConfigName) ->
  pop(bdd:getconfig(atom_to_list(ConfigName)));
pop(ConfigRaw) ->
  Config = bdd:start(ConfigRaw),
  %nodes
  Nodes = [{node1, "node1.crowbar.com", "Populated Information!", 250, "Rack1"},
            {node2, "node2.crowbar.com", "Some Populated Information!", 260, "Rack1"},
            {node3, "node3.crowbar.com", "More Populated Information!", 270, "Rack1"},
            {node4, "node4.crowbar.com", "Extra Populated Information!", 280, "Rack1"}],
  Attributes = [{attrib1, "cpu"}, {attrib2, "service_tag"}],
  C0 = add_group(Config, group1, "Rack1", "North Pole", 1000),
  C1 = add_node(C0, node1, "node1.crowbar.com", "Populated Information!", 250, "Rack1"),
  C2 = add_node(C1, node2, "node2.crowbar.com", "Some Populated Information!", 260, "Rack1"),
  C3 = add_node(C2, node3, "node3.crowbar.com", "More Populated Information!", 270, "Rack1"),
  C4 = add_node(C3, node4, "node4.crowbar.com", "Extra Populated Information!", 280, "Rack1"),
  io:format("Done.~n"),
  C4.

% tear it down
unpop()       ->  unpop(get()).
unpop(Config) ->
  C1 = remove(Config, nodes, node1),
  C2 = remove(C1, nodes, node2),
  C3 = remove(C2, nodes, node3),
  C4 = remove(C3, nodes, node4),
  C5 = remove(C4, groups, group1),
  C5.

remove(Config, Type, Atom) ->
  crowbar_rest:destroy(Config, apply(Type, g, [path]), Atom).

add_group(Config, Atom, Name, Descripton, Order) ->
  JSON = groups:json(Name, Descripton, Order, 'ui'),
  crowbar_rest:create(Config, groups:g(path), Atom, Name, JSON).


add_node(Config, Atom, Name, Description, Order, Group) ->
  C = add_node(Config, Atom, Name, Description, Order),
  groups:step(Config, [], {step_when, 0, ["REST adds the node",Name,"to",Group]}),
  C.
  
add_node(Config, Atom, Name, Description, Order) ->
  Node = nodes:json(Name, Description, Order),
  bdd_restrat:create(Config, nodes:g(path), Atom, Name, Node).

  