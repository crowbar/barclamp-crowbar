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
-export([pop/0, pop/1, unpop/1]).  
-import(bdd_utils).
-import(digest_auth).

% create a base system
pop()           -> pop("default").
pop(ConfigName) ->
  ConfigPre = bdd:getconfig(ConfigName),
  Config = bdd:start(ConfigPre),
  %nodes
  C1 = add_node(Config, node1, "node1.crowbar.com", "Populated Information!", 250),
  C2 = add_node(C1, node2, "node2.crowbar.com", "Some Populated Information!", 260),
  C3 = add_node(C2, node3, "node3.crowbar.com", "More Populated Information!", 270),
  C4 = add_node(C3, node4, "node4.crowbar.com", "Extra Populated Information!", 280),
  C5 = add_group(C4, group1, "Rack1", "North Pole", 1000),
  io:format("Done.~n"),
  C5.

% tear it down
unpop(Config) ->
  remove(Config, nodes, node1),
  remove(Config, nodes, node2),
  remove(Config, nodes, node3),
  remove(Config, nodes, node4),
  remove(Config, groups, group1),
  bdd:stop(Config).

remove(Config, Type, Atom) ->
  bdd_utils:teardown_destroy(Config, apply(Type, g, [path]), Atom).

add_group(Config, Atom, Name, Descripton, Order) ->
  JSON = groups:json(Name, Descripton, Order, 'ui'),
  bdd_utils:setup_create(Config, groups:g(path), Atom, Name, JSON).

  
add_node(Config, Atom, Name, Description, Order) ->
  Node = nodes:json(Name, Description, Order),
  bdd_utils:setup_create(Config, nodes:g(path), Atom, Name, Node).

  