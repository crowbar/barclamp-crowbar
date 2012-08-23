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
-module(nodes).
-export([step/3, json/3]).

json(Name, Description, Order) ->
  json:output([{"name",Name},{"description", Description}, {"order", Order}]).
	
step(Config, _Global, {step_setup, _N, _}) -> 
  Path = "node/2.0",
  % create node(s) for tests
  Node1 = "bdd1.example.com",
  Node = json(Node1, "BDD Testing Only - should be automatically removed", 100),
  bdd_utils:setup_create(Config, Path, node1, Node1, Node);

step(Config, _Global, {step_teardown, _N, _}) -> 
  Path = "node/2.0",
  % find the node from setup and remove it
  bdd_utils:teardown_destroy(Config, Path, node1).
