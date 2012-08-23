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
  Node1 = "bdd1.example.com",
  % just in case, cleanup first (ignore result)
  eurl:delete(Config, Path, Node1),
  % create node(s) for tests
  Node = json(Node1, "BDD Testing Only - should be automatically removed", 100),
  Result = eurl:post(Config, Path, Node),
  {"id", Key} = lists:keyfind("id",1,Result),
  io:format("\tCreated Node ~p (id=~p) for testing.~n", [Node1, Key]),
  [{node1, Key} | Config];

step(Config, _Global, {step_teardown, _N, _}) -> 
  Path = "node/2.0",
  % find the node from setup and remove it
  Node = lists:keyfind(node1, 1, Config),
  {node1, Key} = Node,
  eurl:delete(Config, Path, Key),
  io:format("\tRemoved Node ID ~p for Tear Down Step.~n", [Key]),
  lists:delete(Node, Config).
  