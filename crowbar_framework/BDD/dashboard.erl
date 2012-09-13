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
-module(dashboard).
-export([step/3, g/1]).

g(Item) ->
  case Item of
    name -> "dashboard1.example.com";
    atom -> dashboard1;
    _ -> nodes:g(Item)
  end.
	
step(Config, _Global, {step_setup, _N, _}) -> 
  % create node(s) for tests
  Node = nodes:json(g(name), g(description), 100),
  bdd_utils:setup_create(Config, g(path), g(atom), g(name), Node);

step(Config, _Global, {step_teardown, _N, _}) -> 
  % find the node from setup and remove it
  bdd_utils:teardown_destroy(Config, g(path), g(atom)).

