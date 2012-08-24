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
-module(groups).
-export([step/3, json/3, json/4]).

g(Item) ->
  case Item of
    path -> "group/2.0";
    name1 -> "bddthings";
    atom1 -> group1
  end.

json(Name, Description, Order)           -> json(Name, Description, Order, "ui").
json(Name, Description, Order, Category) ->
  json:output([{"name",Name},{"description", Description}, {"category", Category}, {"order", Order}]).
	
step(Config, _Global, {step_setup, _N, _}) -> 
  % create node(s) for tests
  JSON = json(g(name1), "BDD Testing Only - should be automatically removed", 100),
  bdd_utils:setup_create(Config, g(path), g(atom1), g(name1), JSON);

step(Config, _Global, {step_teardown, _N, _}) -> 
  % find the node from setup and remove it
  bdd_utils:teardown_destroy(Config, g(path), g(atom1)).
