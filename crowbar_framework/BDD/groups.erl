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
-export([step/3, json/3, json/4, validate/1]).
	
validate(JSON) ->
  try JSON of
    [{"category",_Category}, {"created_at",_CreatedAt}, 
     {"description",_Description}, {"id",Id}, {"name",Name}, 
     {"order",Order}, {"updated_at",_UpdatedAt}]
    -> R = [bdd_utils:is_a(number, Order), 
            bdd_utils:is_a(name, Name), 
            bdd_utils:is_a(number, Id)],
      bdd_utils:assert(R)
  catch
    X: Y -> io:format("ERROR: parse error ~p:~p~n", [X, Y]),
		false
	end. 
 
g(Item) ->
  case Item of
    path -> "group/2.0";
    name1 -> "bddthings";
    atom1 -> group1
    name2 -> "bdddelete";
    atom2 -> group2
  end.

json(Name, Description, Order)           -> json(Name, Description, Order, "ui").
json(Name, Description, Order, Category) ->
  json:output([{"name",Name},{"description", Description}, {"category", Category}, {"order", Order}]).
	
step(Config, _Global, {step_setup, _N, _}) -> 
  % create node(s) for tests
  JSON = json(g(name1), "BDD Testing Only - should be automatically removed", 100),
  bdd_utils:setup_create(Config, g(path), g(atom1), g(name1), JSON),
  JSON = json(g(name2), "BDD Testing Only - should be automatically removed", 200),
  bdd_utils:setup_create(Config, g(path), g(atom2), g(name2), JSON);

step(Config, _Global, {step_teardown, _N, _}) -> 
  % find the node from setup and remove it
  bdd_utils:teardown_destroy(Config, g(path), g(atom2)),
  bdd_utils:teardown_destroy(Config, g(path), g(atom1)).
