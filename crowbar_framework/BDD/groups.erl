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
-export([step/3, json/3, json/4, g/1, validate/1]).
	
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
    path -> "2.0/group";
    name1 -> "bddthings";
    atom1 -> group1;
    name2 -> "bdddelete";
    atom2 -> group2;
    _ -> crowbar:g(Item)
  end.

json(Name, Description, Order)           -> json(Name, Description, Order, "ui").
json(Name, Description, Order, Category) ->
  json:output([{"name",Name},{"description", Description}, {"category", Category}, {"order", Order}]).
	
step(_Config, _Given, {step_when, _N, ["AJAX gets the group",Name]}) -> 
  bdd_webrat:step(_Config, _Given, {step_when, _N, ["AJAX requests the",eurl:path(g(path),Name),"page"]});
      
step(Config, _Given, {step_when, _N, ["REST adds the node",Node,"to",Group]}) -> 
  GroupPath = eurl:path(g(path),Group),
  NodePath = eurl:path("node",Node),
  AddPath = eurl:path(GroupPath, NodePath),
  Result = eurl:post(Config, AddPath, []),
  validate(Result);
  
step(_Config, _Result, {step_then, _N, ["the group",Group,"should have at least",Count,"node"]}) -> 
  false;
  
step(_Config, _Result, {step_then, _N, ["the node",Node,"should be in group",Group]}) -> 
  false;
                                                                
step(Config, _Global, {step_setup, _N, _}) -> 
  % create node(s) for tests
  JSON1 = json(g(name1), g(description), 100),
  Config1 = bdd_utils:setup_create(Config, g(path), g(atom1), g(name1), JSON1),
  JSON2 = json(g(name2), g(description), 200),
  Config2 = bdd_utils:setup_create(Config1, g(path), g(atom2), g(name2), JSON2),
  Config2;

step(Config, _Global, {step_teardown, _N, _}) -> 
  % find the node from setup and remove it
  Config2 = bdd_utils:teardown_destroy(Config, g(path), g(atom2)),
  Config1 = bdd_utils:teardown_destroy(Config2, g(path), g(atom1)),
  Config1.
