% Copyright 2011, Dell 
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
% Author: RobHirschfeld 
% 
-module(crowbar).
-export([step/3, validate/1, g/1]).
-import(bdd_utils).
-import(json).

g(Item) ->
  case Item of
    node_name -> "global-node.testing.com";
    node_atom -> global_node;
    description -> "BDD Testing Only - should be automatically removed";
    _ -> io:format("WARNING: Could not resolve g request for ~p (fall through catch).~n", [Item]), false
  end.

validate(JSON) ->
  try JSON of
    J ->
        {"created_at", _CreatedAt} = lists:keyfind("created_at", 1, J),
        {"description",_Description} = lists:keyfind("description", 1, J),
        {"id",Id} = lists:keyfind("id", 1, J),
        {"name",Name} = lists:keyfind("name", 1, J), 
        {"order",Order}  = lists:keyfind("order", 1, J), 
        {"updated_at",_UpdatedAt} = lists:keyfind("updated_at", 1, J), 
        R = [bdd_utils:is_a(number, Order), 
            bdd_utils:is_a(name, Name), 
            bdd_utils:is_a(number, Id)],
        case bdd_utils:assert(R)of
          true -> true;
          false -> io:format("FAIL: JSON did not comply with object format ~p~n", [JSON]), false
        end
  catch
    X: Y -> io:format("ERROR: parse error ~p:~p~n", [X, Y]),
		false
	end. 


% global setup
step(Config, _Global, {step_setup, _N, Test}) -> 
  Node = nodes:json(g(node_name), Test ++ g(description), 100),
  bdd_utils:setup_create(Config, nodes:g(path), g(node_atom), g(node_name), Node);

% find the node from setup and remove it
step(Config, _Global, {step_teardown, _N, _}) -> 
  bdd_utils:teardown_destroy(Config, nodes:g(path), g(node_atom));

% NODES 
step(Config, _Global, {step_given, _N, ["there is a node",Node]}) -> 
  JSON = nodes:json(Node, nodes:g(description), 200),
  eurl:post(Config, nodes:g(path), JSON);

step(Config, _Result, {step_then, _N, ["throw away node",Node]}) -> 
  eurl:delete(Config, nodes:g(path), Node),
  true;

% GROUPS
step(Config, _Global, {step_given, _N, ["there is a",Category,"group",Group]}) -> 
  JSON = groups:json(Group, groups:g(description), 200, Category),
  eurl:post(Config, groups:g(path), JSON);

step(Config, _Result, {step_then, _N, ["throw away group",Group]}) -> 
  eurl:delete(Config, groups:g(path), Group),
  true;

% helper for limiting checks to body
step(_Config, Result, {step_then, _N, ["I should see", Text, "in the body"]}) -> 
  bdd_webrat:step(_Config, Result, {step_then, _N, ["I should see", Text, "in section", "main_body"]});

% helper for limiting checks to body
step(_Config, Result, {step_then, _N, ["I should not see", Text, "in the body"]}) -> 
  bdd_webrat:step(_Config, Result, {step_then, _N, ["I should not see", Text, "in section", "main_body"]});


% helper for checking to make sure the ID of the object your are using it the same as the one from setup
step(Config, Results, {step_then, _N, ["key",ID,"should match",Atom,"from setup"]}) -> 
  SetupID = bdd_utils:config(Config, list_to_atom(Atom)),
  {ajax, JSON, _} = lists:keyfind(ajax, 1, Results),     % ASSUME, only 1 ajax result per feature
  SetupID =:= json:value(JSON, ID);
                                                                
% validate object based on basic rules for Crowbar
step(_Config, Result, {step_then, _N, ["the object is properly formatted"]}) -> 
  {ajax, JSON, _} = lists:keyfind(ajax, 1, Result),     % ASSUME, only 1 ajax result per feature
  crowbar:validate(JSON);
  
% validate object based on it the validate method in it's ERL file (if any)
step(_Config, Result, {step_then, _N, ["the", Type, "object is properly formatted"]}) -> 
  {ajax, JSON, _} = lists:keyfind(ajax, 1, Result),     % ASSUME, only 1 ajax result per feature
  Feature = list_to_atom(Type),
  apply(Feature, validate, [JSON]);

step(_Config, _Given, {step_when, _N, ["I have a test that is not in WebRat"]}) -> true;
                                    
step(_Config, _Result, {step_then, _N, ["I should use my special step file"]}) -> true.

  
      
