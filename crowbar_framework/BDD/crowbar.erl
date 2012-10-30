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
-import(crowbar_rest).
-import(json).

g(Item) ->
  case Item of
    node_name -> "global-node.testing.com";
    node_atom -> global_node;
    description -> "BDD Testing Only - should be automatically removed";
    _ -> io:format("WARNING: Could not resolve g request for ~p (fall through catch).~n", [Item]), false
  end.

% MOVED! DELETE AFTER 12/12/12 helper common to all setups using REST  
validate(JSON) ->
  io:format("** PLEASE MOVE ** validate moved from crowbar to crowbar_rest:json."),
  crowbar_rest:validate(JSON).

% global setup
step(Config, _Global, {step_setup, _N, Test}) -> 
  Node = nodes:json(g(node_name), Test ++ g(description), 100),
  crowbar_rest:create(Config, nodes:g(path), g(node_atom), g(node_name), Node);

% find the node from setup and remove it
step(Config, _Global, {step_teardown, _N, _}) -> 
  crowbar_rest:destroy(Config, nodes:g(path), g(node_atom));


% ============================  THEN STEPS =========================================

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

% ============================  LAST RESORT =========================================
step(_Config, _Given, {step_when, _N, ["I have a test that is not in WebRat"]}) -> true;
                                    
step(_Config, _Result, {step_then, _N, ["I should use my special step file"]}) -> true.
