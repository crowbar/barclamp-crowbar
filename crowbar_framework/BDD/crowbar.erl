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
-export([step/3, g/1]).
-import(bdd_utils).
-import(json).

g(Item) ->
  case Item of
    node_name -> "global-node.testing.com";
    node_atom -> global_node
  end.

% node setup
step(Config, _Global, {step_setup, _N, Test}) -> 
  Node = nodes:json(g(node_name), Test ++ " BDD Testing Only - should be automatically removed", 100),
  bdd_utils:setup_create(Config, nodes:g(path), g(node_atom), g(node_name), Node);

% find the node from setup and remove it
step(Config, _Global, {step_teardown, _N, _}) -> 
  bdd_utils:teardown_destroy(Config, nodes:g(path), g(node_atom));
  
% helper for checking to make sure the ID of the object your are using it the same as the one from setup
step(Config, Results, {step_then, _N, ["key",ID,"should match",Atom,"from setup"]}) -> 
  SetupID = bdd_utils:config(Config, list_to_atom(Atom)),
  {ajax, JSON, _} = lists:keyfind(ajax, 1, Results),     % ASSUME, only 1 ajax result per feature
  SetupID =:= json:value(JSON, ID);
                                                              
% combine multiple steps into a single event
%step(_Config, _Result, {step_then, _N, ["the object should comply with API rules"]}) -> false;
step(Config, Result, {step_then, N, ["the object should comply with API rules"]}) -> 
  R = [bdd_webrat:step(Config, Result, {step_then, N, ["And there should be a key","id"]}),
       bdd_webrat:step(Config, Result, {step_then, N, ["And key", "id", "should be a number"]}),
       bdd_webrat:step(Config, Result, {step_then, N, ["And there should be a key","order"]}),
       bdd_webrat:step(Config, Result, {step_then, N, ["And key", "order", "should be a number"]}),
       bdd_webrat:step(Config, Result, {step_then, N, ["And there should be a key","updated_at"]})],
  case R of 
    [false, _, _, _, _] -> io:format("FAILED: API rules check: id exists.");
    [_, false, _, _, _] -> io:format("FAILED: API rules check: id number.");
    [_, _, false, _, _] -> io:format("FAILED: API rules check: order exists.");
    [_, _, _, false, _] -> io:format("FAILED: API rules check: order number.");
    [_, _, _, _, false] -> io:format("FAILED: API rules check: updated_at exists.")
  end,
  bdd_utils:assert(R);

step(_Config, _Given, {step_when, _N, ["I have a test that is not in WebRat"]}) -> true;
                                    
step(_Config, _Result, {step_then, _N, ["I should use my special step file"]}) -> true.

  
      
