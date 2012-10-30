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
-module(users).
-export([step/3, json/3, validate/1, inspector/1, g/1]).

% Commont Routine
% Provide Feature scoped strings to DRY the code
g(Item) ->
  case Item of
    path -> "2.0/crowbar/2.0/node";
    name -> "bdd1.example.com";
    atom -> node1;
    _ -> crowbar:g(Item)
  end.
  
% Common Routine
% Makes sure that the JSON conforms to expectations (only tests deltas)
validate(JSON) ->
  try JSON of
    J -> 
        R =[bdd_utils:is_a(integer, json:keyfind(J, fingerprint)), 
            bdd_utils:is_a(boolean, json:keyfind(J, allocated)), 
            bdd_utils:is_a(string, json:keyfind(J, state)), 
            bdd_utils:is_a(boolean, json:keyfind(J,admin)), 
            bdd_utils:is_a(dbid, json:keyfind(J,os_id)), 
            crowbar_rest:validate(J)],
        bdd_utils:assert(R)
  catch
    X: Y -> io:format("ERROR: parse error ~p:~p~n", [X, Y]),
		false
	end. 

% Common Routine
% Returns list of nodes in the system to check for bad housekeeping
inspector(Config) -> 
  crowbar_rest:inspector(Config, nodes).  % shared inspector works here, but may not always
  
% Common Routine
% Creates JSON used for POST/PUT requests
json(Name, Description, Order) ->
  json:output([{"name",Name},{"description", Description}, {"order", Order}]).

% GIVEN STEP
     
step(_Config, _Global, {step_given, _N, ["there is a user",Oscar]}) -> false;

% WHEN STEP =======================================================

step(_Config, _Given, {step_when, _N, ["REST gets the user list"]}) -> false;


% Common Routine
% Validates the JSON returned by a test as part of general health tests
% Uses Feature validate, but through central routine     
step(_Config, Result, {step_then, _N, ["the node is properly formatted"]}) -> 
  crowbar_rest:step(_Config, Result, {step_then, _N, ["the", nodes, "object is properly formatted"]});

% Common Routine
% Cleans up nodes that are created during tests                         
step(Config, _Given, {step_finally, _N, ["REST removes the node",Node]}) -> 
  crowbar_rest:destroy(Config, g(path), Node);
                   
step(Config, _Global, {step_setup, _N, _}) -> 
  % create node(s) for tests
  Node = json(g(name), g(description), 100),
  crowbar_rest:create(Config, g(path), g(atom), g(name), Node);

step(Config, _Global, {step_teardown, _N, _}) -> 
  % find the node from setup and remove it
  crowbar_rest:destroy(Config, g(path), g(atom)).
