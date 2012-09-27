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
-module(crowbar_rest).
-export([step/3, g/1]).
-import(bdd_utils).
-import(json).

g(Item) ->
  case Item of
    _ -> crowbar:g(Item)
  end.

% NODES 
step(Config, _Global, {step_given, _N, ["there is a node",Node]}) -> 
  JSON = nodes:json(Node, nodes:g(description), 200),
  eurl:post(Config, nodes:g(path), JSON);

% remove the node
step(Config, _Given, {step_finally, _N, ["throw away node",Node]}) -> 
  eurl:delete(Config, nodes:g(path), Node);

% GROUPS
step(Config, _Global, {step_given, _N, ["there is a",Category,"group",Group]}) -> 
  JSON = groups:json(Group, groups:g(description), 200, Category),
  eurl:post(Config, groups:g(path), JSON);

% remove the group
step(Config, _Given, {step_finally, _N, ["throw away group",Group]}) -> 
  eurl:delete(Config, groups:g(path), Group);


% ============================  THEN STEPS =========================================

% validate object based on basic rules for Crowbar
step(_Config, Result, {step_then, _N, ["the object is properly formatted"]}) -> 
  {ajax, JSON, _} = lists:keyfind(ajax, 1, Result),     % ASSUME, only 1 ajax result per feature
  crowbar:validate(JSON);
  
% validate object based on it the validate method in it's ERL file (if any)
step(_Config, Result, {step_then, _N, ["the", Type, "object is properly formatted"]}) -> 
  {ajax, JSON, _} = lists:keyfind(ajax, 1, Result),     % ASSUME, only 1 ajax result per feature
  Feature = list_to_atom(Type),
  apply(Feature, validate, [JSON]);


% ============================  LAST RESORT =========================================
step(_Config, _Given, {step_when, _N, ["I have a test that is not in WebRat"]}) -> true;
                                    
step(_Config, _Result, {step_then, _N, ["I should use my special step file"]}) -> true.
