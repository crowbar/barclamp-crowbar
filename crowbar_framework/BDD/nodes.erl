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
-export([step/3, json/3, validate/1, inspector/1, g/1]).

% DEPRICATED!!! MOVE TO NODE.

% Commont Routine
% Provide Feature scoped strings to DRY the code
g(Item) ->
  bdd_utils:log(depricate, "DEPRICATED: Please use node instead of nodeS for this call ~p",[g]),
  node:g(Item).
  
% Common Routine
% Makes sure that the JSON conforms to expectations (only tests deltas)
validate(JSON) ->
  bdd_utils:log(depricate, "DEPRICATED: Please use node instead of nodeS for this call ~p",[validate]),
  node:validate(JSON).

% Common Routine
% Returns list of nodes in the system to check for bad housekeeping
inspector(Config) -> 
  bdd_utils:log(depricate, "DEPRICATED: Please use node instead of nodeS for this call ~p",[inspector]),
  node:inspector(Config).
  
% Common Routine
% Creates JSON used for POST/PUT requests
json(Name, Description, Order) ->
  bdd_utils:log(depricate, "DEPRICATED: Please use node instead of nodeS for this call ~p",[json]),
  node:json(Name, Description, Order).  
step(Config, Input, Step) ->
  bdd_utils:log(depricate, "DEPRICATED: Please use node instead of nodeS for this call ~p: ~p",[step, Step]),
  node:step(Config, Input, Step).
