% Copyright 2013, Dell 
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

g(Item)                        -> bdd_utils:depricate({2013, 5,1}, nodes, g, node, g,[Item]).
validate(JSON)                 -> bdd_utils:depricate({2013, 5,1}, nodes, validate, node, validate,[JSON]).
inspector(Config)              -> bdd_utils:depricate({2013, 5,1}, nodes, inspector, node, inspector,[Config]).
json(Name, Description, Order) -> bdd_utils:depricate({2013, 5,1}, nodes, json, node, json,[Name, Description, Order]).
step(Config, Input, Step)      -> bdd_utils:depricate({2013, 5,1}, nodes, step, node, step,[Config, Input, Step]).
