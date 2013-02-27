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
-module(groups).
-export([step/3, json/3, json/4, g/1, validate/1, inspector/1]).
	
	
g(Item) ->
  bdd_utils:depricate({2013,6,1}, groups, g, group_cb, g, [Item]).

validate(JSON) ->
  bdd_utils:depricate({2013,6,1}, groups, validate, group_cb, validate, [JSON]).

% Common Routine
% Returns list of nodes in the system to check for bad housekeeping
inspector(Config) -> 
    bdd_utils:depricate({2013,6,1}, groups, inspector, group_cb, inspector, [Config]).


% Build Group JSON  
json(Name, Description, Order)           -> 
    bdd_utils:depricate({2013,6,1}, groups, json, group_cb, json, [Name, Description, Order]).
    
json(Name, Description, Order, Category) ->
  bdd_utils:depricate({2013,6,1}, groups, json, group_cb, json, [Name, Description, Order, Category]).
  	
% STEPS!

step(Config, Given, {Type, N, Action}) -> 
  bdd_utils:depricate({2013,6,1}, groups, step, group_cb, step, [Config, Given, {Type, N, Action}]).
