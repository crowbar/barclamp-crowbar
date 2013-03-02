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
-module(snapshot).
-export([step/3, json/3, path/1, validate/1, inspector/1, g/1, create/3]).

% Commont Routine
% Provide Feature scoped strings to DRY the code
g(Item) ->
  case Item of
    resource_path -> "/v2/snapshots";
    path -> "crowbar" ++ g(resource_path);
    _ -> crowbar:g(Item)
  end.
  
path(Barclamp) -> Barclamp ++ g(resource_path).

% Common Routine
% Makes sure that the JSON conforms to expectations (only tests deltas)
validate(J) ->
  R =[bdd_utils:is_a(J, length, 12),
      crowbar_rest:validate(J)],
  bdd_utils:assert(R).

create(ID, Name, Extras) ->
  % for now, we are ignoring the extras
  JSON = json(Name, 
              proplists:get_value(description, Extras, g(description)), 
              proplists:get_value(order, Extras, g(order))),
  bdd_restrat:create(ID, node, g(path), Name, JSON).
  
% Common Routine
% Returns list of nodes in the system to check for bad housekeeping
inspector(Config) -> 
  bdd_restrat:inspector(Config, snapshot).  % shared inspector works here, but may not always

% Common Routine
% Creates JSON used for POST/PUT requests
json(Name, Description, Order) ->
  json:output([{"name",Name},{"description", Description}, {"order", Order}]).

                   
step(Config, _Global, {step_setup, _N, _}) -> Config;

step(_Config, _Global, {step_teardown, _N, _}) -> true.
  
