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
-module(attrib).
-export([step/2, json/3, validate/1, inspector/0, g/1]).
-include("bdd.hrl").

% Commont Routine
% Provide Feature scoped strings to DRY the code
g(Item) ->
  case Item of
    path  -> "api/v2/attribs";
    name  -> "bddattribute";
    atom  -> attrib1;
    map   -> 'bdd/rocks';
    value -> 'bravo';
    _ -> crowbar:g(Item)
  end.
  
% Common Routine
% Makes sure that the JSON conforms to expectations (only tests deltas)
validate(JSON) when is_record(JSON, obj) ->
  J = JSON#obj.data,
  R =[JSON#obj.type == "attrib",
      bdd_utils:is_a(J, string, map), 
      bdd_utils:is_a(J, dbid, role_id), 
      bdd_utils:is_a(J, dbid, barclamp_id), 
      bdd_utils:is_a(J, length, 9),
      crowbar_rest:validate(J)],
  bdd_utils:assert(R).
  
% Common Routine
% Creates JSON used for POST/PUT requests
json(Name, Description, Order) -> 
  crowbar:json([{name, Name}, {description, Description}, {order, Order}]).

% Common Routine
% Returns list of nodes in the system to check for bad housekeeping
inspector() -> 
  bdd_restrat:inspector(attrib).  % shared inspector works here, but may not always

% Common Routine

step(_Global, {step_given, {Scenario, _N}, ["REST creates the", attrib, Name, "with map",Map]}) -> 
  JSON = crowbar:json([{name, Name}, {description, g(description)}, {order, g(order)}, {map, Map}]),
  bdd_restrat:create(g(path), JSON, attrib, Scenario);

step(_Given, {step_when, {_Scenario, _N}, ["REST sets the", Attrib, "on", Node, "to", Value]}) -> 
  Path = eurl:path([node:g(path), Node, "attribs", Attrib]),
  % this ASSUMES that the Value is valid JSON
  JSON = crowbar:json([{value, Value}]),
  bdd_utils:log(debug, attrib, step, "~p PUT ~p", [Path, JSON]),
  % now update 
  Result = eurl:put_post(Path, JSON, put),
  O = bdd_restrat:get_object(Result),
  [Result, O];

step(_Global, {step_given, {_Scenario, _N}, ["REST sets the discovery on",Node, "to", JSON]}) -> 
  Path = eurl:path([node:g(path), Node]),
  % this ASSUMES that the Value is valid JSON
  J = crowbar:json([{discovery, json:parse(JSON)}]),
  bdd_utils:log(debug, attrib, step, "~p PUT ~p", [Path, J]),
  % now update 
  Result = eurl:put_post(Path, J, put),
  O = bdd_restrat:get_object(Result),
  [Result, O];

step(_Global, {step_setup, _N, _}) -> true;

step(_Global, {step_teardown, _N, _}) -> true.
