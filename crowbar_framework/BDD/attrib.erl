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
-export([step/3, json/3, validate/1, inspector/1, g/1, create/3]).

% Commont Routine
% Provide Feature scoped strings to DRY the code
g(Item) ->
  case Item of
    path -> "2.0/crowbar/2.0/attribs";
    name -> "bddattribute";
    atom -> attrib1;
    _ -> crowbar:g(Item)
  end.
  
% Common Routine
% Makes sure that the JSON conforms to expectations (only tests deltas)
validate(J) ->
  R =[bdd_utils:is_a(J, length, 6),
      crowbar_rest:validate(J)],
  bdd_utils:assert(R). 
  
% Common Routine
% Creates JSON used for POST/PUT requests
json(Name, Description, Order) ->
  json:output([{"name",Name},{"description", Description}, {"order", Order}]).

create(ID, Name, Extras) ->
  % for now, we are ignoring the extras
  JSON = json(Name, 
              proplists:get_value(description, Extras, g(description)), 
              proplists:get_value(order, Extras, g(order))),
  bdd_restrat:create(ID, attrib, g(path), Name, JSON).

% Common Routine
% Returns list of nodes in the system to check for bad housekeeping
inspector(Config) -> 
  crowbar_rest:inspector(Config, attrib).  % shared inspector works here, but may not always

step(Config, _Global, {step_given, _N, ["there is an attribute",Attribute]}) -> 
  bdd:log(depricate, "Replace Attrib:'there is an attribute' step with generic from bdd_restrat for ~p", [Attribute]),
  JSON = json(Attribute, g(description), 200),
  bdd_restrat:create(Config, g(path), JSON);
  
step(Config, _Global, {step_given, _N, ["there is not an attribute",Attribute]}) -> 
  bdd:log(depricate, "Replace Attrib:'there is not an attribute' step with generic from bdd_restrat for ~p", [Attribute]),
  bdd_restrat:destroy(Config, g(path), Attribute);

step(Config, _Global, {step_when, _N, ["REST adds the attribute",Attribute]}) -> 
  bdd:log(depricate, "Replace Attrib:'REST adds the attribute' step with generic from bdd_restrat for ~p", [Attribute]),
  step(Config, _Global, {step_given, _N, ["there is an attribute",Attribute]});

step(Config, _Given, {step_when, _N, ["REST gets the attribute list"]}) -> 
  bdd:log(depricate, "Replace Attrib:'REST gets the attribute list' step with generic from bdd_restrat", []),
  bdd_restrat:step(Config, _Given, {step_when, _N, ["REST requests the",eurl:path(g(path),""),"page"]});

step(Config, _Given, {step_when, _N, ["REST gets the attribute",Name]}) -> 
  bdd:log(depricate, "Replace Attrib:'REST gets the attribute' step with generic from bdd_restrat for ~p", [Name]),
  bdd_restrat:step(Config, _Given, {step_when, _N, ["REST requests the",eurl:path(g(path),Name),"page"]});

step(Config, _Given, {step_when, _N, ["REST deletes the attribute",Attribute]}) -> 
  bdd:log(depricate, "Replace Attrib:'REST deletes the attribute list' step with generic from bdd_restrat for ~p", [Attribute]),
  bdd_restrat:destroy(Config, g(path), Attribute);

step(Config, _Result, {step_then, _N, ["there is an attribute",Attribute]}) -> 
  ID = bdd_restrat:get_id(Config, g(path), Attribute),
  bdd_utils:log(Config, debug, "attribute:step IS a attribute get id returned ~p for ~p.",[ID, Attribute]),
  bdd_utils:is_a(dbid, ID);

step(Config, _Result, {step_then, _N, ["there is not an attribute",Attribute]}) -> 
  false =:= step(Config, _Result, {step_then, _N, ["there is an attribute",Attribute]});
       

% Common Routine
% Cleans up nodes that are created during tests                         
step(Config, _Given, {step_finally, _N, ["REST removes the attribute",Attribute]}) -> 
  bdd:log(depricate, "Replace Attrib:'REST removes the attribute' step with generic from bdd_restrat for ~p", [Attribute]),
  step(Config, _Given, {step_when, _N, ["REST deletes the attribute",Attribute]});
                   
step(Config, _Global, {step_setup, _N, _}) -> Config;

step(Config, _Global, {step_teardown, _N, _}) -> Config.
