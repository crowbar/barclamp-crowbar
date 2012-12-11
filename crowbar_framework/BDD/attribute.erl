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
-module(attribute).
-export([step/3, json/3, validate/1, inspector/1, g/1]).

% Commont Routine
% Provide Feature scoped strings to DRY the code
g(Item) ->
  case Item of
    path -> "2.0/crowbar/2.0/attribute";
    name -> "bddattribute";
    atom -> attrib1;
    _ -> crowbar:g(Item)
  end.
  
% Common Routine
% Makes sure that the JSON conforms to expectations (only tests deltas)
validate(J) ->
  R =[true,   % placeholder 
      length(J) =:= 6,
      crowbar_rest:validate(J)],
  bdd_utils:assert(R). 

% Common Routine
% Creates JSON used for POST/PUT requests
json(Name, Description, Order) ->
  json:output([{"name",Name},{"description", Description}, {"order", Order}]).

% Common Routine
% Returns list of nodes in the system to check for bad housekeeping
inspector(Config) -> 
  crowbar_rest:inspector(Config, attribute).  % shared inspector works here, but may not always

step(Config, _Global, {step_given, _N, ["there is an attribute",Attribute]}) -> 
  JSON = json(Attribute, g(description), 200),
  crowbar_rest:create(Config, g(path), JSON);
  
step(Config, _Global, {step_given, _N, ["there is not an attribute",Attribute]}) -> 
  crowbar_rest:destroy(Config, g(path), Attribute);

step(Config, _Global, {step_when, _N, ["REST adds the attribute",Attribute]}) -> 
  step(Config, _Global, {step_given, _N, ["there is an attribute",Attribute]});

step(Config, _Given, {step_when, _N, ["REST gets the attribute list"]}) -> 
  bdd_restrat:step(Config, _Given, {step_when, _N, ["REST requests the",eurl:path(g(path),""),"page"]});

step(Config, _Given, {step_when, _N, ["REST gets the attribute",Name]}) -> 
  bdd_restrat:step(Config, _Given, {step_when, _N, ["REST requests the",eurl:path(g(path),Name),"page"]});

step(Config, _Given, {step_when, _N, ["REST deletes the attribute",Attribute]}) -> 
  crowbar_rest:destroy(Config, g(path), Attribute);

step(Config, _Result, {step_then, _N, ["there is an attribute",Attribute]}) -> 
  ID = crowbar_rest:get_id(Config, g(path), Attribute),
  bdd_utils:log(Config, debug, "attribute:step IS a attribute get id returned ~p for ~p.",[ID, Attribute]),
  bdd_utils:is_a(dbid, ID);

step(Config, _Result, {step_then, _N, ["there is not an attribute",Attribute]}) -> 
  false =:= step(Config, _Result, {step_then, _N, ["there is an attribute",Attribute]});
       
% Common Routine
% Validates the JSON returned by a test as part of general health tests
% Uses Feature validate, but through central routine     
step(Config, Result, {step_then, _N, ["the attribute is properly formatted"]}) -> 
  bdd_utils:log(Config, trace, "attribute properly formatted? ~p",[Result]),
  crowbar_rest:step(Config, Result, {step_then, _N, ["the", attribute, "object is properly formatted"]});

% Common Routine
% Cleans up nodes that are created during tests                         
step(Config, _Given, {step_finally, _N, ["REST removes the attribute",Attribute]}) -> 
  step(Config, _Given, {step_when, _N, ["REST deletes the attribute",Attribute]}); 
                   
step(Config, _Global, {step_setup, _N, _}) -> Config;

step(Config, _Global, {step_teardown, _N, _}) -> Config.
