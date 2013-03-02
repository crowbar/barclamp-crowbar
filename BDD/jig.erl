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
-module(jig).
-export([step/3, json/4, validate/1, inspector/1, g/1]).
-include("bdd.hrl").

% Commont Routine
% Provide Feature scoped strings to DRY the code
g(Item) ->
  case Item of
    path -> "api/v2/jigs";
    name -> "bddjig";
    atom -> jig1;
    type -> "BarclampCrowbar::Jig";
    node_atom -> "global-node.testing.com";
    _ -> crowbar:g(Item)
  end.
  
% Common Routine
% Makes sure that the JSON conforms to expectations (only tests deltas)
validate(J) ->
  Wrapper = crowbar_rest:api_wrapper(J),
  JSON = Wrapper#item.data,
  R =[Wrapper#item.type == jig,
      bdd_utils:is_a(JSON, "^Barclamp[A-Z][A-Za-z0-9]*::Jig$", type), 
      bdd_utils:is_a(JSON, length, 7),
      crowbar_rest:validate(JSON)],
  bdd_utils:assert(R). 

% Common Routine
% Creates JSON used for POST/PUT requests
json(Name, Description, Type, Order) ->
  json:output([{"name",Name},{"description", Description}, {"type", Type}, {"order", Order}]).

% Common Routine
% Returns list of nodes in the system to check for bad housekeeping
inspector(Config) -> 
  bdd_restrat:inspector(Config, jig).  % shared inspector works here, but may not always

step(Config, _Global, {step_given, _N, ["there is a jig",Jig,"of type", Type]}) -> 
  JSON = json(Jig, g(description), Type, 200),
  bdd_restrat:create(Config, g(path), JSON);

step(Config, _Global, {step_given, _N, ["there is a jig",Jig]}) -> 
  step(Config, _Global, {step_when, _N, ["REST adds the jig",Jig]});
  
step(Config, _Global, {step_given, _N, ["there is not a jig",Jig]}) -> 
  false =:= step(Config, _Global, {step_given, _N, ["there is a jig",Jig]});

step(Config, _Global, {step_when, _N, ["REST adds the jig",Jig]}) -> 
  step(Config, _Global, {step_given, _N, ["there is a jig",Jig,"of type", g(type)]});


step(Config, _Given, {step_when, _N, ["REST gets the jig",Name]}) -> 
  bdd_restrat:step(Config, _Given, {step_when, _N, ["REST requests the",eurl:path(g(path),Name),"page"]});

step(Config, _Given, {step_when, _N, ["REST deletes the jig",Jig]}) -> 
  bdd_restrat:destroy(Config, g(path), Jig);

step(Config, _Result, {step_then, _N, ["there is a jig",Jig]}) -> 
  ID = bdd_restrat:get_id(Config, g(path), Jig),
  bdd_utils:log(Config, debug, "jig:step IS a jig get id returned ~p for ~p.",[ID, Jig]),
  bdd_utils:is_a(dbid, ID);

step(Config, _Result, {step_then, _N, ["there is not a jig",Jig]}) -> 
  false =:= step(Config, _Result, {step_then, _N, ["there is a jig",Jig]});
       
% Common Routine
% Validates the JSON returned by a test as part of general health tests
% Uses Feature validate, but through central routine     
step(Config, Result, {step_then, _N, ["the jig is properly formatted"]}) ->  	
  bdd_utils:log(Config, trace, "Jig properly formatted? ~p",[Result]),
  crowbar_rest:step(Config, Result, {step_then, _N, ["the", jig, "object is properly formatted"]});

% Common Routine
% Cleans up nodes that are created during tests                         
step(Config, _Given, {step_finally, _N, ["REST removes the jig",Jig]}) -> 
  step(Config, _Given, {step_when, _N, ["REST deletes the jig",Jig]}); 
                   
step(Config, _Global, {step_setup, _N, _}) -> 
  bdd_utils:log(debug, "jig:step Setup running",[]),
  % create Jig entry
  Jig = json(g(name), g(description), g(type), 100),
  bdd_restrat:create(Config, g(path), g(atom), name, Jig);

step(Config, _Global, {step_teardown, _N, _}) -> 
  bdd_util:log(debug, "jig:step Teardown running",[]),
  % remove jig entry
  bdd_restrat:destroy(Config, g(path), g(atom)).
