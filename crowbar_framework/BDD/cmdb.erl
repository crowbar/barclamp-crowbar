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
-module(cmdb).
-export([step/3, json/4, validate/1, inspector/1, g/1]).

% Commont Routine
% Provide Feature scoped strings to DRY the code
g(Item) ->
  case Item of
    path -> "2.0/crowbar/2.0/cmdb";
    name -> "bddcmdb";
    atom -> cmdb1;
    type -> "CmdbTest";
    _ -> crowbar:g(Item)
  end.
  
% Common Routine
% Makes sure that the JSON conforms to expectations (only tests deltas)
validate(JSON) ->
  try JSON of
    J -> 
        R =[bdd_utils:is_a("^Cmdb[A-Z][a-z0-9]*$", json:keyfind(J, type)), 
            length(J) =:= 7,
            crowbar_rest:validate(J)],
        bdd_utils:assert(R)
  catch
    X: Y -> io:format("ERROR: parse error ~p:~p~n", [X, Y]),
		false
	end. 

% Common Routine
% Creates JSON used for POST/PUT requests
json(Name, Description, Type, Order) ->
  json:output([{"name",Name},{"description", Description}, {"type", Type}, {"order", Order}]).

% Common Routine
% Returns list of nodes in the system to check for bad housekeeping
inspector(Config) -> 
  crowbar_rest:inspector(Config, cmdb).  % shared inspector works here, but may not always

step(Config, _Global, {step_given, _N, ["there is a cmdb",CMDB,"of type", Type]}) -> 
  JSON = json(CMDB, g(description), Type, 200),
  crowbar_rest:create(Config, g(path), JSON);

step(Config, _Global, {step_given, _N, ["there is a cmdb",CMDB]}) -> 
  step(Config, _Global, {step_when, _N, ["REST adds the cmdb",CMDB]});
  
step(Config, _Global, {step_given, _N, ["there is not a cmdb",CMDB]}) -> 
  false =:= step(Config, _Global, {step_given, _N, ["there is a cmdb",CMDB]});

step(Config, _Global, {step_when, _N, ["REST adds the cmdb",CMDB]}) -> 
  step(Config, _Global, {step_given, _N, ["there is a cmdb",CMDB,"of type", g(type)]});

step(Config, _Given, {step_when, _N, ["REST gets the cmdb list"]}) -> 
  bdd_restrat:step(Config, _Given, {step_when, _N, ["REST requests the",eurl:path(g(path),""),"page"]});

step(Config, _Given, {step_when, _N, ["REST gets the cmdb",Name]}) -> 
  bdd_restrat:step(Config, _Given, {step_when, _N, ["REST requests the",eurl:path(g(path),Name),"page"]});

step(Config, _Given, {step_when, _N, ["REST deletes the cmdb",CMDB]}) -> 
  crowbar_rest:destroy(Config, g(path), CMDB);

step(Config, _Result, {step_then, _N, ["there is a cmdb",CMDB]}) -> 
  ID = crowbar_rest:get_id(Config, g(path), CMDB),
  bdd_utils:log(Config, debug, "cmdb:step IS a cmdb get id returned ~p for ~p.",[ID, CMDB]),
  bdd_utils:is_a(dbid, ID);

step(Config, _Result, {step_then, _N, ["there is not a cmdb",CMDB]}) -> 
  false =:= step(Config, _Result, {step_then, _N, ["there is a cmdb",CMDB]});
       
% Common Routine
% Validates the JSON returned by a test as part of general health tests
% Uses Feature validate, but through central routine     
step(Config, Result, {step_then, _N, ["the cmdb is properly formatted"]}) -> 
  bdd_utils:log(Config, trace, "CMDB properly formatted? ~p",[Result]),
  crowbar_rest:step(Config, Result, {step_then, _N, ["the", cmdb, "object is properly formatted"]});

% Common Routine
% Cleans up nodes that are created during tests                         
step(Config, _Given, {step_finally, _N, ["REST removes the cmdb",CMDB]}) -> 
  step(Config, _Given, {step_when, _N, ["REST deletes the cmdb",CMDB]}); 
                   
step(Config, _Global, {step_setup, _N, _}) -> 
  % create node(s) for tests
  CMDB = json(g(name), g(description), g(type), 100),
  crowbar_rest:create(Config, g(path), g(atom), g(name), CMDB);

step(Config, _Global, {step_teardown, _N, _}) -> 
  % find the node from setup and remove it
  crowbar_rest:destroy(Config, g(path), g(atom)).
