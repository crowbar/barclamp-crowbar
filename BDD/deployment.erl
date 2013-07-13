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
-module(deployment).
-export([step/2, json/3, validate/1, inspector/1, g/1, create/3]).
-include("bdd.hrl").

% Commont Routine
% Provide Feature scoped strings to DRY the code
g(Item) ->
  case Item of
    path -> "api/v2/deployments";
    resource -> "deployments";
    _ -> crowbar:g(Item)
  end.

% Common Routine
% Makes sure that the JSON conforms to expectations (only tests deltas)
validate(JSON) when is_record(JSON, obj) ->
  J = JSON#obj.data,
  R =[JSON#obj.type == "deployment",
      bdd_utils:is_a(J, length, 10),
      bdd_utils:is_a(J, dbid, committed_snapshot_id),
      bdd_utils:is_a(J, dbid, active_snapshot_id),
      bdd_utils:is_a(J, dbid, proposed_snapshot_id),
      crowbar_rest:validate(J)],
  bdd_utils:assert(R);
validate(JSON) -> 
  bdd_utils:log(error, deployment, validate, "requires #obj record. Got ~p", [JSON]), 
  false.

create(ID, Name, Extras) ->
  % for now, we are ignoring the extras
  JSON = json(Name, 
              proplists:get_value(description, Extras, g(description)), 
              proplists:get_value(order, Extras, g(order))),
  bdd_restrat:create(ID, node, g(path), Name, JSON).
  
% Common Routine
% Returns list of nodes in the system to check for bad housekeeping
inspector(Deployment) -> 
  bdd_restrat:inspector(Deployment, deployment).  % shared inspector works here, but may not always

% Common Routine
% Creates JSON used for POST/PUT requests
json(Name, Description, Order) ->
  json:output([{"name",Name},{"description", Description}, {"order", Order}]).

% TEMPORARY REMAPPING
% -include("bdd.hrl").
step(In, Out) -> step([], In, Out).


step(_C, _G, {step_given, {_S, _N}, ["I propose a",deployment,Deployment,"on the",barclamp,Barclamp]})  -> step(_C, _G, {step_when, {_S, _N}, ["I propose a",deployment,Deployment,"on the",barclamp,Barclamp]});

step(_, _, {step_when, {_S, _N}, ["I propose a",deployment,Deployment,"on the",barclamp,Barclamp]}) ->
  Path = eurl:path(["api","v2","barclamps",Barclamp,"deployments"]),
  JSON = json(Deployment, g(description), g(order)),
  PutPostResult = eurl:put_post([], Path, JSON, post, all),
  {Code, Result} = PutPostResult,
  bdd_utils:log(debug, deployment, step, "deploy from barclamp ~p named ~p got ~p", [Path, JSON, Code]),
  bdd_restrat:ajax_return(Path, post, Code, Result);

step(_Deployment, _Result, {_Type, _N, ["END OF CONFIG"]}) ->
  false.
