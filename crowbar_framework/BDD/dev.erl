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

-module(dev).
-export([pop/0, pop/1, unpop/0]).  
-import(bdd_utils).
-import(digest_auth).

% create a base system
pop()           -> pop(default).
pop(ConfigRaw) ->
  bdd:start(ConfigRaw),
  bdd_utils:config_set(global_setup, dev),
  bdd_utils:config_set(inspect, false),
  {ok, Build} = file:consult(bdd_utils:config(simulator, "dev.config")),
  [ add_group(G) || G <- buildlist(Build, groups) ],
  [ add_attribute(A) || A <- buildlist(Build, attributes) ],
  [ add_node(N) || N <- buildlist(Build, nodes) ],
  bdd_utils:config_unset(global_setup),
  bdd_utils:config_set(inspect, true),
  io:format("Done.~n").

% tear it down
unpop()       ->  
  {ok, Build} = file:consult(bdd_utils:config(simulator, "dev.config")),
  [ remove(nodes, N) || {N, _, _, _, _} <- buildlist(Build, nodes) ], 
  [ remove(attribute, A) || {A, _, _, _, _} <- buildlist(Build, attributes) ], 
  [ remove(groups, G) || {G, _, _, _} <- buildlist(Build, groups) ],
  bdd:stop([]). 

buildlist(Source, Type) ->
  {Type, R} = lists:keyfind(Type, 1, Source),
  R.

remove(Type, Atom) ->
  crowbar_rest:destroy([], apply(Type, g, [path]), Atom),
  bdd_utils:config_unset(Atom),
  bdd_utils:log(info, "Removed ~p with tag ~p", [Type, Atom]).

add_group({Atom, Name, Descripton, Order}) ->
  JSON = groups:json(Name, Descripton, Order, 'ui'),
  Path = apply(groups, g, [path]),
  Result = json:parse(bdd_restrat:create([], Path, JSON)),
  Key = json:keyfind(Result, id),
  bdd_utils:config_set(Atom, Key),
  bdd_utils:log(info, "Created Group ~p=~p named ~p", [Atom, Key, Name]).

add_attribute({Atom, Name, Description, Order, _Type}) ->
  JSON = attribute:json(Name, Description, Order),
  Path = apply(attribute, g, [path]),
  Result = json:parse(bdd_restrat:create([], Path, JSON)),
  Key = json:keyfind(Result, id),
  bdd_utils:config_set(Atom, Key),
  bdd_utils:log(info, "Created Attribute ~p=~p named ~p", [Atom, Key, Name]).

add_node({Atom, Name, Description, Order, Group}) ->
  JSON = nodes:json(Name, Description, Order),
  Path = apply(nodes, g, [path]),
  Result = json:parse(bdd_restrat:create([], Path, JSON)),
  Key = json:keyfind(Result, id),
  groups:step([], [], {step_when, 0, ["REST adds the node",Name,"to",Group]}),
  bdd_utils:config_set(Atom, Key),
  bdd_utils:log(info, "Created Node ~p=~p named ~p in group ~p", [Atom, Key, Name, Group]).

  