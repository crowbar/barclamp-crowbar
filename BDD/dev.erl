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

-module(dev).
-export([pop/0, pop/1, unpop/0]).  
-import(bdd_utils).
-import(digest_auth).
-include("bdd.hrl").

g(Item)         -> 
  case Item of
    node_name -> "sim.cr0wbar.com";
    node_atom -> "sim_admin";
    _ -> crowbar:g(Item)
  end.

% create a base system
pop()           -> pop(default).
pop(ConfigRaw)  ->
  bdd:start(ConfigRaw),
  bdd_utils:config_set(global_setup, dev),
  bdd_utils:config_set(inspect, false),
  % safety setup 
  bdd_crud:delete(node:g(path), crowbar:g(node_name)),
  {ok, Build} = file:consult(bdd_utils:config(simulator, "dev.config")),
  % admin network
  network:make_admin(),
  % admin node
  Admin = crowbar:json([{name, g(node_name)}, {description, "dev" ++ g(description)}, {order, 100}, {alive, true}, {admin, "true"}]),
  bdd_crud:create(node:g(path), Admin, g(node_atom)),
  % rest of the nodes
  [ add_deployment(D) || D <- buildlist(Build, deployments) ],
  [ add_node(N) || N <- buildlist(Build, nodes) ],
  bdd_utils:config_unset(global_setup),
  bdd_utils:config_set(inspect, true),
  io:format("Done.~n").

% tear it down
unpop()       ->  
  {ok, Build} = file:consult(bdd_utils:config(simulator, "dev.config")),
  [ remove(N) || {N, _, _, _, _} <- buildlist(Build, nodes) ], 
  bdd_crud:delete(node:g(path), g(node_name)),
  bdd:stop([]). 

buildlist(Source, Type) ->
  {Type, R} = lists:keyfind(Type, 1, Source),
  R.

remove(Atom) ->
  bdd_crud:delete(Atom).

add_node({Atom, Name, Description, Order, Group}) ->
  JSON = crowbar:json([{name, Name}, {description, Description}, {order, Order}, {alive, true}, {group, Group}]),
  Path = bdd_restrat:alias(node, g, [path]),
  [_R, O] = bdd_crud:create(Path, JSON, Atom),
  bdd_utils:log(info, "Created Node ~p=~p named ~p in group ~p", [Atom, O#obj.id, Name, Group]).

add_deployment({Atom, Name, Extras }) ->
  JSON = crowbar:json([{name, Name} | Extras]),
  Path = bdd_restrat:alias(deployment, g, [path]),
  [_R, O] = bdd_crud:create(Path, JSON, Atom),
  bdd_utils:log(info, "Created Deployment ~p=~p named ~p", [Atom, O#obj.id, Name]).

