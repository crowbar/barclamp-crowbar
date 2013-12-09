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
-export([pop/0, pop/1, unpop/0, g/1]).  
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
  % includes admin network setup
  bdd_utils:config_set(global_setup, dev),
  bdd_utils:config_set(inspect, false),

  % make sure background progress
  true = bdd_clirat:step([], {foo, {0,0}, ["process", "delayed","returns", "delayed_job.([0..9])"]}),

  % safety setup 
  bdd_crud:delete(node:g(path), crowbar:g(node_name)),
  Build = case file:consult(bdd_utils:config(simulator, "dev.config")) of
    {error,enoent} -> bdd_utils:log(error, dev, pop, "missing 'dev.config' initialization file", []), [];
    {ok, B} -> B
  end,

  % admin node
  Admin = crowbar:json([{name, g(node_name)}, {description, "dev" ++ g(description)}, {order, 100}, {alive, "true"}, {admin, "true"}, {bootenv, crowbar:g(bootenv)}]),
  bdd_crud:create(node:g(path), Admin, g(node_atom)),
  % admin node has to complete
  crowbar:step([], {step_given, {0, 0}, ["there are no pending Crowbar runs for",node,g(node_name)]}), 

  % turn on the delays in the test jig (the tests turn these off, simulator wants them on)
  role:step([], {step_given, {0, 1}, ["I set the",role, "test-admin", "property", "test", "to", "true"]}), 
  role:step([], {step_given, {0, 2}, ["I set the",role, "test-server", "property", "test", "to", "true"]}), 
  role:step([], {step_given, {0, 3}, ["I set the",role, "test-client", "property", "test", "to", "true"]}), 
  role:step([], {step_given, {0, 4}, ["I set the",role, "test-library", "property", "test", "to", "true"]}), 
  role:step([], {step_given, {0, 5}, ["I set the",role, "test-discovery", "property", "test", "to", "true"]}), 

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
  JSON = crowbar:json([{name, Name}, {description, Description}, {order, Order}, {alive, true}, {bootenv, node:g(bootenv)}, {group, Group}]),
  Path = bdd_restrat:alias(node, g, [path]),
  Obj = bdd_crud:read_obj(Path,Name),
  case Obj#obj.id of
    "-1" -> [_R, O] = bdd_crud:create(Path, JSON, Atom),
          % load test data
          crowbar:step([], {step_given, {0, 1}, ["test loads the","node_discovery","data into",node, Name]}),
          % user info
          bdd_utils:log(info, "Created Node ~p=~p named ~p in group ~p", [Atom, O#obj.id, Name, Group]);
    _  -> bdd_utils:config_set(Atom, Obj),
          bdd_utils:log(info, "Node ~p already exists (~p)", [Name, Obj#obj.id])
  end.


add_deployment({Atom, Name, Extras }) ->
  JSON = crowbar:json([{name, Name} | Extras]),
  Path = bdd_restrat:alias(deployment, g, [path]),
  Obj = bdd_crud:read_obj(Path,Name),
  case Obj#obj.id of
    "-1" -> [_R, O] = bdd_crud:create(Path, JSON, Atom),
          bdd_utils:log(info, "Created Deployment ~p=~p named ~p", [Atom, O#obj.id, Name]);
    _ -> bdd_utils:config_set(Atom, Obj),
          bdd_utils:log(info, "Deployment ~p (~p) already exists", [Name, Obj#obj.id])
  end.

