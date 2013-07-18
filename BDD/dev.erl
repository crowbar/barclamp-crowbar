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

% create a base system
pop()           -> pop(default).
pop(ConfigRaw) ->
  bdd:start(ConfigRaw),
  bdd_utils:config_set(global_setup, dev),
  bdd_utils:config_set(inspect, false),
  {ok, Build} = file:consult(bdd_utils:config(simulator, "dev.config")),
  [ add_node(N) || N <- buildlist(Build, nodes) ],
  bdd_utils:config_unset(global_setup),
  bdd_utils:config_set(inspect, true),
  io:format("Done.~n").

% tear it down
unpop()       ->  
  {ok, Build} = file:consult(bdd_utils:config(simulator, "dev.config")),
  [ remove(N) || {N, _, _, _, _} <- buildlist(Build, nodes) ], 
  bdd:stop([]). 

buildlist(Source, Type) ->
  {Type, R} = lists:keyfind(Type, 1, Source),
  R.

remove(Atom) ->
  bdd_crud:delete(Atom).

add_node({Atom, Name, Description, Order, Group}) ->
  JSON = node:json(Name, Description, Order),
  Path = bdd_restrat:alias(node, g, [path]),
  [_R, O] = bdd_crud:create(Path, JSON, Atom),
  bdd_utils:log(info, "Created Node ~p=~p named ~p in group ~p", [Atom, O#obj.id, Name, Group]).

