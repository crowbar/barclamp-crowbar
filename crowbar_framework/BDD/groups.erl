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
-module(groups).
-export([step/3, json/3, json/4, g/1, validate/1, inspector/1]).
	
	
g(Item) ->
  case Item of
    categories -> ["ui","rack","tag"];
    path -> "2.0/crowbar/2.0/group";
    name1 -> "bddthings";
    atom1 -> group1;
    name2 -> "bdddelete";
    atom2 -> group2;
    name_node1 -> "group1.node.test";
    atom_node1 -> gnode1;
    _ -> crowbar:g(Item)
  end.

validate(JSON) ->
  bdd_utils:log(trace,"groups:validate json ~p",[JSON]),
  Category = json:keyfind(JSON, category),
  R = [bdd_utils:is_a(JSON, number, order), 
       lists:member(Category,g(categories)), 
       crowbar_rest:validate(JSON)],
  bdd_utils:assert(R).

% Common Routine
% Returns list of nodes in the system to check for bad housekeeping
inspector(Config) -> 
  bdd_restrat:inspector(Config, groups).  % shared inspector works here, but may not always
  
% Returns the JSON List Nodes in the Group
get_group_nodes(Config, Group) ->
  Path = eurl:path([g(path),Group, "node"]),
  Result = eurl:get(Config, Path),
  group_nodes(json:parse(Result), Group).
  
% Given a ERLANG list from the GET, will return the node list
group_nodes(Result, Group) ->
  {"name", Group} = lists:keyfind("name", 1, Result),   % make sure this is the group we think 
  {"nodes", Nodes} = lists:keyfind("nodes", 1, Result),
  Nodes.
  
% DRY the path creation
group_node_path(Group, Node) ->  eurl:path([g(path), Group, "node", Node]).

% Build Group JSON  
json(Name, Description, Order)           -> json(Name, Description, Order, "ui").
json(Name, Description, Order, Category) ->
  json:output([{"name",Name},{"description", Description}, {"category", Category}, {"order", Order}]).
	
% STEPS!

step(Config, _Given, {step_given, _N, ["REST adds the node",Node,"to",Group]}) -> 
  step(Config, _Given, {step_when, _N, ["REST adds the node",Node,"to",Group]});
  
step(Config, _Given, {step_when, _N, ["REST gets the group list"]}) -> 
  bdd_restrat:step(Config, _Given, {step_when, _N, ["REST requests the",eurl:path(g(path),""),"page"]});

step(_Config, _Given, {step_when, _N, ["AJAX gets the group",Name]}) -> 
  bdd_webrat:step(_Config, _Given, {step_when, _N, ["AJAX requests the",eurl:path(g(path),Name),"page"]});
      
step(Config, _Given, {step_when, _N, ["REST adds the node",Node,"to",Group]}) -> 
  R = eurl:post(Config, group_node_path(Group, Node), []),
  Result = json:parse(R),
  bdd_utils:log(Config, trace, "group:step REST add the node ~p to ~p. Result was ~p",[Node, Group, Result]),
  {nodes, group_nodes(Result, Group)};

step(Config, _Given, {step_when, _N, ["REST removes the node",Node,"from",Group]}) -> 
  eurl:delete(Config, group_node_path(Group, Node), []);

step(Config, _Given, {step_when, _N, ["REST removes the group",Group]}) ->   
  eurl:delete(Config, g(path), Group);

step(Config, _Given, {step_when, _N, ["REST moves the node",Node,"from",GroupFrom,"to",GroupTo]}) -> 
  %first check old group
  Nodes = get_group_nodes(Config, GroupFrom),
  1 = length([ N || {_ID, N} <- Nodes, N =:= Node]),
  R = eurl:put(Config, group_node_path(GroupTo, Node), []),
  Result = json:parse(R),
  bdd_utils:log(Config, trace, "group:step REST move the node ~p to ~p. Result was ~p",[Node, GroupTo, Result]),
  {nodes, group_nodes(Result, GroupTo)}; 
  
step(_Config, Result, {step_then, _N, ["the group is properly formatted"]}) -> 
  bdd_restrat:step(_Config, Result, {step_then, _N, ["the", groups, "object is properly formatted"]});

step(Config, _Result, {step_then, _N, ["there is not a",_Category,"group",Group]}) -> 
  % WARNING - this IGNORES THE CATEGORY, it is not really a true test for the step.
  case bdd_restrat:get_id(Config, g(path), Group) of
    "-1" -> true;
    ID -> true =/= is_number(ID)
  end;

step(Config, _Result, {step_then, _N, ["the group",Group,"should have at least",Count,"node"]}) -> 
  {C, _} = string:to_integer(Count),
  Nodes = get_group_nodes(Config, Group),
  Items = length(Nodes),
  Items >= C;

step(Config, _Result, {step_then, _N, ["the group",Group,"should have",Count,"nodes"]}) -> 
  {C, _} = string:to_integer(Count),
  Nodes = get_group_nodes(Config, Group),
  length(Nodes) =:= C;

step(Config, _Result, {step_then, _N, ["the node",Node,"should be in group",Group]}) -> 
  Nodes = get_group_nodes(Config, Group),
  Name = [ N || {_ID, N} <- Nodes, N =:= Node],
  length(Name) =:= 1;

step(Config, _Result, {step_then, _N, ["the node",Node,"should not be in group",Group]}) -> 
  Nodes = get_group_nodes(Config, Group),
  Name = [ N || {_ID, N} <- Nodes, N =:= Node],
  length(Name) =:= 0;

step(Config, _Given, {step_finally, _N, ["REST removes the node",Node,"from",Group]}) -> 
  step(Config, _Given, {step_when, _N, ["REST removes the node",Node,"from",Group]});
                
step(Config, _Global, {step_setup, _N, _}) -> 
  % create node(s) for tests
  JSON0 = node:json(g(name_node1), g(description), 100),
  Config0 = bdd_restrat:create(Config, node:g(path), g(atom_node1), name, JSON0),
  % create groups(s) for tests
  JSON1 = json(g(name1), g(description), 100),
  Config1 = bdd_restrat:create(Config0, g(path), g(atom1), name, JSON1),
  JSON2 = json(g(name2), g(description), 200),
  Config2 = bdd_restrat:create(Config1, g(path), g(atom2), name, JSON2),
  Config2;

step(Config, _Global, {step_teardown, _N, _}) -> 
  % find the node from setup and remove it
  Config2 = bdd_restrat:destroy(Config, g(path), g(atom2)),
  Config1 = bdd_restrat:destroy(Config2, g(path), g(atom1)),
  Config0 = bdd_restrat:destroy(Config1, node:g(path), g(atom_node1)),
  Config0.
