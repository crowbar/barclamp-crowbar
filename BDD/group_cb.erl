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
-module(group_cb).
-export([step/3, json/3, json/4, g/1, validate/1, inspector/1]).
-include("bdd.hrl").	
	
g(Item) ->
  case Item of
    categories -> ["ui","rack","tag"];
    path -> "/crowbar/v2/groups";
    name1 -> "bddthings";
    atom1 -> group1;
    name2 -> "bdddelete";
    atom2 -> group2;
    name_node1 -> "group1.node.test";
    atom_node1 -> gnode1;
    _ -> crowbar:g(Item)
  end.

validate(JSON) ->
  Wrapper = crowbar_rest:api_wrapper(JSON),
  J = Wrapper#item.data,
  Category = json:keyfind(J, category),
  R = [bdd_utils:is_a(J, number, order), 
       lists:member(Category,g(categories)), 
       crowbar_rest:validate(J)],
  bdd_utils:assert(R).

% Common Routine
% Returns list of nodes in the system to check for bad housekeeping
inspector(Config) -> 
  bdd_restrat:inspector(Config, group).  % shared inspector works here, but may not always
  
% Returns the JSON List Nodes in the Group
get_group_nodes(Config, Group) ->
  URI = eurl:path([g(path),Group, "nodes"]),
  bdd_utils:log(debug, group_cb, get_group_nodes, "get ~p list for ~p path", [Group, URI]),
  {200, JSON} = eurl:get_page(Config, URI, all),
  Wrapper = crowbar_rest:api_wrapper_raw(JSON),
  Wrapper#list.ids.
  
% DRY the path creation
group_node_path(Group, Node) ->  eurl:path([node:g(path), Node, "groups", Group]).

% Build Group JSON  
json(Name, Description, Order)           -> json(Name, Description, Order, "ui").
json(Name, Description, Order, Category) ->
  json:output([{"name",Name},{"description", Description}, {"category", Category}, {"order", Order}]).
	
% STEPS!

step(Config, _Given, {step_given, _N, ["REST adds the node",Node,"to",Group]}) -> 
  step(Config, _Given, {step_when, _N, ["REST adds the node",Node,"to",Group]});

step(Config, _Given, {step_when, _N, ["REST adds the node",Node,"to",Group]}) -> 
  bdd_utils:log(debug, group_cb, step, "group:step REST add the node ~p to ~p",[Node, Group]),
  eurl:put_post(Config, group_node_path(Group, Node), [], put, all);

step(Config, _Given, {step_when, _N, ["REST removes the node",Node,"from",Group]}) -> 
  bdd_utils:log(debug, group_cb, step, "group:step REST remove the node ~p from ~p",[Node, Group]),
  eurl:delete(Config, group_node_path(Group, Node), []);


step(Config, _Result, {step_then, _N, ["there is not a",_Category,"group",Group]}) -> 
  % WARNING - this IGNORES THE CATEGORY, it is not really a true test for the step.
  case bdd_restrat:get_id(Config, g(path), Group) of
    "-1" -> true;
    ID -> true =/= is_number(ID)
  end;

step(Config, _Result, {step_then, _N, ["the group",Group,"should have at least",Count,"node"]}) -> 
  {C, _} = string:to_integer(Count),
  Nodes = get_group_nodes(Config, Group),
  bdd_utils:log(debug, group_cb, step_step, "Group ~p at least ~p from ~p", [Group, Count, Nodes]),
  Items = length(Nodes),
  Items >= C;

step(Config, _Result, {step_then, _N, ["the group",Group,"should have",Count,"nodes"]}) -> 
  {C, _} = string:to_integer(Count),
  Nodes = get_group_nodes(Config, Group),
  bdd_utils:log(debug, group_cb, step_step, "Group ~p has ~p from ~p", [Group, Count, Nodes]),
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

step(_Config, _Global, {step_teardown, _N, _}) -> 
  % find the node from setup and remove it
  bdd_restrat:destroy(g(path), g(atom2)),
  bdd_restrat:destroy(g(path), g(atom1)),
  bdd_restrat:destroy(node:g(path), g(atom_node1)).
