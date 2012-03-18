% Copyright 2011, Dell 
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
% Author: RobHirschfeld 
% 
-module(crowbar).
-export([step/3]).
-import(bdd_utils).
-import(json).

step(Config, _Global, {step_setup, _N, _}) -> 
  io:format("\tNo Global Setup Step.~n"),
  Config;

step(Config, _Global, {step_teardown, _N, _}) -> 
  io:format("\tNo Global Tear Down Step.~n"),
  Config;

step(Config, _Global, {step_given, _N, ["there is a node", Node, "in state", State]}) ->
  Path = "/crowbar/crowbar/1.0/transition/default",
  Data = json:output([{name, Node++"."++sc:domain(Config)}, {state, State}]),
  Result = digest_auth:request(Config, post, {sc:url(Config) ++ Path, "application/json", "application/json", Data}, [{timeout, 10000}], []),
  case Result of
    {ok, {_, _, Body}} -> Body;
    _ -> io:format("Post did not return ok: ~s: ~s~n", [Path, Data]), error
  end;

step(_Config, _Given, {step_when, _N, ["I have a test that is not in WebRat"]}) -> true;
                                    
step(_Config, _Result, {step_then, _N, ["I should use my special step file"]}) -> true.

