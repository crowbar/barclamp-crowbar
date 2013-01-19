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
-module(bdd_catchall).
-export([step/3]).
-import(bdd_utils).

step(_Config, _Result, {_, _N, ["pause", Time, "seconds to", Message]}) -> 
  {T, _} = string:to_integer(Time),
  io:format("\t\t\t...paused ~p seconds in order to ~s.~n", [T, Message]),
  timer:sleep(T*1000);
step(_Config, _Result, {_, _N, ["after", Time, "seconds"]}) -> 
  {T, _} = string:to_integer(Time),
  io:format("\t\t\tzzz...sleeping ~p seconds.~n", [T]),
  timer:sleep(T*1000);
step(_Config, _Result, {_, _N, ["after", Time, "minutes"]}) -> 
  {T, _} = string:to_integer(Time),
  io:format("\t\t\tzzz...sleeping ~p minutes.~n", [T]),
  timer:sleep(T*60000);
step(_Config, _Result, {_, _N, ["after", Time, "milliseconds"]}) -> 
  {T, _} = string:to_integer(Time),
  io:format("\t\t\tzzz...sleeping ~p milliseconds.~n", [T]),
  timer:sleep(string:to_integer(T));

step(Config, _Global, {step_setup, _N, _}) -> 
  bdd_utils:log(info, bdd_catchall, step, "No Feature Setup Step.", []),
  Config;

step(Config, _Global, {step_teardown, _N, _}) -> 
  bdd_utils:log(info, bdd_catchall, step, "No Feature Tear Down Step.", []),
  Config;

step(_Config, _Result, {step_given, _N, ["I do nothing to", Text]}) ->  Text;
step(_Config, _Result, {step_when, _N, ["I do nothing to", Text]}) ->  Text;
step(_Config, _Result, {step_then, _N, ["I always pass"]}) -> true;
step(_Config, _Result, {step_then, _N, ["I always fail"]}) -> false;

step(_Config, _Result, {step_given, {Scenario, _N}, ["I set",Key,"to",Value]}) ->
  bdd:log(trace, "bdd_catchall storing scenario info: Given I set ~p to ~p", [Key, Value]),
  bdd_utils:scenario_store(Scenario, Key, Value);

step(Config, _Result, {step_given, _N, StepAction}) ->
	bdd_utils:log(Config, warn, "ADD MISSING GIVEN STEP: ~n\tstep(_Config, _Global, {step_given, {_Scenario, _N}, ~p}) -> false;~n", [StepAction]),
	false;

step(Config, _Result, {step_when, _N, StepAction}) ->
	bdd_utils:log(Config, warn, "ADD MISSING WHEN STEP: ~n\tstep(_Config, _Given, {step_when, {_Scenario, _N}, ~p}) -> false;~n", [StepAction]),
	false;

step(Config, _Result, {step_then, _N, StepAction}) ->
	bdd_utils:log(Config, warn, "ADD MISSING THEN STEP: ~n\tstep(_Config, _Result, {step_then, {_Scenario, _N}, ~p}) -> false;~n", [StepAction]),
	false;
	
step(Config, _Result, {step_finally, _N, StepAction}) ->
	bdd_utils:log(Config, warn, "ADD MISSING FINALLY STEP: ~n\tstep(_Config, _Given, {step_finally, {_Scenario, _N}, ~p}) -> false;~n", [StepAction]),
	false;

step(Config, _Result, {StepType, _N, StepAction}) ->
	bdd_utils:log(Config, error, "UNKNOWN STEP TYPE: \"step(_Config, _, {~p, {_Scenario, _N}, ~p}) -> false;\"~n", [StepType, StepAction]),
	false;

step(Config, _Result, StepTupple) ->
	bdd_utils:log(Config, error, "INVALID STEP TUPPLE: Cannot resolve ~p~n", [StepTupple]),
	false.
