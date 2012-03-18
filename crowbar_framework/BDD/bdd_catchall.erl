-module(bdd_catchall).
-export([step/3]).
-import(bdd_util).

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
  io:format("\tNo Feature Setup Step.~n"),
  Config;

step(Config, _Global, {step_teardown, _N, _}) -> 
  io:format("\tNo Feature Tear Down Step.~n"),
  Config;

step(_Config, _Result, {step_given, _N, ["I do nothing to", Text]}) ->  Text;
step(_Config, _Result, {step_when, _N, ["I do nothing to", Text]}) ->  Text;
step(_Config, _Result, {step_then, _N, ["I always pass"]}) -> true;
step(_Config, _Result, {step_then, _N, ["I always fail"]}) -> false;

step(_Config, _Result, {step_given, _N, StepAction}) ->
	io:format("ADD MISSING GIVEN STEP: \"step(_Config, _Global, {step_given, _N, ~p}) -> false;\"~n", [StepAction]),
	false;

step(_Config, _Result, {step_when, _N, StepAction}) ->
	io:format("ADD MISSING WHEN STEP: \"step(_Config, _Given, {step_when, _N, ~p}) -> false;\"~n", [StepAction]),
	false;

step(_Config, _Result, {step_then, _N, StepAction}) ->
	io:format("ADD MISSING THEN STEP: \"step(_Config, _Result, {step_then, _N, ~p}) -> false;\"~n", [StepAction]),
	false;
	
step(_Config, _Result, {StepType, _N, StepAction}) ->
	io:format("UNKNOWN STEP TYPE: \"step(_Config, _, {~p, _N, ~p}) -> false;\"~n", [StepType, StepAction]),
	false;

step(_Config, _Result, StepTupple) ->
	io:format("INVALID STEP TUPPLE: Cannot resolve ~p~n", [StepTupple]),
	false.
