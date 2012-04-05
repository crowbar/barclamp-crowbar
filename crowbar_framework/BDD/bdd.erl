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

-module(bdd).
-export([test/1, test/3, feature/2]).  %this is the final one
-import(bdd_utils).
-import(digest_auth).

test(ConfigName) -> 
  test(ConfigName, search, []).
test(ConfigName, search, Tests) ->
  BaseConfig = getconfig(ConfigName),
  Features = filelib:wildcard("*." ++ bdd_utils:config(BaseConfig,extension)),
	application:start(inets),		% needed for getting we pages
	application:start(crypto),  % needed for digest authentication
	StartConfig = digest_auth:header(BaseConfig, sc:url(BaseConfig)),   %store the digest header
	%test setup
  Config = step_run(StartConfig, [], {step_setup, 0, []}, [list_to_atom(ConfigName)]),  
  %run the tests
  Results = [{feature, FileName, test(Config, FileName, Tests)} || FileName <- Features],
  %test teardown
  step_run(Config, [], {step_teardown, 0, []}, [list_to_atom(ConfigName)]),  
  % cleanup application services
  application:stop(crypto),
	application:stop(inets),
	Result = [R || {_, R} <- Results, R =/= pass],
	case Result of
		[] -> pass;
		_ -> bdd_selftest:test(all), Result
	end;
test(ConfigBase, FileName, Tests) -> 
	[Feature | _ ] = string:tokens(FileName,"."),
	ConfigFile = [{feature, Feature}, {file, FileName} | ConfigBase],		% stuff the file name into the config set for later
	{feature, Name, Scenarios} = feature_import(FileName),
	[ScenarioName, _ScenarioIn, _ScenarioWho, _ScenarioWhy | _ ] = [string:strip(S) || S <- Name, S =/= []],
	io:format(" FEATURE: ~s.~n", [ScenarioName]),
	% setup the feature
  Config = step_run(ConfigFile, [], {step_setup, 0, []}, [list_to_atom(Feature)]),
  Result = {feature, ScenarioName, [setup_scenario(Config, Scenario, Tests) || Scenario <- Scenarios]},
  step_run(Config, [], {step_teardown, 0, []}, [list_to_atom(Feature)]),
  Result.
  
% similar to test, this can be used to invoke a single feature for testing
feature(ConfigName, FeatureName) ->
  BaseConfig = getconfig(ConfigName),
  FileName = FeatureName ++ "." ++ bdd_utils:config(BaseConfig,extension),
	application:start(inets),		% needed for getting we pages
	application:start(crypto),  % needed for digest authentication
	StartConfig = digest_auth:header(BaseConfig, sc:url(BaseConfig)),   %store the digest header
	Config = step_run(StartConfig, [], {step_setup, 0, []}, [list_to_atom(ConfigName)]),  % setup
  test(Config, FileName, []),
  step_run(Config, [], {step_teardown, 0, []}, [list_to_atom(ConfigName)]),  %teardown
	application:stop(crypto),
	application:stop(inets).
  
getconfig(ConfigName) ->
  {ok, ConfigBase} = file:consult(ConfigName++".config"),
  [{config, ConfigName} | ConfigBase].
  
% read in the feature file
feature_import(FileName) ->
	{ok, Features} = file:read_file(FileName),
	[Header | Body] = re:split(Features,"Scenario:"),
	Name = bdd_utils:clean_line(string:tokens(binary_to_list(Header),"\n")),
	Scenarios = [binary_to_list(S) || S <- Body],
	{feature, Name, Scenarios}.
	
% run the scenarios, test list allows you to pick which tests
setup_scenario(Config, Scenario, Tests) ->
	[RawName | RawSteps] = string:tokens(Scenario, "\n"),
	Name = bdd_utils:clean_line(RawName),
  [First | _ ] = Name,
	Member = lists:member(Name,Tests),
	if 
	  First =:= $% -> io:format("\tDISABLED ~p~n", [Name]);
	  Member; length(Tests) =:= 0 -> test_scenario(Config, RawSteps, Name);
	  true -> io:format("\tSKIPPED ~p~n", [Name])
	end.

print_fail([]) -> true;
print_fail({Pass, {_Type, N, Description}}) ->
  PF = case Pass of
    true -> ". Pass";
    _ -> "X FAIL"
  end,
  io:format("\t\t~s #~p: ~s~n", [PF, N, lists:flatten([D ++ " " || D <- Description])]);
print_fail([Result | Results]) ->
  print_fail(Result),
  print_fail(Results).

% decompose each scearion into the phrases, must be executed in the right order (Given -> When -> Then)
test_scenario(Config, RawSteps, Name) ->
  % organize steps in the scenarios
	{N, GivenSteps, WhenSteps, ThenSteps} = scenario_steps(RawSteps),
	% execute all the given steps & put their result into GIVEN
	bdd_utils:trace(Config, Name, N, RawSteps, ["No Given: pending next pass..."], ["No When: pending next pass..."]),
	Given = [step_run(Config, [], GS) || GS <- GivenSteps],
	% now, excute the when steps & put the result into RESULT
	bdd_utils:trace(Config, Name, N, RawSteps, Given, ["No When: pending next pass..."]),
	When = case length(WhenSteps) of
	  0 -> Given;
	  _ -> [step_run(Config, Given, WS) || WS <- WhenSteps]
	end,
	bdd_utils:trace(Config, Name, N, RawSteps, Given, When),
	% now, check the results
	Result = [{step_run(Config, When, TS), TS} || TS <- ThenSteps],
	% now, run the then steps
	io:format("\tSCENARIO: ~p (~p steps) ", [Name, N]),
	case bdd_utils:assert_atoms(Result) of
		true -> io:format("PASSED!~n",[]), pass;
		_ -> io:format("~n\t\t*** FAILURE REPORT ***~n"), print_fail(lists:reverse(Result))
	end.
  
% Inital request to run a step does not know where to look for the code, it will iterate until it finds the step match or fails
step_run(Config, Input, Step) ->
	StepFiles = [list_to_atom(bdd_utils:config(Config, feature)) | bdd_utils:config(Config, secondary_step_files)],
  step_run(Config, Input, Step, StepFiles).
	
% recursive attempts to run steps
step_run(Config, Input, Step, [Feature | Features]) ->
	try apply(Feature, step, [Config, Input, Step]) of
		error -> 
		  {error, Step};
		Result -> Result
	catch
		error: undef -> step_run(Config, Input, Step, Features);
		error: function_clause -> step_run(Config, Input, Step, Features);
		exit: {noproc, {gen_server, call, Details}} -> 
		  io:format("exit Did not find step: ~p~n", [Feature]),
      io:format("ERROR: web server not responding.  Details: ~p~n",[Details]), 
      throw("BDD ERROR: Could not connect to web server.");
		X: Y -> 
		  io:format("ERROR: step run found ~p:~p~n", [X, Y]), 
      io:format("\tAttempted \"apply(~p, step, [[Config], [Input], ~p]).\"~n",[Feature, Step]),
		  throw("BDD ERROR: Unknown error type in BDD:step_run.")
	end;

% we don't want to FAIL for missing setup and teardown steps	
step_run(Config, _Input, {step_setup, _, _}, []) -> io:format("\tFeature has no Setup defined~n"), Config;
step_run(Config, _Input, {step_teardown, _, _}, []) -> io:format("\tFeature has no Teardown defined~n"), Config;
	
% no more places to try, fail and tell the user to create the missing step
step_run(_Config, _Input, Step, []) ->
	io:format("ERROR: Unable to resolve step ~p!~n", [Step]),
	throw("FAIL: no matching expression found for Step"), 
	error.
	
% Split our steps into discrete types for sequential processing
% Each step is given a line number to help w/ debug
scenario_steps(Steps) ->
	%io:format("\t\tDEBUG: processing steps ~p~n", [Steps]),
	scenario_steps(Steps, 1, [], [], [], unknown).
scenario_steps([H | T], N, Given, When, Then, LastStep) ->
	CleanStep = bdd_utils:clean_line(H),
	{Type, StepRaw} = step_type(CleanStep),
	StepPrep = {Type, bdd_utils:tokenize(StepRaw)},
	Step = case StepPrep of
	  {step_and, SS} -> {LastStep, SS};
	  {Type, SS} -> {Type, SS}
	end,
	case Step of
		{step_given, S} -> scenario_steps(T, N+1, [{step_given, N, S} | Given], When, Then, step_given);
		{step_when, S} -> scenario_steps(T, N+1, Given, [{step_when, N, S} | When], Then, step_when);
		{step_then, S} -> scenario_steps(T, N+1, Given, When, [{step_then, N, S} | Then], step_then);
		{empty, _} -> scenario_steps(T, N, Given, When, Then, empty);
		{unknown, Mystery} -> io:format("\t\tWARNING: No prefix match for ~p~n", [Mystery]), scenario_steps(T, N+1, Given, When, Then, unknown)		
	end;
scenario_steps([], N, Given, When, Then, _) ->
	% returns number of steps and breaks list into types, may be expanded for more times in the future!
	{N, Given, When, Then}.
	
% figure out what type of step we are doing (GIVEN, WHEN, THEN, etc), return value
step_type([$G, $i, $v, $e, $n, 32 | Given]) ->
	{ step_given, Given };
step_type([$W, $h, $e, $n, 32 | When] ) ->
	{ step_when, When };
step_type([$T, $h, $e, $n, 32 | Then ]) -> 
	{ step_then, Then };
step_type([$A, $n, $d, 32 | Next ]) -> 
	{ step_and, Next };
step_type([]) ->
	{ empty, []};
step_type(Step) ->
	{ unknown, Step }.


