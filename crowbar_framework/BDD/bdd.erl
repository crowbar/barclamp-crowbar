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
-export([test/0, test/1, feature/1, feature/2, getconfig/1, start/1, stop/1]).  
-import(bdd_utils).
-import(digest_auth).
-export([step_run/3, step_run/4]).
%-export([start/1, run/2, stop/1]).   % internal access to the testing process

test()                   -> test("default").
test(ConfigName)         -> 
  BaseConfig = getconfig(ConfigName),
  % start the test config & run the global tests
  StartedConfig = start(BaseConfig),
  % get the list of features to test
  Features = filelib:wildcard(bdd_utils:config(BaseConfig,feature_path,"features/") ++ "*." ++ bdd_utils:config(BaseConfig,extension,"feature")),
  %run the tests
  Results = run(StartedConfig, [], Features),
  % cleanup application services
  stop(StartedConfig),
	case [R || {_, R} <- Results, R =/= pass] of
	  [] -> pass;
	  _ -> Results
	end.
	  
% similar to test, this can be used to invoke a single feature for testing
feature(Feature)             -> feature("default", Feature).
feature(ConfigName, Feature) ->
  Config = getconfig(ConfigName),
  FileName = bdd_utils:config(Config,feature_path,"features/") ++ Feature ++ "." ++ bdd_utils:config(Config,extension,"feature"),
  FeatureConfig = run(Config, Feature, FileName),
	[{feature, _F, R} | _ ] = stop(FeatureConfig),
	R.

% helper that finds the feature from the FileName
feature_name(Config, FileName) ->
  RegEx = bdd_utils:config(Config,feature_path,"features/") ++ "(.*)." ++ bdd_utils:config(Config,extension,"feature"),
	{ok, RE} = re:compile(RegEx, [caseless]),
	case re:run(FileName, RE) of 
	  {match,[{0,_},{Start,Length}]} -> string:sub_string(FileName, Start+1, Start+Length);
	  _ -> FileName
	end.

% recursive runner with error catching
run(_Config, [], [])                  -> [];
run(Config, [], [FileName | Features]) ->
  Feature = feature_name(Config, FileName),
	R = try run(Config, Feature, FileName) of
		Run -> 
	    {feature, _Name, Result} = lists:keyfind(feature,1,Run),
	    io:format("\tRESULTS: ~p.~n", [Result]),
      Result
	catch
		X: Y -> io:format("ERROR: feature error ~p:~p~n", [X, Y]),
		[error]
	end,
  [R | run(Config, [], Features)];

% the main runner requires you to have the feature & filename defined
run(Config, Feature, FileName)     ->
  % figure out the file name
  Fatom = list_to_atom(Feature),
  % start inet client if not running
	RunningConfig = start(Config),                                              
	%store the digest header
	StartConfig = digest_auth:header(RunningConfig, sc:url(RunningConfig)),     
  % stuff the feature & file name into the config set
  FeatureConfig = [{feature, Feature}, {file, FileName} | StartConfig],		     
  % import the feature information
	{feature, Name, Scenarios} = feature_import(FileName),
	[ScenarioName, _ScenarioIn, _ScenarioWho, _ScenarioWhy | _ ] = [string:strip(S) || S <- Name, S =/= []],
	% setup UI
	io:format(" FEATURE: ~s.~n", [ScenarioName]),
  % setup the tests
	SetupConfig = step_run(FeatureConfig, [], {step_setup, 0, Feature}, [Fatom]),  % setup
  % run the tests
  Result = {feature, ScenarioName, [setup_scenario(SetupConfig, Scenario, []) || Scenario <- Scenarios]},
  % tear down
  step_run(SetupConfig, [], {step_teardown, 0, Feature}, [Fatom]),  %teardown
  % return setup before we added feature stuff
	[Result | StartConfig].
	
start(Config) ->
  Started = bdd_utils:config(Config, started, false),
  Global = bdd_utils:config(Config, global_setup, default),
  case Started of
    false -> 
      application:start(crypto),
      application:start(inets),
    	AzConfig = bdd_utils:is_site_up(Config),
      SetupConfig = step_run(AzConfig, [], {step_setup, 0, "Global"}, [Global]),  
      [{started, true} | SetupConfig ];
  	_ -> Config
	end.
	
stop(Config) ->
  Started = bdd_utils:config(Config, started, false),
  Global = bdd_utils:config(Config, global_setup, default),
  case Started of
    true -> 
      TearDownConfig = step_run(Config, [], {step_teardown, 0, "Global"}, [Global]),  
      application:stop(crypto),
      application:stop(inets),
  	  lists:delete({started, true}, TearDownConfig);
  	_ -> Config  
	end.

% load the configuration file
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

% output results information
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
	{N, GivenSteps, WhenSteps, ThenSteps, FinalSteps} = scenario_steps(RawSteps),
	io:format("\tSCENARIO: ~p (~p steps) ", [Name, N]),
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
	% safe to cleanup with the finally steps (we don't care about the result of those)
	_Final = [{step_run(Config, Given, FS), FS} || FS <- FinalSteps],
	% now, check the results of the then steps
	case bdd_utils:assert_atoms(Result) of
		true -> bdd_utils:untrace(Config, Name, N), io:format("PASSED!~n",[]), pass;
		_ -> io:format("~n\t\t*** FAILURE REPORT ***~n"), print_fail(lists:reverse(Result))
	end.
  
% Inital request to run a step does not know where to look for the code, it will iterate until it finds the step match or fails
step_run(Config, Input, Step) ->
	StepFiles = [list_to_atom(bdd_utils:config(Config, feature)) | bdd_utils:config(Config, secondary_step_files, [bdd_webrat, bdd_catchall])],
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
      io:format("~nERROR: web server not responding.  Details: ~p~n",[Details]), 
      throw("BDD ERROR: Could not connect to web server.");
		X: Y -> 
		  io:format("~nERROR: step run found ~p:~p~n", [X, Y]), 
      io:format("\tAttempted \"apply(~p, step, [[Config], [Input], ~p]).\"~n",[Feature, Step]),
		  throw("BDD ERROR: Unknown error type in BDD:step_run.")
	end;

% we don't want to FAIL for missing setup and teardown steps	
step_run(Config, _Input, {step_setup, _, Feature}, []) -> io:format("\tFeature ~p has no Setup defined.~n", [Feature]), Config;
step_run(Config, _Input, {step_teardown, _, Feature}, []) -> io:format("\tFeature ~p has no Teardown defined.~n", [Feature]), Config;
	
% no more places to try, fail and tell the user to create the missing step
step_run(_Config, _Input, Step, []) ->
	io:format("ERROR: Unable to resolve step ~p!~n", [Step]),
	throw("FAIL: no matching expression found for Step"), 
	error.
	
% Split our steps into discrete types for sequential processing
% Each step is given a line number to help w/ debug
scenario_steps(Steps) ->
	%io:format("\t\tDEBUG: processing steps ~p~n", [Steps]),
	scenario_steps(Steps, 1, [], [], [], [], unknown).
scenario_steps([H | T], N, Given, When, Then, Finally, LastStep) ->
	CleanStep = bdd_utils:clean_line(H),
	{Type, StepRaw} = step_type(CleanStep),
	StepPrep = {Type, bdd_utils:tokenize(StepRaw)},
	Step = case StepPrep of
	  {step_and, SS} -> {LastStep, SS};
	  {Type, SS} -> {Type, SS}
	end,
	case Step of
		{step_given, S} -> scenario_steps(T, N+1, [{step_given, N, S} | Given], When, Then, Finally, step_given);
		{step_when, S} -> scenario_steps(T, N+1, Given, [{step_when, N, S} | When], Then, Finally, step_when);
		{step_then, S} -> scenario_steps(T, N+1, Given, When, [{step_then, N, S} | Then], Finally, step_then);
		{step_finally, S} -> scenario_steps(T, N+1, Given, When, Then, [{step_finally, N, S} | Finally], step_finally);
		{empty, _} -> scenario_steps(T, N, Given, When, Then, Finally, empty);
		{unknown, Mystery} -> io:format("\t\tWARNING: No prefix match for ~p~n", [Mystery]), scenario_steps(T, N+1, Given, When, Then, Finally, unknown)		
	end;
scenario_steps([], N, Given, When, Then, Finally, _) ->
	% returns number of steps and breaks list into types, may be expanded for more times in the future!
	{N, Given, When, Then, Finally}.
	
% figure out what type of step we are doing (GIVEN, WHEN, THEN, etc), return value
step_type([$G, $i, $v, $e, $n, 32 | Given]) ->
	{ step_given, Given };
step_type([$W, $h, $e, $n, 32 | When] ) ->
	{ step_when, When };
step_type([$T, $h, $e, $n, 32 | Then ]) -> 
	{ step_then, Then };
step_type([$F, $i, $n, $a, $l, $l, $y, 32 | Finally ]) -> 
	{ step_finally, Finally };
step_type([$A, $n, $d, 32 | Next ]) -> 
	{ step_and, Next };
step_type([]) ->
	{ empty, []};
step_type(Step) ->
	{ unknown, Step }.


