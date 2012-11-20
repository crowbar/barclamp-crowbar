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
-export([test/0, test/1, feature/1, feature/2, scenario/2, scenario/3, scenario/4, debug/3, debug/4, failed/0, failed/1, getconfig/1, start/1, stop/1, steps/0, steps/1]).  
-import(bdd_utils).
-import(simple_auth).
-export([step_run/3, step_run/4, inspect/1, is_clean/1]).

test()                   -> test("default").
test(ConfigName)         -> 
  BaseConfig = getconfig(ConfigName),
  % start the test config & run the global tests
  StartedConfig = start(BaseConfig),
  % get the list of features to test
  Features = bdd_utils:features(StartedConfig),
  %run the tests
  Complete = run(StartedConfig, [], Features),
  % cleanup application services
  EndConfig = stop(Complete),
  Results = lists:filter(fun(R) -> case R of {feature, _, _, _}->true; _ -> false end end, EndConfig),
  file:write_file("../tmp/bdd_results.out",io_lib:fwrite("{test, ~p, ~p, ~p}.\n",[date(), time(),Results])),
  [{Fatom, print_report(R)} || {feature, Fatom, _Feature, R} <-Results].
  
% similar to test, this can be used to invoke a single feature for testing
feature(Feature) when is_atom(Feature)  -> feature("default", atom_to_list(Feature));
feature(Feature)                        -> feature("default", Feature).
feature(ConfigName, Feature) when is_atom(ConfigName), is_atom(Feature) -> feature(atom_to_list(ConfigName), atom_to_list(Feature));
feature(ConfigName, Feature)            -> scenario(ConfigName, Feature, all).

% run one or `all` of the scenarios in a feature
scenario(Feature, ID)                  -> scenario("default", atom_to_list(Feature), ID).
scenario(ConfigName, Feature, ID) when is_atom(ConfigName), is_atom(Feature)  
                                       -> scenario(atom_to_list(ConfigName), atom_to_list(Feature), ID, []);
scenario(ConfigName, Feature, ID)      -> scenario(ConfigName, Feature, ID, []).
scenario(ConfigName, Feature, ID, Log) ->
  Config = [{log, Log} | getconfig(ConfigName)],
  FileName = bdd_utils:features(Config, Feature),
  FeatureConfig = run(Config, Feature, FileName, ID),
  C = stop(FeatureConfig),
  lists:keyfind(feature, 1, C).
  
% version of scenario with extra loggin turned on
debug(Config, Feature, ID)      -> scenario(atom_to_list(Config), atom_to_list(Feature), ID, [puts, debug, info, warn]).
debug(Config, Feature, ID, Log) -> scenario(atom_to_list(Config), atom_to_list(Feature), ID, Log).

% used after a test() run to rerun just the failed tests
failed()        -> failed(default).
failed(Config)  ->
  {ok, [{test, _Date, _Time, Results} | _]} = file:consult("../tmp/bdd_results.out"),
  Fails = [{Feature, lists:keyfind(fail, 1, print_result(Fails))} || {feature, Feature, _, Fails} <- Results],
  % please optimize to use just 1 global setup!
  [ failed(Config, Feature, F) || {Feature, {fail, Num, F}} <- Fails, Num > 0].
failed(_Config, Feature, [])     -> Feature;
failed(Config, Feature, [ID | T]) ->
  scenario(Config, Feature, ID),
  failed(Config, Feature, T).

% recursive runner with error catching
run(Config, [], [])                    -> Config;   % stop recursing, return config
run(Config, [], [FileName | Features]) ->
  Feature = bdd_utils:feature_name(Config, FileName),
	R = try run(Config, Feature, FileName) of
		Run -> 
	    {feature, _FAtom, _Name, Result} = lists:keyfind(feature,1,Run),
	    Out = print_result(Result),
	    {total, Total} = lists:keyfind(total, 1, Out),
	    {pass, Pass, _P} = lists:keyfind(pass, 1, Out),
	    {fail, _N, Fail} = lists:keyfind(fail, 1, Out),
	    io:format("\tRESULTS: Passed ~p of ~p.  Failed ~p.~n", [Pass, Total, Fail]),
      lists:keyfind(feature,1,Run)
	catch
		X: Y -> io:format("ERROR: Feature error ~p:~p~n", [X, Y]),
		[error]
	end,
  [R | run(Config, [], Features)];

% the main runner requires you to have the feature & filename defined
run(Config, Feature, FileName)     -> run(Config, Feature, FileName, all).
run(Config, Feature, FileName, ID) ->
  % figure out the file name
  Fatom = list_to_atom(Feature),
  % start inet client if not running
  StartConfig = start(Config),                       
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
  Result = {feature, Fatom, ScenarioName, [setup_scenario(SetupConfig, Scenario, ID) || Scenario <- Scenarios]},
  % tear down
  step_run(SetupConfig, [], {step_teardown, 0, Feature}, [Fatom]),  %teardown
  % return setup before we added feature stuff
  [Result | StartConfig].
	
% manual start helper for debugging
start(Config) when is_atom(Config) -> 
  C = getconfig(atom_to_list(Config)),
  start(C);
  
% critical start method used by tests
start(Config) ->
  Started = bdd_utils:config(Config, started, false),
  Global = bdd_utils:config(Config, global_setup, default),
  case Started of
    false -> 
      application:start(crypto),
      application:start(inets),
      AzConfig = bdd_utils:is_site_up(Config),
      file:write_file("../tmp/inspection.list",io_lib:fwrite("~p.\n",[inspect(AzConfig)])),
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
      is_clean(TearDownConfig),
      application:stop(crypto),
      application:stop(inets),
      lists:delete({started, true}, TearDownConfig);
    _ -> Config  
  end.

% load the configuration file
getconfig(Config) when is_atom(Config) -> getconfig(atom_to_list(Config));
getconfig(ConfigName)                  ->
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
setup_scenario(Config, Scenario, ID) ->
  [RawName | RawSteps] = string:tokens(Scenario, "\n"),
  Name = bdd_utils:clean_line(RawName),
  [First | _ ] = Name,
  TestID = erlang:phash2(Name),
  if 
    First =:= $% -> io:format("\tDISABLED ~p~n", [Name]);
    ID =:= all; TestID =:= ID -> test_scenario(Config, RawSteps, Name);
    true -> io:format("\t........: skipping ~p (~p)~n", [TestID, Name])
  end.

print_report({feature, _, _, Result}) ->  print_report(Result);
print_report(Result)  ->
  Out = print_result(Result),
  {total, Total} = lists:keyfind(total, 1, Out),
  {pass, Pass, _P} = lists:keyfind(pass, 1, Out),
  {fail, Fail, IDs} = lists:keyfind(fail, 1, Out),
  {skip, Skip} = lists:keyfind(skip, 1, Out),
  {Total, Pass, Fail, Skip, IDs}.
print_result(Result)                  ->  print_result(Result, [], [], []).
print_result([], Pass, Fail, Skip)    ->  
  [{total, length(Pass)+length(Fail)+length(Skip)}, 
    {pass, length(Pass), Pass}, 
    {fail, length(Fail), Fail},
    {skip, length(Skip)}];
print_result([Result | T], Pass, Fail, Skip)->
  case Result of
    {ID, pass} -> F=Fail, P=[ID | Pass], S=Skip;
    ok         -> P=Pass, F=Fail, S=[skip | Skip];
    {ID, _}    -> P=Pass, F=[ID | Fail], S=Skip
  end,
  print_result(T, P, F, S).
  
% output results information
print_fail([]) -> true;
print_fail({Pass, {_Type, N, Description}}) ->
  PF = case Pass of
    true -> ". Pass";
    _ -> "X FAIL"
  end,
  io:format("\t\t~s #~p: ~s~n", [PF, N, lists:flatten([D ++ " " || D <- Description, is_list(D)])]);
print_fail([Result | Results]) ->
  print_fail(Result),
  print_fail(Results).

% decompose each scearion into the phrases, must be executed in the right order (Given -> When -> Then)
test_scenario(Config, RawSteps, Name) ->
  % organize steps in the scenarios
	{N, BackwardsGivenSteps, BackwardsWhenSteps, BackwardsThenSteps, BackwardsFinalSteps} = scenario_steps(Config, RawSteps),

  % The steps lists are built in reverse order that they appear in the feature file in
  % accordance with erlang list building optimization.  Reverse the order here so that
  % the steps are executed in the same order as they are listed in the feature file
  GivenSteps = lists:reverse(BackwardsGivenSteps),
  WhenSteps = lists:reverse(BackwardsWhenSteps),
  ThenSteps = lists:reverse(BackwardsThenSteps),
  FinalSteps = lists:reverse(BackwardsFinalSteps),

  Hash = erlang:phash2(Name),
	io:format("\tSCENARIO: ~p (id: ~p, steps:~p) ", [Name, Hash, N]),
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
	R = case bdd_utils:assert_atoms(Result) of
		true -> bdd_utils:untrace(Config, Name, N), io:format("PASSED!~n",[]), pass;
		_ -> io:format("~n\t\t*** FAILURE REPORT ***~n"), print_fail(lists:reverse(Result))
	end,
	{Hash, R}.
  
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
      io:format("Stacktrace: ~p~n", [erlang:get_stacktrace()]),
      io:format("\tAttempted \"apply(~p, step, [[Config], [Input], ~p]).\"~n",[Feature, Step]),
		  throw("BDD ERROR: Unknown error type in BDD:step_run.")
	end;

% we don't want to FAIL for missing setup and teardown steps	
step_run(Config, _Input, {step_setup, _, Feature}, []) -> io:format("\tFeature ~p has no Setup defined (or it throws an error).~n", [Feature]), Config;
step_run(Config, _Input, {step_teardown, _, Feature}, []) -> io:format("\tFeature ~p has no Teardown defined (or it throws an error).~n", [Feature]), Config;
	
% no more places to try, fail and tell the user to create the missing step
step_run(_Config, _Input, Step, []) ->
	io:format("ERROR: Unable to resolve step ~p!~n", [Step]),
	throw("FAIL: no matching expression found for Step"), 
	error.
	
% Split our steps into discrete types for sequential processing
% Each step is given a line number to help w/ debug
scenario_steps(Config, Steps) ->
	%io:format("\t\tDEBUG: processing steps ~p~n", [Steps]),
	scenario_steps(Config, Steps, 1, [], [], [], [], unknown).
scenario_steps(Config, [H | T], N, Given, When, Then, Finally, LastStep) ->
	CleanStep = bdd_utils:clean_line(H),
	{Type, StepRaw} = step_type(CleanStep),
	StepPrep = {Type, bdd_utils:tokenize(Config, StepRaw)},
	Step = case StepPrep of
	  {step_and, SS} -> {LastStep, SS};
	  {Type, SS} -> {Type, SS}
	end,
	case Step of
		{step_given, S} -> scenario_steps(Config, T, N+1, [{step_given, N, S} | Given], When, Then, Finally, step_given);
		{step_when, S} -> scenario_steps(Config, T, N+1, Given, [{step_when, N, S} | When], Then, Finally, step_when);
		{step_then, S} -> scenario_steps(Config, T, N+1, Given, When, [{step_then, N, S} | Then], Finally, step_then);
		{step_finally, S} -> scenario_steps(Config, T, N+1, Given, When, Then, [{step_finally, N, S} | Finally], step_finally);
		{empty, _} -> scenario_steps(Config, T, N, Given, When, Then, Finally, empty);
		{unknown, Mystery} -> io:format("\t\tWARNING: No prefix match for ~p~n", [Mystery]), scenario_steps(Config, T, N+1, Given, When, Then, Finally, unknown)		
	end;
scenario_steps(_Config, [], N, Given, When, Then, Finally, _) ->
	% returns number of steps and breaks list into types, may be expanded for more times in the future!
	{N, Given, When, Then, Finally}.
	
% inspect system to ensure that we have not altered it
% this relies on the features implmenting the inspect meth
inspect(Config) ->
  Features = bdd_utils:features(Config),
  inspect(Config, [], [list_to_atom(bdd_utils:feature_name(Config,F)) || F <- Features]).

inspect(_Config, Result, []) -> Result;
inspect(Config, Result, [Feature | Features]) ->
  try apply(Feature, inspector, [Config]) of
		R -> R ++ inspect(Config, Result, Features)
	catch
		_X: _Y -> inspect(Config, Result, Features) % do nothing, we just ignore lack of inspectors
	end.
	
is_clean(Config) -> 
  {ok, [Inspect]} = file:consult("../tmp/inspection.list"),
  is_clean(Config, Inspect).
is_clean(Config, StartState) ->
  EndState = inspect(Config),
  Diff = lists:subtract(StartState, EndState),
  case Diff of
    []      -> true;
    % TODO - cleanup should tell you if the artifacts are from BEFORE or AFTER.  Right now, it is not clear!
    Orphans -> io:format("~nWARNING, Inspector Reports tests did NOT CLEANUP all artifacts!~n\tOrphans: ~p.~n",[Orphans]),
               false
  end.

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


%utilities to create step information from code
steps_output(RE, Step) ->
	{Type, S} = case re:run(Step, RE) of
	  {match, [_A, {T1, T2}, _B, {S1, S2} | _]} -> {string:substr(Step,T1+1, T2), string:substr(Step,S1+1, S2)};
	  nomatch -> {"other", Step}
	end,
	io:format("  * ~s ~s~n",[Type, S]).

steps() ->
  Files = filelib:wildcard("*.erl"),
  [steps(File) || File <- Files].
  
steps(File) ->
  io:format("* ~s~n",[File]),
  {ok, RawCode} = file:read_file(File),
	RawLines = string:tokens(binary_to_list(RawCode),"\n"),
	{ok, RE} = re:compile("\\{step_([a-z]*),(.*)\\[(.*)\\]\\}"),	
	StepLines = [S || S <- RawLines, string:substr(S,1,5) =:= [$s, $t, $e, $p, $( ]],
	[steps_output(RE, S) || S <- StepLines],
  io:format("~n").
  
