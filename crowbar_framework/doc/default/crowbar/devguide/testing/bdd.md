### BDD Testing

Crowbar includes a Business Driven Development (BDD) framework written in Erlang that is based on the Cucumber DSL.

The intent of these tests are to focus on the responses and requests to the web-interface and RESTful API.

### Using the BDD tool

1. `cd /opt/dell/crowbar_framework/BDD`
1. `linux.sh` or `Win7.bat` to compile the erlang code depending on your platform (may give an error, that's ok)
1. `erl` to start a command shell for erlang
  1. `bdd:test().` will run all the tests and report the results
  1. `bdd:feature("name").` will run just the named feature set

> You can run `bdd:test("profile").` or `bdd:feature("profile","feature").` if you want to use an alternate profile than `default`.  Alternate profiles use the matching configuration name and had a different global setup/teardown location.

### BDD DSL

The BDD domain specific language (DSL) that's designed to be very natural language like.  There are 3 primary clauses in the DSL:

1. **Given** ... some background thing has happened
1. **When** ... take some action
1. **Then** ... get some result

### Writing Feature Tests

Test files all end with the extension `.feature` and contain "plain English" scripts for testing features.  This is know as the BDD DSL (domain specific la language).  While it looks like plain language, it is very specifically mapped into the testing framework and _must_ follow the DSL guidelines.

A feature file (in the `features` directory) is broken into specific "scenarios" to be tested.  Each scenario is effectively a test and has multiple steps.  They all start with a known state expressed using given or when instructions.  The state is then tested using then checks.  The concept is to mirror actions that a user takes: when the user goes this action then they should see this results.  Yes, it's that simple!

A scenario must include a when statement but the given is optional.  Given is used to setup a scenario before the when action is taken.  This is very important for testing linking from a page.  For example, _given_ that I'm on the nodes list page _when_ I click on the all link _then_ I should a list that includes the admin node.  BDD's goal is to turn those types of directives into tests.

#### HTML Tests

The following sentences can be used for testing HTML web pages where you can change the information in "quotes".

* Given I am on the home page
* Given I am on the "dashboard" page
* When I go to the home page
* When I go to the "node/2.0/1" page
* When I click on the "Dashboard" menu item
* Then I should see "Full Name"
* Then there should be no translation errors
* Then I should not see "Error"

> Note: This is _not_ a complete list!

#### AJAX Tests

The following sentences can be used for testing JSON AJAX API calls where you can change the information in 

* When AJAX requests the "node/status/2.0" page
* Then key "fingerprint" should be a number
* Then key "[nodes][admin][state]" should be "Ready"
* Then key "count" should be "0"
* Then key "[groups][0]" should contain "7" items

> Note: This is _not_ a complete list!

### Extending the DSL

Each feature definition can add it's own ERL step parser.  The BDD system will automatically search for a step definition based on the feature name.  It will also automatically search the "crowbar" and "webrat" steps.  

The webrat steps are designed to be general purpose Web access checks.  If you find yourself doing a routine HTTP or AJAX request then it likely belongs in the webrat file instead of your feature steps.

You can add custom step files from the config page.


#### Steps

All of the BDD tests decompose into the same Erlang method, known as the `step` method. The BDD test engine will search in multiple Erlang code files for steps in a specific order.  

   > The steps have been defined to ensure that the last file tried (`bdd_catchall`) contains steps that have been defined to match all possible cases.  If the final catchall step is reached, that step will generate a stub step that you can use to create a new step.

A step is a standard Erlang function with 3 parameters:

1. The BDD configuration file
1. The pass forward file that is represents the accumulated output of previous steps
1. The DSL tuple populated by BDD as follows
   1. keyword (setup, teardown, given, when, or then)
   1. step number
   1. list of the DSL string tokenized by quotes

  > If you are terrified by the phrase "tokenized by quotes" then relax.  That just means that BDD turns your friendly `when I click on link "foo"` into an Erlang list that is super easy to parse: ["when I click on link", Foo].  

Let's look at an example step:

    step(Config, _Global, {step_given, _N, ["I went to the", Page, "page"]}) ->
        bdd_utils:http_get(Config, Page);

This step will match the DSL `Given I went to the "dashboard" page` in the scenario.  It simply does an HTTP get using the BDD utilities.  The `http_get` routine takes the base URL from the config file and adds the page information from the sentence.  BDD will take the result of this step function and add it to the `Given` list that is passed into all following 'when' steps.

  > Reminder: Erlang variables that start with _ are considered optional and don't throw a warning if they are not used.  If you plan to use those variables, you can use keep the _; however, I recommend removing the underscore for clarity.

There is a simple output expectation from all steps:

* setup steps add to the Global list that is passed into Given steps
* given steps add to the Given list that is passed into When steps
* when steps add to the Results list that is passed into the Results steps
* results steps return true if the test passes or something else if it fails

One of the most important step files is known as "webrat" as a hold over from Cucumber.  The `bdd_webrat` file contains most of the HTML & AJAX routines you will ever need for routine testing.  It is also a great place to look for examples of step programming.

#### Adding Pre & Post Conditions

To add pre/post-configuration for a Feature file, you must have an Erlang step file with the same name as the feature file.  For example, if you have a feature called `nodes.feature` then you must have a `nodes.erl` to create setup and tear down steps for that feature file.

Setup Steps use the `step_teardown` atom:

    step(Config, _Global, {step_setup, _N, _}) -> 
      io:format("\tNo Feature Setup Step.~n"),
      Config;

> Setup step stores adds results to the Config file.  You should use `[{item, value} | Config]` to ensure that your values get added to the Config list and are available for the features' steps.
> _Global is always an empty list (`[]`) for setup steps
_
Teardown Steps use the `step_teardown` atom:

    step(Config, _Global, {step_teardown, _N, _}) -> 
      io:format("\tNo Feature Tear Down Step.~n"),
      Config;

To perform actions, replace or augment the code in the steps to perform the needed operations.  The from the Setup action is added to the `Global` list that is passed into all the steps called within the feature.  This allows you to reference items created in setup during subsequent tests.  You should remember to unwind any action from the setup in the teardown.

For example, the Nodes feature setup and tear down look like this:

    step(Config, _Global, {step_setup, _N, _}) -> 
      Path = "node/2.0",
      Node1 = "BDD1.example.com",
      % just in case, cleanup first
      http_delete(Config, Path, Node),
      % create node(s) for tests
      Node = node_json(Node1, "BDD Testing Only", 100),
      Result = http_post(Config, Path, Node),
      {"id", Key} = lists:keyfind("id",1,Result),
      io:format("\tCreated Node ~p (id=~p) for testing.~n", [Node1, Key]),
      [{node1, Key} | Config];
    
    step(Config, Global, {step_teardown, _N, _}) -> 
      % find the node from setup and remove it
      {"node1", Key} = lists:keyfind("node1", Global),
      http_delete(Config, Path, Key),
      io:format("\tRemoved Node ID ~p for Tear Down Step.~n", [Key]),
      Config;

### Debugging

Some handly Erlang tips:

* `Config = bdd:getconfig("crowbar")` will load the configuration file for passing into Step routines for manual testing

### BDD Code Files

* bdd - contains the core running logic
* bdd_utils - utilities used across all modules of the bdd system
* eurl - HTTP get, post, delete functions (like curl)
* json - JSON parser converts to and from lists
* digest_auth - Wrapps http to provide secure access
* bdd_catchall - last step file executed, has fall back steps
* bdd_webrat - handles most basic web & AJAX based steps
* default - the fall back step file (global setup/teardown goes here)
* crowbar - Crowbar specific logic
* [feature] - Each feature can have a specific step file

### Test Files

Each barclamp is expected to add it's own tests to the suite

The Crowbar barclamp tests include:

* Dashboard.features
   * Tests the nodes UI view
* Documentation.features
   * Tests the documentation / help system
* Navigation.features
   * Tests the basic menu system
   * Checks for localization omissions
* Proposal.features
   * Tests the Proposal Status API
* Nodes.features
   * Tests the node status API
   * Tests the node detail page & API
* Scaffolds.features
   * Tests all the feature objects
