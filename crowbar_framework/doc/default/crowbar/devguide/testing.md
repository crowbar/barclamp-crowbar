## Testing Crowbar

Crowbar includes a Business Driven Development (BDD) framework written in Erlang that is based on the Cucumber DSL.

### Using the BDD tool

1. `cd /opt/dell/crowbar_framework/BDD`
1. `linux.sh` or `Win7.bat` to compile the erlang code depending on your platform (may give an error, that's ok)
1. `erl` to start a command shell for erlang
  1. `bdd:test("crowbar").` will run all the tests and report the results
  1. `bdd:feature("crowbar","name").` will run just the named feature set

### BDD DSL

The BDD domain specific language (DSL) that's designed to be very natural language like.  There are 3 primary clauses in the DSL:

1. **Given** ... some background thing has happened
1. **When** ... take some action
1. **Then** ... get some result

### Writing Feature Tests

Test files all end with the extension `.feature` and contain "plain English" scripts for testing features.  This is know as the BDD DSL (domain specific la language).  While it looks like plain language, it is very specifically mapped into the testing framework and _must_ follow the DSL guidelines.

A feature file is broken into specific "scenarios" to be tested.  Each scenario is effectively a test and has multiple steps.  They all start with a known state expressed using given or when instructions.  The state is then tested using then checks.  The concept is to mirror actions that a user takes: when the user goes this action then they should see this results.  Yes, it's that simple!

A scenario must include a when statement but the given is optional.  Given is used to setup a scenario before the when action is taken.  This is very important for testing linking from a page.  For example, _given_ that I'm on the nodes list page _when_ I click on the all link _then_ I should a list that includes the admin node.  BDD's goal is to turn those types of directives into tests.

### Extending the DSL

Each feature definition can add it's own ERL step parser.  The BDD system will automatically search for a step definition based on the feature name.  It will also automatically search the "crowbar" and "webrat" steps.  

The webrat steps are designed to be general purpose Web access checks.  If you find yourself doing a routine HTTP or AJAX request then it likely belongs in the webrat file instead of your feature steps.

You can add custom step files from the config page.

### Adding Pre Conditions

TBD

### Adding Post Conditions

TBD

### Test Files

Each barclamp is expected to add it's own tests to the suite

The Crowbar barclamp tests include:

* Dashboard.features
* Navigation.features
   * Tests the basic menu system
   * Checks for localization omissions
* Proposal.features
   * Tests the Proposal Status API