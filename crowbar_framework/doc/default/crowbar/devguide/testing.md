## Testing Crowbar

Crowbar includes a Business Driven Development (BDD) framework written in Erlang that is based on the Cucumber DSL.

### Using the BDD tool

1. `cd /opt/dell/crowbar_framework/BDD`
1. `linux.sh` to compile the erlang code
1. `erl` to start a command shell for erlang
  1. `bdd:test("crowbar").` will run all the tests and report the results
  1. `bdd:feature("crowbar","name").` will run just the named feature set

### BDD DSL

The BDD domain specific language (DSL) that's designed to be very natural language like.  There are 3 primary clauses in the DSL:

1. **Given** ... some background thing has happened
1. **When** ... take some action
1. **Then** ... get some result

### Writing Feature Tests

### Extending the DSL

### Adding Pre Conditions

### Adding Post Conditions
