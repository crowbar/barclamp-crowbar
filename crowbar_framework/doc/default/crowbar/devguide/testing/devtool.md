### Dev Tool Helpers 

The dev tool has been updated to allow for running these tests from in a build environment.  

The following gems are pre-requisite (must run as root)
* `sudo -i`
* `apt-get install sqlite3`
* `gem install rake`
* `gem install bundler`
* `gem install sqlite3`

The following commands work for Unix environments:
1. `./dev setup-unit-tests`   # builds a unit test environment in /tmp/crowbar
1. `./dev reload-unit-tests`  # builds fixtures and migrates data for /tmp/crowbar
1. `./dev run-unit-tests`     # executes the unit tests and BDD tests
1. `./dev clear-unit-tests`   # Removes the /tmp/crowbar environment

For debugging, the /tmp/crowbar environment can be used as a rails app.  The server can be run by running:
* `script/rails s Puma`

