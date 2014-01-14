##  Crowbar Environment Simulator

> This topic uses the same infrastructure as the BDD test environment

You need a working [[devtool-build]] system.

### To use the simulator:

In your dev system, run the test server:

    ./dev tests server

In a new window:

    cd ~/crowbar/barclamps/crowbar/BDD
    cp example.config default.config
    Edit default.config
    Search for http:
    Change the IP address in the URL to the IP of your dev system
    Save the file
    cp dev.sample dev.config
    ./linux_compile.sh
    ./linux_sim.sh

Open the Crowbar UI under 'http://[dev system IP]:3000'
You can then explore and even run the Annealer!

### Interactive Mode

You can also run the simulate interactively from 'erl'.  Perform all of the above steps, but instead of running linux_sim.sh, do the following:
    erl
    dev:pop().

To remove all of the nodes:
    dev:unpop().

Note that nodes can only be removed with "dev:unpop." if "dev:pop." has been run first.  You can run "dev:pop." after running linux_sim.sh, and it will find the nodes instead of creating new ones.

You can change the nodes and other information created by the simulator by editing your copy of 'dev.config'.
