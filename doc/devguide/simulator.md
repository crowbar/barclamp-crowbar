##  Crowbar Environment Simulator

> This topic uses the same infrastructure as the BDD test environment

You need a working [[devtool-build]] system.

### To use the simulator:

1. In your dev system, run the test server:

    `./dev tests server`

1. In a new window:

    <pre>cd ~/crowbar/barclamps/crowbar/BDD
    cp example.config default.config</pre>
    Edit default.config
    Search for http:
    Change the IP address in the URL to the IP of your dev system
    Save the file
    <pre>cp dev.sample dev.config
    ./linux_compile.sh
    ./linux_sim.sh</pre>

1. Open the Crowbar UI using the URL `http://[dev system IP]:3000`.
1. You can then explore and even run the Annealer!

### Interactive Mode

You can also run the simulate interactively from 'erl'.  Perform all of the above steps, but instead of running linux_sim.sh, do the following:

<pre>
    erl
    dev:pop().
</pre>

To remove all of the nodes:

<pre>
    dev:unpop().
</pre>

Note that nodes can only be removed with `dev:unpop().` if `dev:pop().` has been used to create the nodes.  You can run `dev:pop().` after running linux_sim.sh, and it will find the nodes instead of creating new ones.

You can change the nodes and other information created by the simulator by editing your copy of `dev.config`.
