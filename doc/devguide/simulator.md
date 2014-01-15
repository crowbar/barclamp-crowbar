##  Crowbar Environment Simulator

> This topic uses the same infrastructure as the BDD test environment

You need a working [[devtool-build]] system.

### To use the simulator:

1. In your dev system, run the test server:

    `./dev tests server`

1. In a new window, start erlang

    <pre>'cd ~/crowbar/barclamps/crowbar/BDD'
    cp example.config default.config
    [review default.config and update if needed]
    cp dev.sample dev.config
    ./linux_compile.sh
    ./linux_sim.sh</pre>

1. Open the Crowbar UI using the URL `http://[dev system IP]:3000`.
2. You can then explore and even run the Annealer!

### Interactive Mode

You can also run the simulate interactively from `'erl'` using `'dev:pop().'` to create machines, and `'dev:unpop().'` to remove them.

You can change the nodes and other information created by the simulator by editing your copy of `'dev.config'`.
