##  Build your own Crowbar ISO for 2.x

**Note:** Crowbar 2.x is currently not fully functional. As a result
the instructions below are only to be used by developers intending to
contribute to the effort.

Building Crowbar is not an activity intended for users. If you simply want
to download an ISO to either deploy Hadoop or OpenStack, I suggest you
get one of our [pre-compiled ISOs](http://crowbar.github.io/download/).

Crowbar was originally designed to operate without any internet access,
and since it includes everything needed to deploy a complete cluster,
the below process will walk you through building an entire ISO which
will contain all the bits in one package.

Unlike many projects Crowbar spans many repositories, which almost all
the time need to be managed simultaneously. This means that branches
or pull requests often times impact multiple repositories at once.
In order to more easily manage this, we've created [a tool called the Dev
Tool](https://github.com/crowbar/crowbar/blob/master/README.dev-and-workflow),
which makes some of these common tasks easier.

**Note:** This page documents how to use the Dev Tool to create
a Crowbar ISO.  For more detailed documentation specifically
on how to use the tool for regular development, how to submit
pull-requests, how releases are defined, please refer to the
[README.dev-and-workflow](https://github.com/crowbar/crowbar/blob/master/README.dev-and-workflow).

### The Dev Tool and GitHub
 
Since all the development on Crowbar takes place on GitHub, the Dev Tool
is absolutely reliant on having a reliable connection to GitHub. If you
ever see console output like this: `......!!!.....!!!!.` while Dev Tool
is at work, it indicates connection problems. Also, if you don't want to
use HTTPS, make sure that your build server's public key is registered
with your GitHub account.

If you intend on doing _development_ which might result in you submitting
pull- requests, you will also need a GitHub account .  

**Note:** Using your GitHub account will result in every needed Crowbar
repo being forked to your account. During development it is important
to use the Dev Tool for managing branches and releases.

**If you don't have any intentions on submitting pull-requests, you can
append any of the Dev Tool commands below with `--no-github`.**

### Pre-Requisites:

These directions depend on having a build environment already setup. 
The requirements are:

1. A Physical or virtual build environment for one of the supported build
   environments. Refer to the 
   [general VM documentation](./development/dev-vm.md) and one of the following
   OS specific setup descriptions:

   * [Ubuntu 12.04](./development/dev-vm-Ubuntu.md) (reference environment, works.)
   * [Fedora 19](./development/dev-vm-Fedora.md) (in progress, should work but expect glitches.)
   * [SUSE 11](./development/dev-vm-SUSE.md) (expect some glitches.)

2. An ISO of your target Crowbar OS 
   (see instructions below on where they'll go)

   * Use the [12.04.2 server ISO](http://old-releases.ubuntu.com/releases/12.04.2/ubuntu-12.04.2-server-amd64.iso) for **OpenStack** builds
   * Use [CentOS 6.4 DVD ISO](http://mirrors.seas.harvard.edu/centos/6.4/isos/x86_64/CentOS-6.4-x86_64-bin-DVD1.iso) for **Hadoop** builds

### Preparing Our Build Box

#### Put the needed base ISOs in place
As a starting point for the build process we will need the ISOs [mentioned above](#pre-requisites) placed where the Dev Toll can find them. The Dev Tool will then use them during the build process described below.

    mkdir -p ~/.crowbar-build-cache/iso
    cd ~/.crowbar-build-cache/iso
    # now scp or wget the ISOs into this directory

### Building Our ISO

#### Checking out code and setting up
This next step will checkout the core Crowbar code, which includes the Dev Tool. Once the initial checkout is complete, we let the Dev Tool handle the checkout of the remaining repositories. As a result this next process will create personal forks of each Crowbar repository. If you have no interest of submitting pull requests append `--no-github`. **Note:** This next step will take some time.

    cd ~
    git clone https://github.com/crowbar/crowbar.git
    cd ~/crowbar
    # append ./dev setup --no-github if you don't want to submit any pull-requests
    ./dev setup
    # If this is your first build on the server then you will need to register your github name and email 
    git config --global user.name "user.name"
    git config --global user.email "email.address"
    # fetch updates from configured upstream repositories
    ./dev fetch
    # synchronize fetched updates with the local repos
    ./dev sync

**Notes:** 
* As [mentioned above](#the-dev-tool-and-github) Dev Tool can be very taxing on GitHub and your connectivity. This means you may need to run the `./dev setup; ./dev fetch; ./dev sync` commands more than once to completely setup, fetch and sync all the repos needed.  Interate these commands a few times until there is no further change.  This normally takes two or three cycles when first setting up a build environment.
* Ignore potential warnings like this: `ulimit: open files: cannot modify limit: Invalid argument` 

If you receive any messages stating "new file:   README.empty-branch" and the ./dev sync fails then run the following commands 

    cd barclamps
    for bc in *; do (cd "$bc"; git checkout master; git reset HEAD README.empty-branch); done
    cd ..
    find / -name README.empty-branch -delete

#### Running the test suites

We've put a lot of effort into creating test suites:

   ./dev tests setup --update-gem-cache

##### Building the discovery image
During the cluster deployment Crowbar uses a special stripped down image (Sledgehammer) for node discovery. As part of our build process we also need to build Sledgehammer. This is a one time process and doesn't need to be repeated everytime. **Note:** This next step will take some time.

    cd ~/crowbar
    ./build_sledgehammer.sh


#### Picking our release and build

Now that everything is setup and prepped, the last remaining step is
to pick what we actually want to build. Several Crowbar versions and
flavors (Hadoop and OpenStack) are available. Next we're picking which
version/release and flavor/build we'd like to build. 

**It is extremely
important to pick the right release and build combination.** Some of
the releases will be under heavy development and others will be old. If
you're unsure which one is the right one for you you should ask via
[IRC or mailing list](http://crowbar.github.io/docs/getting-help.html).

    cd ~/crowbar

    # working with releases
    # show all the available releases
    ./dev releases

    # switch to a release
    ./dev switch developmnet
    
    # display your current release
    ./dev release


    # working with builds
    # show all the builds
    ./dev builds

    # choosing a certain build
    ./dev build openstack-os-build

    # display which build your on
    ./dev build

The above results in the following viable combinations:

* **Basic Crowbar 2.0 Development:**
  development/openstack-os-build is the latest 
  The pre-2.0 releases require completely different versions of Ruby and gems and other apps.
  You cannot build them in the envioronment you just setup.
* **OpenStack:** 
  mesa-1.6/openstack-os-build for the latest and most stable OpenStack build based on **Grizzly**.
* **OpenStack:** 
  mesa-1.6.1/openstack-os-build for the latest and most stable OpenStack build based on **Havana**.
* **Hadoop:** 
  hadoop-2.3/hadoop-os-build for the latest and most stable Hadoop build

#### Building
With the above knowledge we can now kick off our Hadoop or OpenStack build. 

**Note:** The Dev Tool currently has a bug where it litters README.empty-branch files. Those will be problematic during `dev switch` operations. In order to **clean them up run the following commands** any time you run into this issue.

    # clean up any .empty-branch files first
    cd ~/crowbar/barclamps
    for bc in *; do (cd "$bc"; git clean -f -x -d 1>/dev/null 2>&1; git reset --hard 1>/dev/null 2>&1); done 


