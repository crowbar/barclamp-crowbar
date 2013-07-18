##  Build your own Crowbar ISO for Hadoop or OpenStack

Building Crowbar is not an activity intended for users. If you simply want to download an ISO to either deploy Hadoop or OpenStack, I suggest you get one of our [pre-compiled ISOs](http://crowbar.github.io/download/).

Crowbar was originally designed to operate without any internet access, and since it includes everything needed to deploy a complete cluster, the below process will walk you through building an entire ISO which will contain all the bits in one package. 

Unlike many projects Crowbar spans many repositories, which almost all the time need to be managed simultaneously. This means that branches or pull requests often times impact multiple repositories at once. 
In order to more easily manage this, we've created [a tool called the Dev Tool](https://github.com/crowbar/crowbar/blob/master/README.dev-and-workflow), which makes some of these common tasks easier.

**Note:** This page documents how to use the Dev Tool to create a Crowbar ISO.  For more detailed documentation specifically on how to use the tool for regular development, how to submit pull-requests, how releases are defined, please refer to the [README.dev-and-workflow](https://github.com/crowbar/crowbar/blob/master/README.dev-and-workflow).


### Pre-Requisites:

* A physical or virtual Ubuntu 12.04.2 server (you will need root access)
* An ISO of your target Crowbar OS (see instructions below on where they'll go)
   * Use the [12.04.2 server ISO](http://releases.ubuntu.com/precise/ubuntu-12.04.2-server-amd64.iso) for **OpenStack** builds
   * Use [CentOS 6.4 DVD ISO](http://mirrors.seas.harvard.edu/centos/6.4/isos/x86_64/CentOS-6.4-x86_64-bin-DVD1.iso) for **Hadoop** builds


### The Dev Tool and GitHub

Since all the development on Crowbar takes place on GitHub, the Dev Tool is absolutely reliant on having a reliable connection to GitHub. If you ever see console output like this: `......!!!.....!!!!.` while Dev Tool is at work, it indicates connection problems. Also, if you don't want to use HTTPS, make sure that your build server's public key is registered with your GitHub account.

If you intend on doing _development_ which might result in you submitting pull- requests, you will also need a GitHub account .  **Note:** Using your GitHub account will result in every needed Crowbar repo being forked to your account. During development it is important to use the Dev Tool for managing branches and releases.

**If you don't have any intentions on submitting pull-requests, you can append any of the Dev Tool commands below with `--no-github`.**


### Preparing Our Build Box

#### Setup password-less sudo
During the build process the Dev Tool has to perform certain tasks which require root access (mounting ISOs, etc.). In order to avoid being prompted for your password every time we will setup password-less sudo. **Don't run your build as root.**

    # run this command to add your 
    sed -ie "s/%sudo\tALL=(ALL:ALL) ALL/%sudo ALL=(ALL) NOPASSWD: ALL/g" /etc/sudoers


#### Install needed packages and gems

    # let's install some OS packages
    sudo apt-get update
    sudo apt-get install git rpm ruby rubygems1.8 curl build-essential debootstrap \
    mkisofs binutils markdown erlang debhelper python-pip \
    build-essential libopenssl-ruby1.8 libssl-dev zlib1g-dev

    # let's install some needed gems next
    sudo gem install json net-http-digest_auth kwalify bundler rake rcov rspec --no-ri --no-rdoc


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
    # fetch updates from configured upstream repositories
    ./dev fetch
    # synchronize fetched updates with the local repos
    ./dev sync

**Notes:** 
  * As [mentioned above](#the-dev-tool-and-github) Dev Tool can be very taxing on GitHub and your connectivity. This means you may need to run the `./dev setup; ./dev fetch; ./dev sync` commands more than once to completely setup, fetch and sync all the repos needed.  Interate these commands a few times until there is no further change.  This normally takes two or three cycles when first setting up a build environment.
  * Ignore potential warnings like this: `ulimit: open files: cannot modify limit: Invalid argument` 

#### Building the discovery image
During the cluster deployment Crowbar uses a special stripped down image (Sledgehammer) for node discovery. As part of our build process we also need to build Sledgehammer. This is a one time process and doesn't need to be repeated everytime. **Note:** This next step will take some time.

    cd ~/crowbar
    sudo ./build_sledgehammer.sh


#### Picking our release and build
Now that everything is setup and prepped, the last remaining step is to pick what we actually want to build. Several Crowbar versions and flavors (Hadoop and OpenStack) are available. Next we're picking which version/release and flavor/build we'd like to build. **It is extremely important to pick the right release and build combination.** Some of the releases will be under heavy development and others will be old. If you're unsure which one is the right one for you you should ask via [IRC or mailing list](http://crowbar.github.io/docs/getting-help.html).

    cd ~/crowbar

    # working with releases
    # show all the available releases
    ./dev releases

    # switch to a release
    ./dev switch mesa-1.6

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
  * **OpenStack:** mesa-1.6/openstack-os-build for the latest and most stable OpenStack build based on Grizzly.
  * **Hadoop:** hadoop-2.3/hadoop-os-build for the latest and most stable Hadoop build

#### Building
With the above knowledge we can now kick off our Hadoop or OpenStack build. 

**Note:** The Dev Tool currently has a bug where it litters README.empty-branch files. Those will be problematic during `dev switch` operations. In order to **clean them up run the following commands** any time you run into this issue.

    # clean up any .empty-branch files first
    cd ~/crowbar/barclamps
    for bc in *; do (cd "$bc"; git clean -f -x -d 1>/dev/null 2>&1; git reset --hard 1>/dev/null 2>&1); done 

Now we can actually kick off the build.

    # FOR HADOOP
    ./dev switch hadoop-2.3/hadoop-os-build
    ./dev build --os centos-6.4 --update-cache

    # FOR OPENSTACK
    ./dev switch mesa-1.6/openstack-os-build
    ./dev build --os ubuntu-12.04 --update-cache



#TODO:
**Anything below this line should be ignored, since it hasn't been rewritten edited yet.**

   1. choices are `master` (default, trunk), `openstack-os-build` (OpenStack), `cloudera-os-build` (Hadoop)
   1. different branches represent different "distros" which combine different sets of barclamps (e.g. openstack, hadoop)
1. `./dev build --os ubuntu-12.04 --update-cache` 
   1. The parameter --update-cache will create your Build Cache at this time, thus the first run will download from the internet what it needs.  This takes time and can be skipped for later builds.  Dev Tool will tell you if your cache is need to be updated
   1. you can add --release development --branch master if you did not switch or checkout 

### Special Notes for Using a Build VM
If you are using VMware Worktation (or similar) then these extra tips will help you improve your build experience

Tips:

* VMware tools are not working on Ubuntu 12.04 at this time (5.5.2012) so use 11.10
* To save space on your VM, you can use ISOs that are local to your host
   * create a shared file location (e.g.: c:\isos) mounted to "isos"
   * `vi ~/.build-crowbar.conf`
   * add a reference to `ISO_LIBRARY=/mnt/hgfs/isos`
* To make it easier to build VMs from the build ISO, have the build save it directory to your host's drive
   * create a shared file location (e.g.: c:\temp\crowbar) mounted to "crowbar"
   * `vi ~/.build-crowbar.conf`
   * add a reference to `ISO_DEST=/mnt/hgfs/crowbar`
* If you have several build VMs, you may want to share your crowbar cache from the host
* If you have several active work areas, you can pull down your work-in-progress from github after a './dev backup' by using 'git checkout remotes/personal/[branch e.g. master]' from the impacted barclamp(s).

### Building Barclamp TARs only

If you only want barclamps for import (Utils\Import) then you should use the "--no-iso" flag with the build command.  Note that you must still provide an operating system!

You can find the tars in ~/.crowbar-build-cache/ubuntu-12.04/build/dell/barclamps where the OS will match the one that you choose to target.

See [[Packaging-Barclamps]] about building individual barclamps.

### Coding Steps

We recommend you consult the ./dev command line help, and also [README.dev-and-workflow](https://github.com/crowbar/crowbar/blob/master/README.dev-and-workflow) in the crowbar repository.

1. ./dev backup
   1. makes a copy into your personal forks (if you did not have any, you do now!)
1. ./dev fetch
   1. fetches changes from remote repositories, but does not make any local changes.
1. ./dev sync
   1. Merges changes fetched with ./dev fetch into your local branches.
1. commit your code
   1. git add
   1. git commit
1. ./dev is_clean
1. ./dev pull-requests-prep
   1. it will give you instructions on next steps to automatically submit a push request on ALL impacted barclamps


### Build your cache
A Crowbar build contains the packages (e.g. deb,rpm,gem etc) that will be used to install your cloud. To avoid having to download the Internet on every build, the Crowbar build system maintains a local cache in ~/.crowbar-build-cache by default.
Additionally, Crowbar utilizes a customized LiveCD image, SledgeHammer. You can build that too (sources on github), but it changes very infrequently - so you're better of snatching it from somewhere.
