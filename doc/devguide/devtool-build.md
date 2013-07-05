##  Dev tool development and build for Crowbar.ISO - for developers

Changes effective 9/14/2012: *Applies to Crowbar 1.5+ & Crowbar 2.x Deployment*

The Crowbar Dev Tool is a git overlay that helps manage Crowbar releases, barclamp integration, and gated checkins to the Crowbar source base.

This page documents how to use the dev tool to build the Crowbar ISO.  For more detailed documentation specifically on how to use the tool for regular development, see [README.dev-and-workflow](https://github.com/crowbar/crowbar/blob/master/README.dev-and-workflow) in the main crowbar git repository.

If you are planning to write code for Crowbar core or develop a barclamp, we highly recommend you follow the following steps to create your build server.  If you are NOT planning to edit Crowbar then include the `--no-github` flag or follow the read-only [[Build-Crowbar.ISO]] instructions.

Tip: Want to watch it on video? http://youtu.be/vIQLK4MXcqg

### Pre-requisites:

* A GitHub account (https://github.com/).  They are free for personal open source use.
* A 40GB Ubuntu 12.04.1 server (VM is ok) is recommended for this process!  
   * `sudo apt-get update` before taking any other steps (+ coffee break)
   * You'll like want to `sudo apt-get install openssh-server` so you can access it
* It is important to use the dev tool for managing branches & releases because it:
   * takes care of synchronization with all the barclamps
   * builds nested tags with the release information
* A copy of your target Crowbar OS.  
   * This page uses Ubuntu 12.04.1 server x64 iso as the example
* `sudo sed -ie "s/%admin ALL=(ALL) ALL/%admin ALL=(ALL) NOPASSWD: ALL/g" /etc/sudoers`
   * note: this will allow passwordless sudo for users in the admin group. The build process performs some high level kung-fu, some of which requires root access. if running as non-root, you'll be prompted for password unless you setup Passwordless SUDO permissions for your user. 
   * if the above does not work, using your username instead of 'crowbar', try `echo 'crowbar ALL=(ALL) NOPASSWD: ALL' >/etc/sudoers.d/crowbar; chmod 400 /etc/sudoers.d/crowbar`
   * Alternatively run the build as root.


### Build Steps:

_note:_ use sudo as instructed.  Do not use the Dev tool as root!

1. If you don't want to use HTTPS, then make sure that your build server's public key is registered with your Github account
1. `sudo apt-get update`
1. the following packages are needed
   1. `sudo apt-get install git rpm ruby rubygems1.8 curl build-essential debootstrap`
   1. `sudo apt-get install mkisofs binutils markdown erlang debhelper python-pip`
   1. `sudo apt-get install build-essential libopenssl-ruby1.8 libssl-dev zlib1g-dev` 
1. For Trunk CB2 Dev on Ruby 1.9 you need the following (do NOT do this for 1.x dev work!)
   1. `sudo update-alternatives --config ruby` (to make Ruby 1.9.1 the default. ruby -v will report version 1.9.3)
   1. `sudo update-alternatives --config gem` (to make Gem 1.9 the default, gem -v will report version 1.9)
   1. `sudo apt-get install ruby1.9.1-dev` 
   1. `sudo gem install builder bluecloth`
   1. continue with steps below
1. `sudo gem install json net-http-digest_auth kwalify bundler rake rcov rspec`
  1. note: rcov does not work in ruby-1.9 - use 'simplecov' instead
1. `git clone https://github.com/crowbar/crowbar.git`
1. `cd ~/crowbar` directory
1. `./dev setup`
   1. You can ignore warnings: "ulimit: open files: cannot modify limit: Invalid argument"
   1. this will create personal github forks for you of all the Crowbar modules. If you don't have a Github login, or you don't care about pushing changes back upstream, you can run `./dev setup --no-github`
1. optional, if this is a new system, set your user information as directed 
   1. git config --global user.name "Your Name"
   1. git config --global user.email you@example.com
1. `./dev fetch`
1. ` sudo ./build_sledgehammer.sh`
   1. this builds the "discovery image" called Sledgehammer.  
   1. this step take a long time.  It helps if you've pre-fetched the CentOS ISO (see [[Build-Crowbar.ISO]])
   1. It does not change often and does not need to be repeated once it has been built.
1. sudo ln -s /usr/share/debootstrap/scripts/gutsy /usr/share/debootstrap/scripts/precise
   1. note: This is required only if: You're building for 12.04 on a 11.04 or 11.10 machine, since those versions don't know how to build Precise
1. Make sure you have the right ISOs in `~/.crowbar-build-cache/iso` directory
   1. For Ubuntu, you should have http://old-releases.ubuntu.com/releases/12.04.1/ubuntu-12.04.1-server-amd64.iso
      1. `cd ~/.crowbar-build-cache/iso`
      1. `wget http://old-releases.ubuntu.com/releases/12.04.1/ubuntu-12.04.1-server-amd64.iso`
1. `./dev switch development`
   1. you can see all the releases with ./dev releases
   1. choices trunk (default, trunk), 1.2 = fledermaus, 1.3 Cloudera = elefante
1. `./dev checkout master` 
   1. you can see all the branches with git branch -a
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
