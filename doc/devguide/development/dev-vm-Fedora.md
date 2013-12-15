# Crowbar Development Environment Using Fedora

## Setting Up the Virtual Machine (VM)

1. Create a new virtual network 192.168.124.0/24 (using virt-manager or virsh).
   Do not use DHCP for this network.

1. Start with a fresh Fedora 19 VM (this can be a minimal install).

1. Assign the previously created virtual network to the VM and configure the
   VM's networking like this:

   <pre>IP address: 192.168.124.10
   Netmask:    255.255.255.0
   Gateway:    192.168.124.1
   DNS:        192.168.124.1</pre>

   (You will need to edit `/etc/sysconfig/network-scripts/ifcfg-eth0` and
   `/etc/resolv.conf` to achieve this on a VM that was configured differently
   during installation.)

1. Connect to the machine via SSH and try to ping some address to verify that
   traffic gets routed correctly and DNS works.

   <pre>kvm-host> ssh root@192.168.124.10</pre>

1. [Optional] If you have problems with outbound connections from VM even after
   editing network-scripts and resolv.conf, it might be that iptables
   forwarding rules for the virtual network didn't get created on your host
   machine. Check that with iptables:

   <pre>kvm-host> sudo iptables -L

   Chain FORWARD
   target     prot opt source               destination
   ACCEPT     all  --  anywhere             192.168.124.0/24     state RELATED,ESTABLISHED
   ACCEPT     all  --  192.168.124.0/24     anywhere

   (The output is shortened to show the important part only.)</pre>

   If you don't see the above forwarding rules, shut down your VMs and restart
   libvirtd:

   <pre>kvm-host> systemctl restart libvirtd.service</pre>
   
   Then check the iptables output again and the forwarding rules should be there.

1. Create a user on the VM for Crowbar development.

   root@crowbar-dev> useradd -m crowbar
    root@crowbar-dev> passwd crowbar

## Setting up the development environment

Before beginning to set up a development environment, you should have
a working VM that with a build user created.  These directions use 'crowbar'
as the build user.

The general requirements are:

1. You should be able to access the machine from the host with ssh as the build 
   user.
1. The build user has passwordless sudo access enabled.
1. The machine has outbound acess via http for downloading packages,operating 
   system images, and Ruby gems, including any necessary firewall and 
   proxy setup.
1. The machine has access to github for fetching code.
1. The machine has approximately 40Gb of free disk space 
   ( 15 Gb - operating system images, 10 Gb for output isos, 15 Gb for 
   build cache

After verifying these requirements, you can begin setting up the development 
environment.

### Core development environment 

Install the core packages needed for development:

    sudo yum install git
    sudo yum install gcc gcc-c++
    sudo yum install mkisofs binutils erlang debootstrap  dpkg  dpkg-dev dh-make
    sudo yum install libxml2-devel libxslt-devel
    sudo yum install sqlite sqlite-devel

> Note: the Ubuntu instructions call for the markdown package.  Fedora 19 has 
> either python-markdown or python-markdown2. not sure which of these is 
> appropriate yet.

#### Ruby installation and configuration

Fedora 19 supports Ruby 2.0 as it's standard, and we need Ruby 1.9.  The best
way to set up Run 1.9 is to use the 
[Ruby Version Manager, rvm]( https://rvm.io/). 

As the build user, not root:

    curl -sSL https://get.rvm.io > rvm_setup
    sudo bash rvm_setup stable
    sudo usermod -G rvm mikeyp

Log out and back in to pick up the new Ruby environment and for the groups 
change to take effect.

    rvm install 1.9.3

Now, configure the correct ruby using the alternatives system. Note,
The commands below that reference /bin/ruby will fail if the standard
Ruby is not installed; you can ignore those errors.

    sudo rm /bin/ruby
    sudo rm /bin/gem
    sudo update-alternatives --install /bin/ruby ruby /usr/local/rvm/rubies/ruby-1.9.3-p484/bin/ruby 1
    sudo update-alternatives --install /bin/gem gem /usr/local/rvm/rubies/ruby-1.9.3-p484/bin/gem 1
    sudo update-alternatives --install /bin/ruby ruby /usr/bin/ruby 1
    sudo update-alternatives --install /bin/gem gem /usr/bin/gem 1
    sudo update-alternatives --config ruby
    # choose the rvm ruby
    sudo update-alternatives --config gem
    # choose the rvm gem
 
#### Postgres installation and configuration

Fedora 19 supports PostgreSQL 9.2 as it's standard package, and we need 
PostgreSQL 9.3, which is available from the postgres repositories.

Add the postgres repository, and install and configure postgres:

    wget http://yum.postgresql.org/9.3/fedora/fedora-19-x86_64/pgdg-fedora93-9.3-1.noarch.rpm
    sudo yum install pgdg-fedora93-9.3-1.noarch.rpm
    sudo yum install postgresql93-server postgresql93-contrib postgresql93-devel
    yum install --nogpgcheck libpqxx libpqxx-devel

    sudo postgresql-setup initdb
    sudo systemctl enable postgresql-9.3.service

    sudo systemctl start postgresql-9.3.service
    sudo systemctl status postgresql-9.3.service

Make sure the postgres server starts correctly before continuing. A typical
example is:

    postgresql-9.3.service - PostgreSQL 9.3 database server
       Loaded: loaded (/usr/lib/systemd/system/postgresql-9.3.service; enabled)
       Active: active (running) since Fri 2013-12-13 17:33:07 PST; 5s ago
      Process: 20384 ExecStart=/usr/pgsql-9.3/bin/pg_ctl start -D ${PGDATA} -s -o -p ${PGPORT} -w -t 300 (code=exited, status=0/SUCCESS)
     Main PID: 20387 (postgres)
       CGroup: name=systemd:/system/postgresql-9.3.service
                 20387 /usr/pgsql-9.3/bin/postgres -D /var/lib/pgsql/9.3/data -p 5432
                 20388 postgres: logger process   
                 20390 postgres: checkpointer process   
                 20391 postgres: writer process   
                 20392 postgres: wal writer process   
                 20393 postgres: autovacuum launcher process   
                 20394 postgres: stats collector process   

Now, we need to change the port and setup authentication. 
(On Fedora 19, the postgres data directory is in /var/lib/pgsql.)

    sudo vi /var/lib/pgsql/9.3/data/pg_hba.conf

    # add a line for 'local  all   all    trust'
    # put this before 'local all all peer'

Example: 

    ...
    # TYPE  DATABASE        USER            ADDRESS                 METHOD
    # "local" is for Unix domain socket connections only
    local   all             all                                     trust
    local   all             all                                     peer

    # IPv4 local connections:

    host    all             all             127.0.0.1/32            ident
    ...

Change the port to 5439. and remember to uncomment the default port line in
postgresql.conf.

    sudo vi /var/lib/pgsql/9.3/data/postgresql.conf
    sudo vi /etc/systemd/system/postgresql-9.3.service  #new file

This will be a new file...add the lines:

    .include /lib/systemd/system/postgresql-9.3.service
    [Service]
    Environment=PGPORT=5439

Save the file, restart postgres, and verify the port (-P) in the status output.

    sudo systemctl daemon-reload
    sudo systemctl restart postgresql-9.3.service
    sudo systemctl status postgresql-9.3.service

Finally, create a crowbar user for postgres

    pushd /.tmp
    sudo -u postgres createuser -s -d -U postgres -p 5439 crowbar
    popd


#### Ruby gems

We need to install some ruby gems:

    gem install builder bluecloth
    gem install json net-http-digest_auth kwalify bundler --no-ri --no-rdoc
    gem install delayed_job delayed_job_active_record rake rspec --no-ri --no-rdoc
    gem install pg -- --with-pg-config=/usr/pgsql-9.3/bin/pg_config
    gem install simplecov  

> Note: The Ubuntu directions call for the rcov gem, which does not 
> support Ruby 1.9, so we're using simpleconv.


## Building Crowbar 

At this point, the development environment is set up, and you can 
follow the directions in [devtool-build.md](../devtool-build.md) to 
checkout the Crowbar code and start developing.
 
