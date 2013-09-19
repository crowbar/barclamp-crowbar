# Crowbar Concepts 

## The operations challenge

A deployment framework is key to solving the problems of deploying,
configuring, and scaling open source clusters for cloud computing.

Deploying an open source cloud can be a complex undertaking. If you’re
using manual processes, you could spend days or even weeks working to
get your cloud up and running. And even then, your cloud never sits
still—it’s always on an upgrade or improvement path. You need to
continually deploy new servers, add management capabilities, and track
the upstream releases, while keeping the cloud running, and providing
reliable services to end users.

These were among the challenges that drove the development of the Crowbar
software framework from it's roots as an [OpenStack](http://OpenStack.org)
installer into a much broader orchestration tool.  Because of this evolution,
Crowbar has a number of architectural features to address these
challenges.


features:

Crowbar is implemented as a web application, with a full user interface
and a REST API.



bare metal provisioning

heterogenous operating systems

barclamps for extensibility

jigs

state machine

network abstraction


scalability 




Crowbar is an Apache 2 licensed open source framework
developed by Dell and enriched by a growing developer ecosystem. With
Crowbar, you can install cloud software across clusters and scale out
systems quickly and automatically.

Crowbar also provides a growing range of capabilities for cloud operation
and optimization, such as network monitoring and discovery and the
gathering performance data. The software provides a configurable,
adjustable framework, complete with many built-in features that can save
time, streamline effort, and potentially lower costs.

Crowbar leverages capabilities in Chef, an open source systems integration
framework from Opscode, built for cloud automation. Crowbar is designed
to deploy multi-node OpenStack clouds on bare metal servers in as little
as two hours.

To make things even easier, Dell develops modular “barclamps” that
are used to attach new capabilities to Crowbar. For example, barclamps
are available for DNS, discovery, Nova, Swift, Nagios, Gangalia, and
BIOS configuration, among other integrations.


Viewed from a longer-term perspective, the Crowbar 2 enhancements will
move the software framework closer to the ultimate technical goal of
closed-loop operations. The vision here is to enable self-correcting,
self-healing cloud environments that continuously optimize themselves
to improve performance and reduce operational costs.


## From Victor, 9/9/2013

Design difference between CB 1.x and CB 2.0: •     Shifting from
coarse-grained linear semi-implicit dependencies between barclamps to
having a proper dependency graphs for roles and node-role bindings.

In Crowbar 1.x, the core Crowbar framework has several
partially-overlapping ad-hoc mechanisms for determining when it should do
what, and they are all driven by having barclamp authors assigning integer
priorities to barclamps, chef roles, configuration data, and hacks and
monkey patching to work around where they do not play nicely together.
Since the rules were informal and never documented, the only way to
determine when things would happen is via experimenting with priority
numbers until things worked the way you expected them to, or adding yet
another hack if you could not get things to work properly. In Crowbar
2.0, barclamp writers only need to explicitly declare the dependencies
on other roles their roles have, and make sure their declarations do not
make the role dependency graph circular, which is easier to reason about.
The Crowbar framework then uses the dependency graph between roles to
build the graph between node-role bindings as the cluster is built out
to ensure that things always happen in the right order.

•	Shifting the primary emphasis in interacting with Crowbar from
interactions at a barclamp level to interactions at the role and node-role
binding level.

In Crowbar 1.x, virtually all of the interaction between the user and
Crowbar happens in terms of barclamp level proposals. In Crowbar 2.0,
that responsibility is divided into 3 different spheres of responsibility:

o	Roles, which represent a specific bundle of capabilities that can be attached to a node. For instance, dns-server is the role in Crowbar 2.0 that allows a server to act as a DNS server.

o	Deployments, which act as a collection of nodes along with a default set of role configuration information for any roles that may be bound to a node in the deployment.  Deployments will be hierarchical, and there is a system deployment that the Crowbar framework manages at the root of that hierarchy.  All newly-discovered nodes will be added to the system deployment to get their initial node-role bindings.

o	Node-role bindings (called noderoles for short -- please suggest a better name for this!), which represent a specific instance of a role bound to a node.  Noderoles have their own configuration in addition to the role configuration at a deployment level and the default role configuration.
    adam - assignments as alternatives ?

•	Not relying on Chef as a foundational component.

Crowbar 1.x relies on Chef 10.x for everything, including storing all of the information needed to run the Crowbar framework.  This meant that any failure on the part of Chef had catastrophic consequences on Crowbar.  Crowbar 2.0 is a standalone application that does not rely on Chef to store its configuration information.

•	Not relying on Chef as the only way to effect change on a node.

In Crowbar 2.0, we have the concept of a jig as a component that can be used to effect change on another node.  Support for using Chef in Crowbar 2.0 is provided by the Chef jig, and Crowbar 2.0 also has a Script jig that is used for bootstrapping.  Every role declares what jig must be used to do what it needs on a node.
Next Steps for Crowbar 2.0 (in rough dependency order):

•	Make framework side of the Network barclamp operate properly, instead of working in rigged demo mode.

Right now, the Crowbar framework side of things is just smart enough to allocate an IP to the admin node to exercise the network recipe and bootstrap the rest of the admin node roles.  It needs to be able to track IP address allocations in a generic fashion, and it needs to be able to support other networks besides the admin network.

•	Flesh out node discovery.

We can boot nodes into Sledgehammer, but we need to wire in more functionality to allow node discovery and configuration to take place.

•	Come up with a reasonable OS installation story, and implement it.

•	De-cruft the Crowbar-specific Chef cookbooks.

While the current Chef cookbooks work with minimal changes, a side-effect
of how the Crowbar framework manages configuration data via the node-role
graph is that it is very easy for most of our core cookbooks to operate
in an entirely attribute-driven fashion.  Refactoring them and stripping
out the CB 1.x specific cruft would be a good way to learn the framework.

## Robs comments

Another item worth mentioning for people looking to play w/ CB2.0 – we have a SIMULATOR that can populate a test system for easy play!

To use the simulator:
In your dev system, run the test server: ./dev tests server
In a new window, start erlang 
                cd ~/crowbar/barclamps/crowbar/BDD
                cp example.config default.config
                [review default.config and update if needed]
                ./linux_compile.sh
                erl
                dev:pop().
Open the Crowbar UI under http://[dev system IP]:3000
You can then explore and even run the Annealer!

I’ll upload a demo.

Note, most of this is in the INTEGRATED DOCS system of the barclamps.  Please update there if you find errors.

### Deployments

So Deployments are containers in which nodes reside, and a Deployment
can contain other Deployments?  Are node roles considered to reside in
the Deployment that contains their associated nodes and roles?

Yes and no.  All the noderoles together make up the noderole graph,
which by itself does not care about deployments.  However, when a
noderole is created (and gets its initial configuration) and bound to
the noderole graph, it is always created in the context of a deployment,
which determines what configuration it will get and determines what
noderoles it will be bound to to satisfy the dependencies of the role
it is binding to the node.

## Noderoles 

What is the configuration that a node role owns?  Would this
configuration be user visible, or something that would be set by the
barclamp associated with the role?

The node role owns any configuration data that is specific to that
binding of a role to a node.  As an example, every node in Crowbar will
have a noderole binding the network-admin role to it, and that noderole
binding will be used to carry the addresses and conduit information
from the Crowbar framework to the network recipe when it executes on the
node. For now, all of that data is editable by both the user and by the
Crowbar framework.

The other thing is that barclamps in CB2.0 are mostly just a way to
package related roles their recipes/scripts/whatever, and whatever glue
is needed to tie them in to the core Crowbar framework.  They do not
have configuration data of their own.

## barclamps, roles and jigs 
Say there are 3 different orchestration engines that have jigs.  As a
barclamp developer, I would imagine I would want to write a barclamp, and
provide the equivalent of Chef recipes for each of the jigs I wanted to
support, the thought being that Crowbar would use whichever set of recipes
it needed to based on what jig was present.  Will this be possible?

Each role declares what jig must be used to implement it.  As a barclamp
developer, you choose what jig will be used to implement the roles that
will be part of that barclamp.  There is no mechanism for having more
than one jig being able to implement a role -- adding that restriction
gets rid of a whole slew of problems that it we really should not want
to deal with for a .0 release.

##Adams comments

Having this would be a really nice bonus, although such a demo
will quickly become out of date, so IMHO I think it would be of more
lasting value if any spare time was first spent on documenting the new
architecture.  For example, the great info Victor sent out the other day
could be turned into real documentation, and a couple of simple diagrams
explaining how the annealer handles node-role state transitions would be
very useful in the long-term.  Actually this could even be done by using
a series of HTML tables interspersed with text, to show how the states
in the node-role matrix change over time during an example deployment.

## Where did Crowbar come from?


## KEy features

Our technical objective for Crowbar 2.0 is to simplify and streamline development efforts as the development and user community grows. We are seeking to:

1. simplify our use of Chef and eliminate Crowbar requirements in our Opscode Chef recipes.
   1. reduce the initial effort required to leverage Crowbar
   1. opens Crowbar to a broader audience (see Upstreaming)
1. provide heterogeneous / multiple operating system deployments. This enables:
   1. multiple versions of the same     OS running for upgrades
   1. different operating systems operating simultaneously (and deal with heterogeneous packaging issues)
   1. accommodation of no-agent systems like locked systems (e.g.: virtualization hosts) and switches (aka external entities)
   1. UEFI booting in Sledgehammer
1. strengthen networking abstractions
   1. allow networking configurations to be created dynamically (so that users are not locked into choices made before Crowbar deployment)
   1. better manage connected operations
   1. enable pull-from-source deployments that are ahead of (or forked from) available packages.
1. improvements in Crowbar's core database and state machine to enable
   1. larger scale concerns
   1. controlled production migrations and upgrades
1. other important items
   1. make documentation more coupled to current features and easier to maintain
   1. upgrade to Rails 3 to simplify code base, security and performance
   1. deepen automated test coverage and capabilities

Beyond these great technical targets, we want Crowbar 2.0 is to address barriers to adoption that have been raised by our community, customers and partners. We have been tracking concerns about the learning curve for adding barclamps, complexity of networking configuration and packaging into a single ISO.

## Attribute Injection / Judd

upstreaming /  shareable cookbooks 

orchestration 

http://newgoliath.wordpress.com/2013/03/08/orchestration-consistency-and-community-cookbooks/

eventual consistency 

## Rob's Crowbar recap 

Background: Crowbar is an open source cloud deployment framework
originally developed by Dell to support our OpenStack and Hadoop powered
solutions.  Recently, it’s scope has increased to include a DevOps
operations model and other deployments for additional cloud applications.

It’s only been a matter of months since we open sourced the Dell Crowbar
Project at OSCON in June 2011; however, the progress and response to the
project has been over whelming.  Crowbar is transforming into a community
tool that is hardware, operating system, and application agnostic.
With that in mind, it’s time for me to provide a recap of Crowbar for
those just learning about the project.

Crowbar started out simply as an installer for the “Dell
OpenStack™-Powered Cloud Solution” with the objective of deploying a
cloud from unboxed servers to a completely functioning system in under
four hours.  That meant doing all the BIOS, RAID, Operations services
(DNS, NTP, DHCP, etc.), networking, O/S installs and system configuration
required creating a complete cloud infrastructure.  It was a big job,
but one that we’d been piecing together on earlier cloud installation
projects.  A key part of the project involved collaborating with Opscode
Chef Server on the many system configuration tasks.  Ultimately, we met
and exceeded the target with a complete OpenStack install in less than
two hours.

In the process of delivering Crowbar as an installer, we realized that
Chef, and tools like it, were part of a larger cloud movement known
as DevOps.

The DevOps approach to deployment builds up systems in a layered model
rather than using packaged images.  This layered model means that
parts of the system are relatively independent and highly flexible.
Users can choose which components of the system they want to deploy and
where to place those components.  For example, Crowbar deploys Nagios
by default, but users can disable that component in favor of their own
monitoring system.  It also allows for new components to identify that
Nagios is available and automatically register themselves as clients and
setup application specific profiles.  In this way, Crowbar’s use of a
DevOps layered deployment model provides flexibility for BOTH modularized
and integrated cloud deployments.

We believe that operations that embrace layered deployments are
essential for success because they allow our customers to respond to
the accelerating pace of change.  We call this model for cloud data
centers “CloudOps.”

Based on the flexibility of Crowbar, our team decided to use it as the
deployment model for our Apache™ Hadoop™ project (“Dell | Apache
Hadoop Solution”).  While a good fit, adding Hadoop required expanding
Crowbar in several critical ways.

We had to make major changes in our installation and build processes
to accommodate multi-operating system support (RHEL 5.6 and Ubuntu
10.10 as of Oct 2011).  We introduced a modularization concept that we
call “barclamps” that package individual layers of the deployment
infrastructure.  These barclamps reach from the lowest system levels
(IPMI, BIOS, and RAID) to the highest (OpenStack and Hadoop).  Barclamps
are a very significant architecture pattern for Crowbar:

They allow other applications to plug into the framework and leverage
other barclamps in the solution.  For example, VMware created a
Cloud Foundry barclamp and Dream Host has created a Ceph barclamp.
Both barclamps are examples of applications that can leverage Crowbar
for a repeatable and predictable cloud deployment.  They are independent
modules with their own life cycle.  Each one has its own code repository
and can be imported into a live system after initial deployment.
This allows customers to expand and manage their system after initial
deployment.  They have many components such as Chef Cookbooks, custom UI
for configuration, dependency graphs, and even localization support.  They
offer services that other barclamps can consume.  The Network barclamp
delivers many essential services for bootstrapping clouds including IP
allocation, NIC teaming, and node VLAN configuration.  They can provide
extensible logic to evaluate a system and make deployment recommendations.
So far, no barclamps have implemented more than the most basic proposals;
however, they have the potential for much richer analysis.  Making these
changes was a substantial investment by Dell, but it greatly expands the
community’s ability to participate in Crowbar development.  We believe
these changes were essential to our team’s core values of open and
collaborative development.

Most recently, our team moved Crowbar development into the open.
This change was reflected in our work on OpenStack Diablo (+ Keystone and
Dashboard) with contributions by Opscode and Rackspace Cloud Builders.
Rather than work internally and push updates at milestones, we are now
coding directly from the Crowbar repositories on Github.  It is important
to note that for licensing reasons, Dell has not open sourced the optional
BIOS and RAID barclamps.  This level of openness better positions us to
collaborate with the crowbar community.

For a young project, we’re very proud of the progress that we’ve made
with Crowbar.  We are starting a new chapter that brings new challenges
such as expanding community involvement, roadmap transparency, and growing
Dell support capabilities.  You will also begin to see optional barclamps
that interact with proprietary and licensed hardware and software.
All of these changes are part of growing Crowbar in framework that can
support a vibrant and rich ecosystem.

We are doing everything we can to make it easy to become part of the
Crowbar community.  Please join our mailing list, download the open source
code or ISO, create a barclamp, and make your voice heard.  Since Dell
is funding the core development on this project, contacting your Dell
salesperson and telling them how much you appreciate our efforts goes
a long way too.



## CB20 changes What are the big changes?

introduction of a database
introduction of the jig
model redesign, including state machine
switching to upstream non-Crowbar-specific community cookbooks
Chef 11
Ruby 1.9
Rails 3.2
Linux Packaging
integrated documentation model (based on markdown)
consistent API w/ validation
What is the Annealer?

The Annealer is somewhat like a scheduler. It drives the node-roles
through their life-cycles on the appropriate Jigs. There is an implied
state machine created by the dependencies of node-roles between each
other. It drives the implied state machine, reporting on its state at
every phase of "annealing."

What is the jig, and why do we need it?

Jigs are abstraction layers between the Crowbar Annealer and any node
control product, like Chef, Puppet, scripts, Ansible, etc. They report
node-role state back to the Annealer.

The Script Jig installs Crowbar and all other Jigs. It SSHes into the
node, copies the appropriate scripts into place, and supplies attribute
data through JSON.

Why do we need a database? (instead of Chef or NoSQL)

We've chosen PostgreSQL as our database. Initially it was a significant
performance gain, but we chose not to depend upon Chef-Server to manage
our Crowbar specific data. The change offers system architects more
choices in how (or if) they run the Chef-Server. It also gives barclamp
creators greater flexibility in storing attribute data and delivering
it across nodes, roles and even barclamps.

How has the installation process changed?

The Script Jig now does the majority of the installation procedures.

# Crowbar core 

## Expected Audiences & Goals
50% OpenStack -> Goal: get to Folsom faster
30% Crowbar as general -> Goal: use Crowbar to handle more generic Ops
20% Hadoop -> Goal: improve performance and scale of Hadoop

## Refactor / Design Objectives

Get back to working stable as soon as possible – we are laying a
foundation, not completing features.  Incremental architecture approach
– we are making changes to unlock the architecture Automation & testing
– we want more!  Open development – we will continue to do check-ins,
commits and pull-requests as our normal process.

## Legals

If you want to be a contributor, please complete the company or individual
agreements.  See https://github.com/dellcloudedge/crowbar/wiki/License

## Technical Background

Crowbar 2 refactor is needed because there are interlocking changes
to database, networking, configuration and operating system management
changes that had to be done as a group.

## Core, not up for discussion

### Moving to a database (Chef will not be the primary database)

Allows improve scale and eliminates race conditions from Chef data


### Changing to Rails 3 

Moderize the code base - includes user management
- Which version of Ruby is targeted? Will 1.8.7 be supported still? (It would be important for us to still see 1.8.7 supported)
- We are using RVM

### Dynamic documentation

Allows documentation (in markdown) to be kept w/ the barclamps and merged into a master document dynamically.

### Barclamp SDK

Makes it easier to create new barclamps.  Works with http://crowbar.sync.in/crowbar2-attributeconfig feature

### Big Disk Support (UEFI)

Enable Sledgehammer booting and configuration of disks > 2Tb
