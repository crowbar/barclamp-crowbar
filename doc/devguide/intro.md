# Welcome to Crowbar 

## What is Crowbar?

Crowbar is an open source DevOps-based full-stack cloud deployer.

Crowbar is an Open Source platform for server provisioning and
deployment from bare metal. It provides server discovery, firmware
upgrades, and operating system installation using PXE Boot. It deploys
applications on top of functioning operating systems using chef.

## Where did Crowbar come from?

Crowbar was developed by the [Dell CloudEdge Solutions Team]
(http://dell.com/openstack) as an [OpenStack](http://OpenStack.org) installer,
but has evolved as a much broader orchestration tool.

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
