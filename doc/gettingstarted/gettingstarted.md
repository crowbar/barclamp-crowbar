# Getting Started with Dell Crowbar #

This book is designed as a quick start quide for new Crowbar users. In addition to Crowbar installation and configuration instructions, this guide also includes instructions for installing and configuring minimal OpenStack and Hadoop clusters via Crowbar.

Some additional information, that may help with specific issues, has also been included.

## What is Crowbar? ##

Crowbar is an open-source cloud deployment framework, originally developed by Dell Inc. to support our OpenStack and Hadoop-powered solutions. Recently, its scope has increased to include a DevOps operations model and other deployments for additional cloud applications.

### Crowbar 1.0 ###

Crowbar 1.0 was a wrapper around Chef, written in Ruby on Rails 2.x. It stored all of its information in Chef’s database as data_bags, roles, and node objects.  
 
The Crowbar Admin ISO contained an OS with all of the required services and packages to deploy an OpenStack Nova and/or Swift cluster, and their dependencies. It allowed for deployment on just about any commodity hardware, but was certified in particular configurations of Dell hardware. 
 
*Barclamps* are the modular units of code and configuration to deploy an entire cluster, or portions of a cluster. One deploys those barclamps onto nodes via *Proposals* (now called *Barclamp Configurations*.) There are certain “default” barclamps (e.g., networking, dns, ntp, etc.) that are portions of a cluster (e.g., mysql, glance, swift-storage); and there are barclamps that represent a whole cluster (e.g., nova).
 
Nodes are managed by the Crowbar state machine. They are discovered via DHCP; hardware is discovered and configured via a special, temporarily-installed OS image that we call *Sledgehammer*; and are activated by deploying barclamp configurations upon them. They can also be decommissioned.

> See Rob Hirschfeld's Background Post: [http://robhirschfeld.com/2011/10/18/dell-crowbar-project-open-source-cloud-deployer/](http://robhirschfeld.com/2011/10/18/dell-crowbar-project-open-source-cloud-deployer/ "Rob Hirschfeld's Background Post") 
 
### Crowbar 2.0 ###

Crowbar 2.0 will remove the hard dependency of Chef by storing all of Crowbar’s configuration information in a simple database.