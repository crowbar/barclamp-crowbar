# Crowbar Developers Guide

Crowbar is an Apache 2 licensed open source platform for server
provisioning and deployment from bare metal. It provides server discovery,
firmware upgrades, and operating system installation using PXE Boot. It
deploys applications on top of functioning operating systems using chef.

Crowbar was developed by the [Dell CloudEdge Solutions Team]
(http://dell.com/openstack) as an [OpenStack](http://OpenStack.org)
installer, but has evolved as a much broader orchestration tool.

This is the developers guide for Crowbar.  This guide provides an
introduction to the concepts and architecture of Crowbar, and describes
of the development environment used to customize crowbar and extend
the framework.

Note: This guide is currently under development, and changing rapidly.

## Table of Contents

The table of contents for the documentation will follow the outline below.

TODO dynamically generate this from the actual content.

* crowbar concepts / architecture 
    * what is crowbar 
    * barclamps
    * jigs
	* Jig: “writing and integrating upstream cookbooks & modules”
    * Crowbar's state machine , the Annealer
	* Deployments, Nodes, Roles & the Annealer
    * crowbar builds / releases
    * ui 
    * web framework / rails 
    * network abstraction
    * heterogenous operating systems
    * state machine
    * pull from source 
    * database backend
    * sledgehammer
    * UEFI 

* crowbar development environment
    * code organization
    * build process overview
    * community development workflow 
        * branches, pull requests, etc
    * setting up a build VM
        * dev-vm-Fedora
        * dev-vm-SUSE
        * dev-vm-Ubuntu
    * Judd's vagrant build setup

    * devtool-build.md 
    * dev-vm
    * devtool
    * coding conventions 
    * how to write a new barclamp
    * packaging and installation
    * “how to build packages” – I thought packages was our primary output for CB2.0, not ISOs – allowing continuous deployment of crowbar itself.
        * how to build an ISO from existing code

* using crowbar for continuous deployment 
* testing and continuous integration

* documentation system
* api reference
    * api calls 

