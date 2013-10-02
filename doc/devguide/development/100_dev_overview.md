## Crowbar Development Environment

Working on Crowbar as a developer can involve a few different levels:

- modifying barclamps
- developing new barclamps
- creating custom builds of Crowbar 
- modifying core Crowbar code

Each of these levels will require a development environment and a copy of the 
Crowbar repositories to get started.

We assume you are setting up the Crowbar development environment in
a qemu-kvm virtual machine (VM). If you want to work in a different
environment, just adapt the steps and commands accordingly.

### Source code organization

Crowbar source code is managed using git, and hosted at GitHub in several
different repositories.  The main 'crowbar' repository contains the
Crowbar framework, development tools, build metadata, and test framework.
Each additional barclamp is in a separate repository.

Managing the branches and metadata across multiple repositories is handled
by the 'dev' tool in the main Crowbar repository.  After an initial
clone of the crowbar repository, dev is used to make sure everything is
in sync across multiple repositories, and to manage branches, releases,
and generating pull requests.

### Build Process Overview

> TODO what a build means, what it involves

### Community Development Workflow

> TODO need to document an overview of branches, releases, and general
pull request process and workflow.


