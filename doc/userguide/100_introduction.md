#Introduction
This document provides instructions you for operating Crowbar.  Please refer to additional user guide for specific products that are deployed by Crowbar such as OpenStack™, Apache™, or Hadoop™.   

##Concepts##
The purpose of this guide is to explain the user interface of Crowbar.  Use the Crowbar User Guide for assistance with installing Crowbar and configuring the target system.

>![notes.png](graphics/notes.png "notes.png") Concepts beyond the scope of this guide will be introduced as needed in notes and references to other documentation.

 
##Opscode Chef Server##
Crowbar makes extensive use of Opscode Chef Server (<http://opscode.com>). To explain Crowbar actions, you should understand the underlying Chef implementation.  

>![chef.png](graphics/chef.png "chef.png") To use Crowbar, it is not necessary to log into the Chef Server; consequently, use of the Chef UI is not covered in this guide.  Supplemental information about Chef is included.
This guide provides this additional Chef information as notes flagged with the Opscode logo.  

##Dell Specific Options##
The Dell EULA version of Crowbar provides additional functionality and color pallets than the open source version.  When divergences are relevant, they are identified.

>![Dell.jpg](graphics/Dell.jpg "Dell.jpg") To perform some configuration options and provide some integration, we use libraries that cannot be distributed using open source. 

>![notes.png](graphics/notes.png "notes.png") Crowbar is not limited to managing Dell servers and components.  Due to driver requirements, some barclamps, for example: BIOS & RAID, must be targeted to specific hardware; however, those barclamps are not required for system configuration.

##New For Version 1.5##
The following features have been added to Crowbar version 1.5:

- Dashboard
	- Node Alias
	- Grouping
- Bulk Edit
	- Node Alias
	- Description Population
- Networking Menu
	- Switch view 
	- VLAN view
- Utility Menu
	- Import Barclamps
	- Export Logs and Chef Objects