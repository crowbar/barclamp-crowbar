#Introduction
This document provides instructions you for operating Crowbar.  Please refer to additional user guide for specific products that are deployed by Crowbar such as OpenStack™, Apache™, or Hadoop™.   

##Concepts##
The purpose of this guide is to explain the user interface of Crowbar.  Use the *Dell Crowbar Software Framework User's Guide* for assistance with installing Crowbar and configuring the target system.

>**Note**: Concepts beyond the scope of this guide will be introduced as needed in notes and references to other documentation.

 
##Opscode Chef Server##
Crowbar makes extensive use of Opscode Chef Server (<http://opscode.com>). To explain Crowbar actions, you should understand the underlying Chef implementation.  

>**Note**: To use Crowbar, it is not necessary to log into the Chef Server; consequently, use of the Chef UI is not covered in this guide.  Supplemental information about Chef is included.
This guide provides this additional Chef information as notes flagged with the Opscode logo.  

##Dell-Specific Options##
The Dell EULA version of Crowbar provides additional functionality and color pallets than the open source version.  When divergences are relevant, they are identified.

>**Note**: To perform some configuration options and provide some integration, we use libraries that cannot be distributed using open source. 

>Crowbar is not limited to managing Dell servers and components.  Due to driver requirements, some barclamps (i.e., BIOS and RAID) must be targeted to specific hardware. However, those barclamps are not required for system configuration.

##New For Version 1.6##
The following features have been added to Crowbar version 1.6:

* Dell PowerEdge-C6220 and C8000 support for Compute nodes
* Dell Force10 S4810 support for ToR and aggregation switches
* 10 GbE support on the following platforms:
	* C6220
	* C8000
	* R720
* 3 TB large disk support on the following platforms:
	* C8000
	* R720
* Scalability up to 6 to 120 nodes per cluster
