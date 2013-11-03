## Docker Workload Containers

We include Docker packages on the admin node so that you can use Docker to for test and demos.

### Prep your dev environment

These steps create a registery on your local host environment that is used to pull containers into your latest Crowbar Admin node.

1. Install Docker (=apt-get install lxc-docker=)
1. Get a local registery (=docker pull samalba/docker-registry=)
  1. this will take some time to download files, be patient


### Setup Containers on the Admin Server

1. Install Docker on Admin Node (=apt-get install lxc-docker=)