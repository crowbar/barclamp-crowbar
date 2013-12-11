## Deployments

The =Deployments= area of the menu is for operations about nodes in the system

### Deployments

Has a generated submenu for each Deployment for quick navigation

### Roles

### Annealer

### System Overview

Shows a functional "layer cake" of the deployment roles in the system.  It is a global deployment view that combines node-roles from all deployments.  This page is helpful to visualize the relative placement of all the roles in the system relative to their function for users instead of their internal dependency relationships.

Because this is a system view, node-roles from _all_ deployments are shown together.  There is no way to scope this view to just a single deployment.

>**Note**: This screen automatically refreshes.

The colors of the layers change depending on the state of the node-roles in the layer:

* red = error
* green = ready (stable)
* yellow = transition (todo, blocked, transition, etc)
* blue = proposed (waiting on user)

As the system processes the node-roles the layers will change to the correct colors.

>**Note**: You can click on the node-roles to see details.
