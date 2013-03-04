### API Index

This is a temporary summary - API should be split down into subpages!

* /api/v2
  * /nodes (CRUD)
    * /:node/attribs
    * /:node/groups
    * /transition (put)
    * /allocate (put)
  * /barclamps (R)
    * /deployments
    * /template (redirects to snapshot)
  * /deployments
    * /commit (put)
    * /recall (put)
  * /snapshots (CRUD)
  * /jigs (CRUD)
  * /role_types (CRUD)
  * /roles (CRUD)
    * /attribs
    * /nodes
  * /attrib_types (CRUD)
  * /attribs
  * /groups
    * /:group/nodes
  
* /:barclamp/v2
  * /deployments (R)
  * /snapshots (R)
    
* /dashboard
  * /status/:node
* /barclamp
  * /status/:deployment
