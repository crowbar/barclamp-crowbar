### API Index

This is a temporary summary - API should be split down into subpages!

* /api
  * /v2
    * /nodes (CRUD)
      * /:node/attribs
      * /:node/groups
      * /transition (put)
      * /allocate (put)
    * /barclamps (CRUD)
    * /jigs (CRUD)
    * /attrib_types (CRUD)
    * /role_types (CRUD)
    * /groups
      * /:group/nodes
  * /status
    * /nodes(:/id)
    * /deployments(:/id)
    
* /:barclamp/v2
  * /deployments (CRUD)
    * /commit (put)
    * /recall (put)
  * /template (redirects to snapshot)
  * /snapshots (CRUD)
  * /roles (CRUD)
    * /attribs
    * /nodes
  * /attribs
    