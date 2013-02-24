Feature: Scaffolds
  In order develop the system
  The devoper operator, Greg
  wants to be able to quickly check the models
  
  Scenario: Attribs
    When I go to the "utils/scaffolds/attribs" page
    Then I should see heading "Attribs"
      And I should see "Type"
      And I should see "Value actual"
      And I should see "Value request"
      And I should see "Attrib"
      And I should see "Barclamp"
      And I should see "Jig run"
      And I should see "Node"
      And I should see "Role"
      And I should see "Role_Type"
      And I should see "BarclampCrowbar::AttribDefault"
      And there should be no translation errors

  Scenario: Attrib Type
    When I go to the "utils/scaffolds/attrib_types" page
    Then I should see heading "AttribTypes"
      And I should see "Name"
      And I should see "Created At"
      And I should see "Updated At"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors    
  
  Scenario: Barclamp
    When I go to the "utils/scaffolds/barclamps" page
    Then I should see heading "Barclamps"
      And I should see "Name"
      And I should see "Description"
      And I should see "Order"
      And I should see "transition list"
      And I should see "Display"
      And I should see "Layout"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors
      
  Scenario: Nodes
    When I go to the "utils/scaffolds/nodes" page
    Then I should see heading "Nodes"
      And I should see "Name"
      And I should see "Description"
      And I should see "Groups"
      And I should see "State"
      And I should see "Os"
      And I should see "Order"
      And I should see "Created At"
      And I should see "Updated At"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors

  Scenario: Roles
    When I go to the "utils/scaffolds/roles" page
    Then I should see heading "Roles"
      And I should see "Description"
      And I should see "Created At"
      And I should see "Updated At"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors    
            
  Scenario: Role Types
    When I go to the "utils/scaffolds/role_types" page
    Then I should see heading "RoleTypes"
      And I should see "Name"
      And I should see "Barclamp"
      And I should see "Created At"
      And I should see "Updated At"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors    
      
  Scenario: Navs
    When I go to the "utils/scaffolds/navs/" page
    Then I should see heading "Navs"
      And I should see "Name"
      And I should see "Description"
      And I should see "Development"
      And I should see "Parent item"
      And I should see "Path"
      And I should see "Order"
      And I should see "Created At"
      And I should see "Updated At"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors  
      
  Scenario: Docs
    When I go to the "utils/scaffolds/docs/" page
    Then I should see heading "Docs"
      And I should see "Author"
      And I should see "Copyright"
      And I should see "Date"
      And I should see "Description"
      And I should see "License"
      And I should see "Order"
      And I should see "Parent name"
      And I should see "Url"
      And I should see "Parent"
      And I should see "Children"
      And I should see "Created At"
      And I should see "Updated At"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors  
      
  Scenario: Deployments
    When I go to the "utils/scaffolds/deployments" page
    Then I should see heading "Deployments"
      And I should see "Name"
      And I should see "Description"
      And I should see "Order"
      And I should see "Barclamp"
      And I should see "Created At"
      And I should see "Updated At"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors 
                  
  Scenario: Os
    When I go to the "utils/scaffolds/os" page
    Then I should see heading "Os"
      And I should see "Name"
      And I should see "Description"
      And I should see "Order"
      And I should see "Nodes"
      And I should see "Packages"
      And I should see "Created At"
      And I should see "Updated At"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors  

  Scenario: OsPackages
    When I go to the "utils/scaffolds/os_packages" page
    Then I should see heading "OsPackages"
      And I should see "Name"
      And I should see "Description"
      And I should see "Order"
      And I should see "Os"
      And I should see "Created At"
      And I should see "Updated At"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors  

  Scenario: Jigs
    When I go to the "utils/scaffolds/jigs" page
    Then I should see heading "Jigs"
      And I should see "Name"
      And I should see "Description"
      And I should see "Order"
      And I should see "Created At"
      And I should see "Updated At"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors 
            
  Scenario: Groups
    When I go to the "utils/scaffolds/groups" page
    Then I should see heading "Groups"
      And I should see "Name"
      And I should see "Description"
      And I should see "Order"
      And I should see "Category"
      And I should see "Created At"
      And I should see "Updated At"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors  
            
