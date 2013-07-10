Feature: Scaffolds
  In order develop the system
  The devoper operator, Greg
  wants to be able to quickly check the models
  
  Scenario: Attribs
    While interactive
    When I go to the "utils/scaffolds/attribs?limit=1" page
    Then I should see heading "Attribs"
      And I should see "Name"
      And I should see "Map"
      And I should see "Type"
      And there should be no translation errors

  Scenario: Barclamp
    While interactive
    When I go to the "utils/scaffolds/barclamps?limit=1" page
    Then I should see heading "Barclamps"
      And I should see "Name"
      And I should see "Description"
      And I should see "Order"
      And I should see "Display"
      And I should see "Layout"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors
      
  Scenario: Nodes
    While interactive
    When I go to the "utils/scaffolds/nodes?limit=1" page
    Then I should see heading "Nodes"
      And I should see "Name"
      And I should see "Description"
      And I should see "Groups"
      And I should see "Order"
      And I should see "Created At"
      And I should see "Updated At"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors

  Scenario: Roles
    While interactive
    When I go to the "utils/scaffolds/roles?limit=1" page
    Then I should see heading "Roles"
      And I should see "Description"
      And I should see "Created At"
      And I should see "Updated At"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors    

  Scenario: Cycles
    While interactive
    When I go to the "utils/scaffolds/cycles?limit=1" page
    Then I should see heading "Cycles"
      And I should see "Description"
      And I should see "Node roles"
      And I should see "Created At"
      And I should see "Updated At"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors    
            
  Scenario: Navs
    While interactive
    When I go to the "utils/scaffolds/navs?limit=1" page
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
    While interactive
    When I go to the "utils/scaffolds/docs?limit=1" page
    Then I should see heading "Docs"
      And I should see "Description"
      And I should see "Order"
      And I should see "Parent name"
      And I should see "Parent"
      And I should see "Children"
      And I should see "Created At"
      And I should see "Updated At"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors  
      
  Scenario: Deployments
    While interactive
    When I go to the "utils/scaffolds/deployments?limit=1" page
    Then I should see heading "Deployments"
      And I should see "Name"
      And I should see "Description"
      And I should see "Order"
      And I should see "Created At"
      And I should see "Updated At"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors 
                  
  Scenario: Jigs
    While interactive
    When I go to the "utils/scaffolds/jigs?limit=1" page
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
    While interactive
    When I go to the "utils/scaffolds/groups?limit=1" page
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
            
