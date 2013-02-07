Feature: Scaffolds
  In order develop the system
  The devoper operator, Greg
  wants to be able to quickly check the models
  
  Scenario: Attribute
    When I go to the "scaffolds/attribs" page
    Then I should see "Attributes"
      And I should see "Name"
      And I should see "Description"
      And I should see "Order"
      And I should see "Nodes"
      And there should be no translation errors
      
  Scenario: Barclamp
    When I go to the "scaffolds/barclamps" page
    Then I should see "Barclamps"
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
    When I go to the "scaffolds/nodes" page
    Then I should see "Nodes"
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
    When I go to the "scaffolds/roles" page
    Then I should see "Roles"
      And I should see "Name"
      And I should see "Barclamp"
      And I should see "Created At"
      And I should see "Updated At"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors
    
  Scenario: Proposals
    When I go to the "scaffolds/proposals" page
    Then I should see "Proposals"
      And I should see "Name"
      And I should see "Description"
      And I should see "Last applied rev"
      And I should see "Active config"
      And I should see "Barclamp"
      And I should see "Current config"
      And I should see "Proposal config"
      And I should see "Created At"
      And I should see "Updated At"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors  
      
  Scenario: Proposals Configs
    When I go to the "scaffolds/proposal_configs" page
    Then I should see "ProposalConfigs"
      And I should see "Config"
      And I should see "Failed reason"
      And I should see "Revision"
      And I should see "Status"
      And I should see "Proposal"
      And I should see "Node"
      And I should see "Node role"
      And I should see "Created At"
      And I should see "Updated At"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors        
      
  Scenario: Menu Items
    When I go to the "scaffolds/navs" page
    Then I should see "Navs"
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
    When I go to the "scaffolds/docs" page
    Then I should see "Docs"
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
      
      
  Scenario: Configuration Attibs
    When I go to the "scaffolds/attribs" page
    Then I should see "Attrib"
      And I should see "Name"
      And I should see "Description"
      And I should see "Order"
      And I should see "Attrib instances"
      And I should see "Created At"
      And I should see "Updated At"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors        
      
  Scenario: Interfaces
    When I go to the "scaffolds/interfaces" page
    Then I should see "Interfaces"
      And I should see "Name"
      And I should see "Ip addresses"
      And I should see "Created At"
      And I should see "Updated At"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors  
      
  Scenario: Os
    When I go to the "scaffolds/os" page
    Then I should see "Os"
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
    When I go to the "scaffolds/os_packages" page
    Then I should see "OsPackages"
      And I should see "Name"
      And I should see "Description"
      And I should see "Order"
      And I should see "Os"
      And I should see "Created At"
      And I should see "Updated At"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors  
      
  Scenario: Groups
    When I go to the "scaffolds/groups" page
    Then I should see "Groups"
      And I should see "Name"
      And I should see "Description"
      And I should see "Order"
      And I should see "Category"
      And I should see "Created At"
      And I should see "Updated At"
      And I should see "Search"
      And I should see "Create New"
      And there should be no translation errors  
            
