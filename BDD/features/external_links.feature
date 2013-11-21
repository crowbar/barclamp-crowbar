Feature: External Links
  The Crowbar community, 
  wants to rely on documentation navigation

  Scenario: Home Page
    Skip WIP
    When I go to the "crowbar.github.io" site
    Then I should see a "Welcome to Crowbar"
      And I should get an http ok response

  Scenario: Read.me
    Skip WIP
    When I go to the "https://github.com/crowbar/crowbar/blob/master/README.md" site
    Then I should see a heading "Crowabar Documentation README"
      And I should see "not the documentation you are looking for"
      And I should get an http ok response

  Scenario: Read.me Links Getting Started
    Skip WIP
    Given I am on the "https://github.com/crowbar/crowbar/blob/master/README.md" site
    When I click the "Getting Started Guide"
    Them I should get an http ok response

  Scenario: Read.me Links User Resources
    Skip WIP
    Given I am on the "https://github.com/crowbar/crowbar/blob/master/README.md" site
    When I click the "User Resources"
    Them I should get an http ok response

  Scenario: Read.me Links Developer Resources
    Skip WIP
    Given I am on the "https://github.com/crowbar/crowbar/blob/master/README.md" site
    When I click the "Developer Resources"
    Them I should get an http ok response

  Scenario: Read.me Links Release Resources
    Skip WIP
    Given I am on the "https://github.com/crowbar/crowbar/blob/master/README.md" site
    When I click the "Crowbar Release Management"
    Them I should get an http ok response
