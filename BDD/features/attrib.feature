Feature: Attrib(utes)
  In order to use track node information
  The system operator, Oscar
  wants inspect information discovered about the nodes
  and maybe add some of his own

  Scenario: UI Attrib List
    When I go to the "attribs" page
    Then I should see {bdd:crowbar.i18n.attribs.index.title}
      And I should see "random"
      And there should be no translation errors

  Scenario: UI Attrib List Click to Attrib
    Given I am on the "attribs" page
    When I click on the "random" link
    Then I should see "random"
      And there should be no translation errors

  Scenario: REST Attrib List
    When REST gets the {object:attrib} list
    Then the list should have an object with key "name" value "random"
    Then the list should have an object with key "map" value "random"

  Scenario: REST JSON check
    When REST gets the {object:attrib} "random"
    Then the {object:attrib} is properly formatted

    
  Scenario: REST Can Delete
    Given REST creates the {object:attrib} "bdd_foo"
    When REST deletes the {object:attrib} "bdd_foo"
    Then I get a {integer:200} result
      And there is not a {object:attrib} "bdd_foo"
  
  Scenario: REST Get 404
    When REST gets the {object:attrib} "thisdoesnotexist"
    Then I get a {integer:404} error

  Scenario: REST creates with map value
    Given REST creates the {object:attrib} "bdd_data_map" with map "bdd/value"
    When REST gets the {object:attrib} "bdd_data_map"
    Then key "map" should be "bdd/value"
    Finally REST removes the {object:attrib} "bdd_data_map"

  Scenario: Test attrib value is detectable on node using discovery
    Given REST creates the {object:node} "bdd-test-attrib.cr0wbar.com"
      And REST creates the {object:attrib} "bdd_data_here" with map "bdd/value"
      And REST sets the discovery on "bdd-test-attrib.cr0wbar.com" to "{ \"bdd\": { \"value\": 123 }}"
    When REST gets the {object:node} "bdd-test-attrib.cr0wbar.com"
    Then key "discovery:bdd:value" should be "123"
    Finally REST removes the {object:attrib} "bdd_data_here"
      And REST removes the {object:node} "bdd-test-attrib.cr0wbar.com"

  Scenario: Test attrib value is detectable on node using map
    Given REST creates the {object:node} "bdd-test-attribs.cr0wbar.com"
      And REST creates the {object:attrib} "bdd_data_there" with map "bdd/easyway"
    When REST sets the "bdd_data_there" on "bdd-test-attribs.cr0wbar.com" to "456"
    Then key "discovery:bdd:easyway" should be "456"
    Finally REST removes the {object:attrib} "bdd_data_there"
      And REST removes the {object:node} "bdd-test-attribs.cr0wbar.com"
