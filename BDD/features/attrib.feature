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