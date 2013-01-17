Feature: Attribs
  In order to collect data from systems
  The system operator, Oscar
  wants to be able to pick attributes to collect (or enable to write)

  Scenario: Attribute List
    Given there is a {object:attrib} "my_special_attribute"
    When REST gets the {object:attrib} list
    Then there should be a value "my_special_attribute"
    Finally REST removes the {object:attrib} "my_special_attribute"

  Scenario: REST JSON check
    Given there is a {object:attrib} "attribute_json_test"
    When REST gets the {object:attrib} "attribute_json_test"
    Then the {object:attrib} is properly formatted
    Finally REST removes the {object:attrib} "attribute_json_test"

  Scenario: REST Add 
    Given there is not a {object:attrib} "attribute_add_test"
    When REST creates the {object:attrib} "attribute_add_test"
    Then REST call returned success
    Finally REST removes the {object:attrib} "attribute_add_test"

  Scenario: REST Delete 
    Given there is a {object:attrib} "attribute_delete_test"
    When REST deletes the {object:attrib} "attribute_delete_test"
    Then REST call returned success
