Feature: Attribs
  In order to collect data from systems
  The system operator, Oscar
  wants to be able to pick attributes to collect (or enable to write)

  Scenario: Attribute List
    Given there is an attribute "my_special_attribute"
    When REST gets the attribute list
    Then there should be a value "my_special_attribute"
    Finally REST removes the attribute "my_special_attribute"

  Scenario: REST JSON check
    Given there is an attribute "attribute_json_test"
    When REST gets the attribute "attribute_json_test"
    Then the {object:attrib} is properly formatted
    Finally REST removes the attribute "attribute_json_test"

  Scenario: REST Add 
    Given there is not an attribute "attribute_add_test"
    When REST adds the attribute "attribute_add_test"
    Then there is an attribute "attribute_add_test"
    Finally REST removes the attribute "attribute_add_test"

  Scenario: REST Delete 
    Given there is an attribute "attribute_delete_test"
    When REST deletes the attribute "attribute_delete_test"
    Then there is not an attribute "attribute_delete_test"
