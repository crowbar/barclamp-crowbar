Feature: Barclamp Catalog
  In order to support the Crowbar API
  The integration programmer, Simon
  wants to be able to do list barclamp API information and routes

  Scenario: Fetch Logging Barclamp Catalog
    When REST requests the "/logging/v2/barclamps/catalog" page
    Then key "api_version" should be "v2"
    And  key "name" should be "logging"

  Scenario: Fetch DNS Barclamp Catalog
    When REST requests the "/dns/v2/barclamps/catalog" page
    Then key "api_version" should be "v2"
    And  key "name" should be "dns"

  Scenario: Fetch Ipmi Barclamp Catalog
    When REST requests the "/ipmi/v2/barclamps/catalog" page
    Then key "api_version" should be "v2"
    And  key "name" should be "ipmi"

  Scenario: %Fetch Chef Barclamp Catalog
    When REST requests the "/chef/v2/barclamps/catalog" page
    Then key "api_version" should be "v2"
    And  key "name" should be "chef"

  Scenario: %Fetch  Network Catalog
    When REST requests the "/network/v2/barclamps/catalog" page
    Then key "api_version" should be "v2"
    And  key "name" should be "network"

  Scenario: Fetch Deployer Barclamp Catalog
    When REST requests the "/deployer/v2/barclamps/catalog" page
    Then key "api_version" should be "v2"
    And  key "name" should be "deployer"



