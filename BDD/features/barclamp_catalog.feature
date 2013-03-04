Feature: Barclamp Catalog
  In order to support the Crowbar API
  The integration programmer, Simon
  wants to be able to do list barclamp API information and routes

  Scenario: Fetch Barclamp Catalogs
    When REST requests the "/logging/v2/barclamps/catalog" page
    Then key "api_version" should be "v2"

