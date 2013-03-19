Feature: Support UI
  In order to support the Crowbar framework
  The system operator, Oscar
  wants to be able to do administration like export

  Scenario: Installed Barclamps
    Given I am on the home page
    When I click on the "Barclamp Versions" menu item
    Then I should see {bdd:crowbar.i18n.support.import.title_all}
      And I should see "crowbar"
      And I should see "chef"
      And I should see "provisioner"
      And there should be no translation errors

  Scenario: Localization AJAX CN
    When I18N checks "chuck_norris"
    Then I should see "Die!!!"

  Scenario: Localization AJAX Hit
    When I18N checks "test.verify"
    Then I should see "Affirmative"

  Scenario: Localization AJAX Miss
    When I18N checks "test.miss"
    Then I get a {integer:404} error

  Scenario: Use the Log Marker
    When I go to the "utils/marker/foo" page
    Then I should see "foo"
    
  Scenario: Localization from Regular Step
    When I go to the "barclamp/graph" page
    Then I should see {bdd:crowbar.i18n.barclamp.graph.title}
    
  Scenario: Find Mark in Log
    While local or devtool
    Given I mark the logs with "REMAIN CALM"
    When I inspect the "../crowbar_framework/log/development.log" for "MARK >>>>>"
    Then I should grep "MARK >>>>>"
      And I should grep "REMAIN_CALM"
      And I should grep "<<<<< KRAM"
    