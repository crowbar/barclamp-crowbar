Feature: Support UI
  In order to support the Crowbar framework
  The system operator, Oscar
  wants to be able to do administration like export

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
    When I go to the "barclamps" page
    Then I should see {bdd:crowbar.i18n.barclamps.index.title}
    
  Scenario: Find Mark in Log
    While local or devtool
    Given I mark the logs with "REMAIN CALM"
    When I inspect the "../crowbar_framework/log/development.log" for "MARK >>>>>"
    Then I should grep "MARK >>>>>"
      And I should grep "REMAIN_CALM"
      And I should grep "<<<<< KRAM"

  Scenario: Bootstrap Page
    When I go to the "utils/bootstrap" page 
    Then I should see a heading {bdd:crowbar.i18n.support.bootstrap.title}
      And I should see a link to "ntp-server"
      And I should see a link to "dns-server"
      And I should see a link to "chef-server"
      And I should see a link to "network-server"
      And there are no localization errors
    