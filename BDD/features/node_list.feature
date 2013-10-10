Feature: Node List
  In order bulk edit nodes
  The system operator, Oscar
  wants to be able to edit all the nodes on a single page

  Scenario: UI Node List
    When I go to the "dashboard/list" page
    Then I should see {bdd:crowbar.i18n.dashboard.list.title}
      And I should see {lookup:crowbar.node_name}
      And I should see {bdd:crowbar.i18n.all}
      And there should be no translation errors

  Scenario: UI Node List for Deployment
    When I go to the "dashboard/list/system" page
    Then I should see {bdd:crowbar.i18n.dashboard.list.title}
      And I should see {lookup:crowbar.node_name}
      And I should see "system"
      And there should be no translation errors

  Scenario: UI Node List Click to Node
    Given I am on the "dashboard/list" page
    When I click on the {lookup:crowbar.node_name} link
    Then I should see {lookup:crowbar.node_name}
      And there should be no translation errors

