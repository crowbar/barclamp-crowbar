Feature: Node Graph
  In order to debug
  The developer, David
  wants to be able to look at all of the node data

  Scenario: Nodes Graph
    When I go to the "dashboard/graph" page 
    Then I should see a heading {bdd:crowbar.i18n.dashboard.graph.title}
      And there should be no translation errors
