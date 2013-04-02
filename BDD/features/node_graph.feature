Feature: Node Graph
  In order to debug
  The developer, David
  wants to be able to look at all of the node data

  Scenario: Nodes Graph
    When I go to the "dashboard/graph" page 
    Then there should be a heading "Node Graph"
      And there should be no translation errors
