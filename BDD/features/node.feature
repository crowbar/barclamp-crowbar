Feature: Nodes
  In order check out the nodes
  The system operator, Oscar
  wants to be able to check the status of nodes

  Scenario: Admin Node working & defaults
    When REST gets the {object:node} {lookup:crowbar.node_name}
    Then the {object:node} is properly formatted
      And key "deployment_id" should be "1"
      And key "admin" should be "true"
      And key "alive" should be "true"
      And key "available" should be "true"

  Scenario: UI Node List
    When I go to the "nodes" page
    Then I should see {bdd:crowbar.i18n.nodes.index.title}
      And I should see {lookup:crowbar.node_name}
      And there should be no translation errors

  Scenario: UI Node List Click to Node
    Given I am on the "nodes" page
    When I click on the {lookup:crowbar.node_name} link
    Then I should see {lookup:crowbar.node_name}
      And there should be no translation errors

  Scenario: Nodes List
    When REST gets the {object:node} list
    Then the list should have an object with key "name" value {lookup:node.name}

  Scenario: REST JSON check
    When REST gets the {object:node} {lookup:node.name}
    Then the {object:node} is properly formatted
    
  Scenario: REST Can Delete
    Given REST creates the {object:node} "going.going.gone"
    When REST deletes the {object:node} "going.going.gone"
    Then I get a {integer:200} result
      And there is not a {object:node} "going.going.gone"
  
  Scenario: REST Get 404
    When REST gets the {object:node} "thisdoesnotexist"
    Then I get a {integer:404} error
    
  Scenario: Node List
    Given there is a {object:node} "bdd-node-list.example.com"
    When REST gets the {object:node} list
    Then the list should have an object with key "name" value "bdd-node-list.example.com"
      And the list should have an object with key "name" value "bdd1.example.com"
      And the list should have an object with key "name" value {lookup:crowbar.node_name}
    Finally REST removes the {object:node} "bdd-node-list.example.com"

  Scenario: Node Alive Settable Default False
    Given there is a {object:node} "bdd-alive-false.example.com"
    When REST gets the {object:node} "bdd-alive-false.example.com"
    Then key "alive" should be "false"
      And the {object:node} is properly formatted
    Finally REST removes the {object:node} "bdd-alive-false.example.com"

  Scenario: Node Alive Settable
    Given there is a {object:node} "bdd-alive-set.example.com"
    When REST sets the {object:node} "bdd-alive-set.example.com" {atom:alive} state to be "true"
    Then key "alive" should be "true"
      And the {object:node} is properly formatted
    Finally REST removes the {object:node} "bdd-alive-set.example.com"

  Scenario: Node Available Settable Default True
    Given there is a {object:node} "bdd-available-false.example.com"
    When REST gets the {object:node} "bdd-available-false.example.com"
    Then key "available" should be "true"
      And the {object:node} is properly formatted
    Finally REST removes the {object:node} "bdd-available-false.example.com"

  Scenario: Node into New Deployment
    Given there is a {object:node} "node-deploy.example.com"
      And there is a {object:deployment} "new_deploy"
    When REST sets {object:node} "node-deploy.example.com" property "deployment" to "new_deploy"
    Then the {object:node} is properly formatted
      And key "deployment_id" should not be "1"
    Finally REST removes the {object:node} "node-deploy.example.com"
      And REST removes the {object:deployment} "new_deploy"

  Scenario: Node Available Settable
    Given there is a {object:node} "bdd-available-set.example.com"
    When REST sets the {object:node} "bdd-available-set.example.com" {atom:available} state to be "false"
    Then key "available" should be "false"
      And the {object:node} is properly formatted
    Finally REST removes the {object:node} "bdd-available-set.example.com"

  Scenario: Node UI shows alive
    Given there is a {object:node} "bdd-alive-ui.example.com"
    When I go to the "nodes/bdd-alive-ui.example.com" page 
    Then I should see {bdd:crowbar.i18n.common.state.dead}
      And there should be no translation errors
    Finally REST removes the {object:node} "bdd-alive-ui.example.com"

  Scenario: Nodes UI shows alive
    Given there is a {object:node} "bdd-alive-ui.example.com"
    When I go to the "nodes" page 
    Then I should see {bdd:crowbar.i18n.common.state.dead}
      And there should be no translation errors
    Finally REST removes the {object:node} "bdd-alive-ui.example.com"

  Scenario: Node UI shows reserved
    Given there is a {object:node} "bdd-reserved-ui.example.com"
      And REST sets the {object:node} "bdd-reserved-ui.example.com" {atom:available} state to be "false"
    When I go to the "nodes/bdd-reserved-ui.example.com" page 
    Then I should see {bdd:crowbar.i18n.common.state.reserved}
      And there should be no translation errors
    Finally REST removes the {object:node} "bdd-reserved-ui.example.com"

  Scenario: Nodes UI shows reserved
    Given there is a {object:node} "bdd-reserved-ui1.example.com"
      And REST sets the {object:node} "bdd-reserved-ui1.example.com" {atom:available} state to be "false"
    When I go to the "nodes" page 
    Then I should see {bdd:crowbar.i18n.common.state.reserved}
      And there should be no translation errors
    Finally REST removes the {object:node} "bdd-reserved-ui1.example.com"

  Scenario: Node special API for deployment change
    Given there is a {object:node} "bdd-deployment-change.example.com"
      And there is a {object:deployment} "bdd_test1"
    When REST updates an object at "/api/v2/nodes/bdd-deployment-change.example.com" with "{\"node\":{\"deployment\":\"bdd_test1\"}}"
    Then key "deployment_id" should not be "1"
      And key "deployment_id" should match id for {object:deployment}
    Finally REST removes the {object:node} "bdd-deployment-change.example.com"
      And REST removes the {object:deployment} "bdd_test1"

  Scenario: Node loads test data
    Given there is a {object:node} "bdd-discovery.data.edu"
      Given test loads the "node_discovery" data into {object:node} "bdd-discovery.data.edu"
    When REST gets the {object:node} "bdd-discovery.data.edu"
    Then key "discovery" should contain at least "1" items
    Finally REST removes the {object:node} "bdd-discovery.data.edu"