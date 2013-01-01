Feature: Users
  In order manage users
  The system operator, Oscar
  wants to be able to add, edit and delete users

  Scenario: User Table Renders
    Given I am on the home page 
    When I click on the "Manage Users" menu item
    Then I should see "Manage User Accounts"
      And I should see "Username"
      And I should see "Sign In Count"
      And there should be no translation errors
      
  Scenario: %Nav to Add User Page
    Given I am on the "manage_users" page 
    When I click on the "Add User" link 
    Then I should see "Node Dashboard"
      
  Scenario: User List
    Unless undefined
    Given there is a user "oscar"
    When REST gets the user list
    Then there should be a value "crowbar"
      And there should be a value "oscar"
    Finally REST removes the user "oscar"
  
  Scenario: REST Add User
    Skip this is not working
    Given there is not a user "invisible"
    When REST adds the user "invisible"
    Then there should be a user "invisible"
    Finally REST removes the user "invisible"
    
  Scenario: %Create User 
    Given there is not a user "foo"
    Given I am on the "users/create" page
    When I fill in {fields:username=foo&password=bar} and submit using the "Add User" button
    Then I should see {bdd:crowbar.i18n.user.create.success}
    
  Scenario: %Sample Form to demo only - remove
    Given I am on the "node/bdd1.example.com/edit" page
    When I fill in {fields:alias=foo&description=bar} and submit using the "Save" button
    Then I should see {bdd:crowbar.i18n.nodes.edit.save_node_success}
