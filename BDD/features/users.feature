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
 
  Scenario: %Create User 
    Given I am on the "manage_users" page
    When I fill in {fields:username=test_user_1&password=password&password_confirmation=password} and submit using the "Add User" button
    Then I should see {bdd:crowbar.i18n.user.create_success}
    Finally REST deletes the {object:users} "test_user_1"
    
  Scenario: %Sample Form to demo only - remove
    Given I am on the "node/bdd1.example.com/edit" page
    When I fill in {fields:alias=foo&description=bar} and submit using the "Save" button
    Then I should see {bdd:crowbar.i18n.nodes.edit.save_node_success}
      
  Scenario: View Add User Form
    Given I am on the "manage_users" page
    Then I should see "user_username"
      And I should see "user_password"
      And I should see "user_password_confirmation"

  Scenario: Create User via REST then see on UI
    Given there is a user "seemore" with email "foo@bar.com"
    When I go to the "users" page
    Then I should see "seemore"
      And I should see "foo@bar.com"
    Finally REST removes the user "seemore"
        
  Scenario: %Update User
    While interactive
    Given I am on the "manage_users" page
    When I click on the "Manage Users" menu item
    I fill in "user[username]" with "testing123"
      And I fill in "user[password]" with "password"
      And I fill in "user[password_confirmation]" with "password"
      And When I click on the "Add User" button
    Then I should see "User was created successfully."
  
  Scenario: %Lock User
    Given I am on the "manage_users" page
    When I click on the "Manage Users" menu item
    I fill in "user[username]" with "testing123"
      And I fill in "user[password]" with "password"
      And I fill in "user[password_confirmation]" with "password"
      And When I click on the "Add User" button
    Then I should see "User was created successfully."
    
  Scenario: %Unlock User
    Given I am on the "manage_users" page
    When I click on the "Manage Users" menu item
    I fill in "user[username]" with "testing123"
      And I fill in "user[password]" with "password"
      And I fill in "user[password_confirmation]" with "password"
      And When I click on the "Add User" button
    Then I should see "User was created successfully."
    
  Scenario: %Reset Password
    Given I am on the "manage_users" page
    When I click on the "Manage Users" menu item
    I fill in "user[username]" with "testing123"
      And I fill in "user[password]" with "password"
      And I fill in "user[password_confirmation]" with "password"
      And When I click on the "Add User" button
    Then I should see "User was created successfully."
    
  Scenario: %Delete User
    Given I am on the home page
    When I click on the "Manage Users" menu item
    I fill in "user[username]" with "testing123"
      And I fill in "user[password]" with "password"
      And I fill in "user[password_confirmation]" with "password"
      And When I click on the "Add User" button
    Then I should see "User was created successfully."
      
  Scenario: %Create User 
    Given there is not a user "foo"
    Given I am on the "users/create" page
    When I fill in {fields:username=foo&password=bar} and submit using the "Add User" button
    Then I should see {bdd:crowbar.i18n.user.create.success}
    
  Scenario: %Sample Form to demo only - remove
    Given I am on the "node/bdd1.example.com/edit" page
    When I fill in {fields:alias=foo&description=bar} and submit using the "Save" button
    Then I should see {bdd:crowbar.i18n.nodes.edit.save_node_success}
    
  Scenario: REST get user list
    When REST requests the list of users
    Then the list of objects is properly formatted
     
  Scenario: REST Create, Read, and Delete a user
    Given there is not a user "test_user_1"
    When REST adds the user "test_user_1"
    Then there should be a valid user "test_user_1"
    Finally REST removes the user "test_user_1"
    
  Scenario: REST Updates a user email
    Given there is a user "test_user_1"
    When REST modifies user "test_user_1" setting email to "test_user_zed@test.com" 
    Then the user "test_user_1" email should be "test_user_zed@test.com"
    Finally REST removes the user "test_user_1"
    
  Scenario: REST makes user admin
    Given there is a user "test_user_1"
    When REST elevates user "test_user_1" to administrator
    Then the user "test_user_1" is_admin should be "true"
    Finally REST removes the user "test_user_1"
    
  Scenario: REST remove user admin
    Given there is an admin user "test_user_1"
    When REST removes admin privilege for user "test_user_1"
    Then the user "test_user_1" is_admin should be "false"
    Finally REST removes the user "test_user_1"
    
  Scenario: REST resets a user password
    Given there is a user "test_user_1"
    When REST modifies user "test_user_1" setting password and password_confirmation to "password123" 
    Finally REST removes the user "test_user_1"
    
  Scenario: REST locks a user
    Given there is a user "test_user_1"
    When REST locks user "test_user_1"
    Finally REST removes the user "test_user_1"
    
  Scenario: REST unlocks a user
    Given there is a user "test_user_1"
    When REST unlocks user "test_user_1"
    Finally REST removes the user "test_user_1"

