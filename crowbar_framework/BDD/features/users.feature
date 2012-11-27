Feature: Users
  In order manage users
  The system operator, Oscar
  wants to be able to add, edit and delete users

  Scenario: %User Table Renders
    Given I am on the home page 
    When I click on the "Manage Users" menu item
    Then I should see "Manage User Accounts"
      And I should see "Username"
      And I should see "Sign In Count"
      And there should be no translation errors
      
  Scenario: %View Add User Form
    Given I am on the home page
    When I click on the "Manage Users" menu item
    Then I should see "user_username"
      And I should see "user_password"
      And I should see "user_password_confirmation"
      
  Scenario: %Add User
    Given I am on the home page
    When I click on the "Manage Users" menu item
    I fill in "user[username]" with "testing123"
      And I fill in "user[password]" with "password"
      And I fill in "user[password_confirmation]" with "password"
      And When I click on the "Add User" button
    Then I should see "User was created successfully."
    
  Scenario: %Update User
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
    
  Scenario: %Orig Nav to Add User Page
    Given I am on the "manage_users" page 
    When I click on the "Add User" link 
    Then I should see "Node Dashboard"
      
  Scenario: %Retrieve the list of networks via the network API
    Given there is a network "bdd_net"
    When REST requests the list of networks
    Then the object id list is properly formatted
    Finally REST removes the network "bdd_net"
    
    
  Scenario: %BUP User List
    Given there is a user "oscar"
    When REST gets the user list
    Then there should be a value "crowbar"
      And there should be a value "oscar"
    Finally REST removes the user "oscar"
    
  Scenario: %User List
    REST requests the list of users
    Then there should be a value "crowbar"
     
  Scenario: %REST Add User
    Given there is not a user "invisible"
    When REST adds the user "invisible"
    Then there should be a user "invisible"
    Finally REST removes the user "invisible"
    
  