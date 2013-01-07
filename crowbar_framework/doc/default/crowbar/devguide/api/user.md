
The user API is used to manage users.

#### User CRUD: List

Lists the current users.

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> GET  </td><td>crowbar/2.0/user/2.0/users</td><td>N/A</td><td>JSON array of user ID:Username pairs</td><td></td></tr>
</table>


**Output:**

	{
	  "32": "test123",
	  "1": "developer",
	  "2": "crowbar",
	  "3": "machine-install",
	  "4": "davpat2112"
	}


#### User CRUD: Show

Shows details about a selected user.

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> GET  </td><td>crowbar/2.0/user/2.0/users/[id]</td><td>id is the user ID or username.</td><td>Details of the user in JSON format</td><td></td></tr>
</table>


**Output:**

    {
     "created_at": "2012-12-12T18:56:14Z",
     "updated_at": "2013-01-01T06:27:33Z",
     "username": "crowbar",
     "id": 2,
     "is_admin": true,
     "email": "email@emai.com"
    }


#### User CRUD: Create

Creates a new user.

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> POST  </td><td>crowbar/2.0/user/2.0/users</td><td></td><td> User json definition (see User CRUD: Show) </td><td> must be a valid user object </td></tr>
</table>

    {
      "username":"testuser1",
      "email":"test1@domain.com",
      "password":"password123",
      "password_confirmation":"password123",
      "remember_me":"false",
      "is_admin":"false"
    }

Details:


* username - The unique username (must be letters and numbers, and must start with a letter)
* email - well formed unique and valid email address
* password - password field (must meet password strength requirement)
* password_confirmation - password confirmation field
* remember_me - when user logs into UI will a cookie be set so username field is prepopulated.
* is_admin - will user have admin privileges, (create new update existing users in ui)

**Output:**

    {
     "created_at": "2012-12-12T18:56:14Z",
     "updated_at": "2013-01-01T06:27:33Z",
     "username": "crowbar",
     "id": 2,
     "is_admin": true,
     "email": "email@emai.com"
    }
    
#### User CRUD: Update

Updates existing user.

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> PUT  </td><td>crowbar/2.0/user/2.0/users/[id]</td><td></td><td> User json definition (see User CRUD: Show) </td><td></td></tr>
</table>


    {
      "id":"1",
      "username":"testuser1x",
      "email":"testuser1x@domain.com",
      "remember_me":"false",
      "is_admin":"false"
    }

Details:

* id - the ID or username of the user to update
* username - unique username (must be letters and numbers, and must start with a letter)
* email - well formed unique and valid email address
* remember_me - when user logs into UI will a cookie be set so username field is prepopulated.
* is_admin - will user have admin privileges, (create new update existing users in ui)


**Output:**

    {
     "created_at": "2012-12-12T18:56:14Z",
     "updated_at": "2013-01-01T06:27:33Z",
     "username": "crowbar",
     "id": 2,
     "is_admin": true,
     "email": "email@emai.com"
    }

#### Reset User Password

Change existing user password

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> PUT  </td><td>crowbar/2.0/user/2.0/users/reset_password/[id]</td><td></td><td> User json definition (see User CRUD: Show) </td><td></td></tr>
</table>


    {
      "id":"1",
      "password":"password123",
      "password_confirmation":"password123"
    }

Details:

* id - the ID or username of the user to update
* password - password field (must meet password strength requirement)
* password_confirmation - password confirmation field


**Output:**

    {
     "created_at": "2012-12-12T18:56:14Z",
     "updated_at": "2013-01-01T06:27:33Z",
     "username": "crowbar",
     "id": 2,
     "is_admin": true,
     "email": "email@emai.com"
    } 
 
#### Lock User

Lock existing user account.

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> POST </td><td>crowbar/2.0/user/2.0/users/lock/[id]</td><td>id is the user ID or username</td><td> User json definition (see User CRUD: Show) </td><td></td></tr>
</table>


**Output:**

    {
     "created_at": "2012-12-12T18:56:14Z",
     "updated_at": "2013-01-01T06:27:33Z",
     "username": "crowbar",
     "id": 2,
     "is_admin": true,
     "email": "email@emai.com"
    }
    
    
#### Unlock User

Unlock existing user account.

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> DELETE </td><td>crowbar/2.0/user/2.0/users/lock/[id]</td><td>id is the user ID or username</td><td> User json definition (see User CRUD: Show) </td><td></td></tr>
</table>


**Output:**

    {
     "created_at": "2012-12-12T18:56:14Z",
     "updated_at": "2013-01-01T06:27:33Z",
     "username": "crowbar",
     "id": 2,
     "is_admin": true,
     "email": "email@emai.com"
    }
    
#### Make User Admin

Add user administrator priviledge to existing user.

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> POST </td><td>crowbar/2.0/user/2.0/users/admin/[id]</td><td>id is the user ID or username</td><td> User json definition (see User CRUD: Show) </td><td></td></tr>
</table>


**Output:**

    {
     "created_at": "2012-12-12T18:56:14Z",
     "updated_at": "2013-01-01T06:27:33Z",
     "username": "crowbar",
     "id": 2,
     "is_admin": true,
     "email": "email@emai.com"
    }    
    
    
#### Remove User Admin

Delete user administrator priviledge from existing user.

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> DELETE </td><td>crowbar/2.0/user/2.0/users/admin/[id]</td><td>id is the user ID or username</td><td> User json definition (see User CRUD: Show) </td><td></td></tr>
</table>


**Output:**

    {
     "created_at": "2012-12-12T18:56:14Z",
     "updated_at": "2013-01-01T06:27:33Z",
     "username": "crowbar",
     "id": 2,
     "is_admin": true,
     "email": "email@emai.com"
    } 
    

#### User CRUD: Delete

Deletes a user.

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> DELETE  </td><td>crowbar/2.0/user/2.0/users/[id]</td><td> User ID or username </td><td>HTTP error code 200 on success</td><td></td></tr>
</table>

No body.

**Output:**

None.
