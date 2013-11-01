### User API

The user API is used to manage users.

#### API Actions

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Comments </th></tr>
<tr><td> GET  </td>
  <td> api/v2/users </td>
  <td> List </td></tr>
<tr><td> GET  </td>
  <td> api/v2/users/:id </td>
  <td> Specific Item </td></tr>
<tr><td> PUT  </td>
  <td> api/v2/users/:id </td>
  <td> Update Item </td></tr>
<tr><td> POST  </td>
  <td> api/v2/users </td>
  <td> Create Item </td></tr>
<tr><td> DELETE  </td>
  <td> api/v2/users/:id </td>
  <td> Delete Item </td></tr>
<tr><td> VARIOUS  </td>
  <td> api/v2/users/:id/extra </td>
  <td> Special Ops </td></tr>

</table>

* username - The unique username (must be letters and numbers, and must start with a letter)
* email - well formed unique and valid email address
* password - password field (must meet password strength requirement)
* password_confirmation - password confirmation field
* remember_me - when user logs into UI will a cookie be set so username field is prepopulated.
* is_admin - will user have admin privileges, (create new update existing users in ui)

