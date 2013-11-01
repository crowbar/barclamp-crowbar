### Attribute (aka Attrib) APIs

Attribute models are used to map/expose attribute data attached to system objects.  They do NOT actually store any data, they are just maps.

> To prevent Rails name collisions, Crowbar uses 'Attrib' instead of Attribute.

For now, they are tied to nodes only.  While they can be linked to data from a wide range of sources, they are generally retrieved from json stored in the node.discovery field.

Attribs.value can be customized by subclassing when they are created.  Subclassing allows the attrib to return data composed in more complex ways than simple json lookups using the automatic mapping method.

#### API Actions

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Comments </th></tr>
<tr><td> GET  </td>
  <td> api/v2/attribs </td>
  <td> List </td></tr>
<tr><td> GET  </td>
  <td> api/v2/attribs/:id </td>
  <td> Specific Item </td></tr>
<tr><td> PUT  </td>
  <td> api/v2/attribs/:id </td>
  <td> Update Item </td></tr>
<tr><td> POST  </td>
  <td> api/v2/attribs </td>
  <td> Create Item </td></tr>
<tr><td> DELETE  </td>
  <td> api/v2/attribs/:id </td>
  <td> Delete Item </td></tr>
</table>

#### Attribs on Nodes

After an attrib is set, you can use it to easily retrieve information from a node using the method missing pattern.  For example, the "asset_tag" attribute can be retrieved on a node using =node.attrib_asset_tag= to return the node's asset tag.

Attibs may return simple strings or complex json hashes depending on how they have been defined.