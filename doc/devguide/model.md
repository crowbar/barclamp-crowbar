### Crowbar Data Models

#### Lionel

The data models in Crowbar are expressed in ActiveRecord

Namespacing:  Individual Barclamps can add models, but are expected to add them 
in their own namespace.


Example:  Foo Barclamp with a model for Attrib::FooBar with a subclass for Attrib

BarclampFoo::Attrib
Path = `app/models/foo/attrib_foo_bar.rb`
Class = `Foo::AttribFooBar < Attrib`




