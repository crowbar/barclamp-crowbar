
require 'test_helper'
 
class NodeModelTest < ActiveSupport::TestCase

  test "Unique Name" do
    Node.create :name=>"foo"
    e = assert_raise(ActiveRecord::RecordInvalid) { Node.create!(:name => "foo") }
    assert_equal "Validation failed: Name Name item must be unique", e.message

    b = Node.create(:name => "foo")
    b = b.save
    assert_equal false, b
  end

  test "Naming Conventions" do
    assert_raise(ActiveRecord::RecordInvalid) { Node.create(:name=>"1123") }
    assert_raise(ActiveRecord::RecordInvalid) { Node.create(:name=>"1foo") }
    assert_raise(ActiveRecord::RecordInvalid) { Node.create(:name=>"Ille!gal") }
    assert_raise(ActiveRecord::RecordInvalid) { Node.create(:name=>" nospaces") }
    assert_raise(ActiveRecord::RecordInvalid) { Node.create(:name=>"no spaces") }
    assert_raise(ActiveRecord::RecordInvalid) { Node.create(:name=>"nospacesatall ") }
    end
  end

end

