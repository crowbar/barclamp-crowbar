
require 'test_helper'
 
class GroupModelTest < ActiveSupport::TestCase

  test "Naming Conventions" do
    assert_raise(ActiveRecord::RecordInvalid) { Group.create(:name=>"1123") }
    assert_raise(ActiveRecord::RecordInvalid) { Group.create(:name=>"1foo") }
    assert_raise(ActiveRecord::RecordInvalid) { Group.create(:name=>"Ille!gal") }
    assert_raise(ActiveRecord::RecordInvalid) { Group.create(:name=>" nospaces") }
    assert_raise(ActiveRecord::RecordInvalid) { Group.create(:name=>"no spaces") }
    assert_raise(ActiveRecord::RecordInvalid) { Group.create(:name=>"nospacesatall ") }
    end
  end

end

