require 'spec_helper'

describe ClientObject do
  describe "finders" do
    describe "find_client_by_name" do
      it "responds to it" do
        ClientObject.should respond_to(:find_client_by_name)
      end

      it "prints a deprecation warning" do
        ClientObject.stubs(:load).returns(true)
        ClientObject.expects(:deprecate_warning).once
        ClientObject.find_client_by_name("test")
      end
    end
  end
end
