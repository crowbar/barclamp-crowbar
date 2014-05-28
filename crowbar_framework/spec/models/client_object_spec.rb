require 'spec_helper'

describe ClientObject do
  describe "finders" do
    describe "interface" do
      it "responds to find_client_by_name" do
        ClientObject.should respond_to(:find_client_by_name)
      end
    end
  end
end
