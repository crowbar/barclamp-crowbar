require 'spec_helper'

describe ClientObject do
  let(:co) { ClientObject }

  describe "finders" do
    describe "interface" do
      it "responds to find_client_by_name" do
        co.should respond_to(:find_client_by_name)
      end
    end
  end
end
