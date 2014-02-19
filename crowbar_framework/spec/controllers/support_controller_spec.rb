require 'spec_helper'

describe SupportController do
  describe "GET export_chef" do
    it "displays flash message on error" do
      NodeObject.stubs(:all).raises(StandardError)
      get :export_chef
      response.should redirect_to(utils_url)
      flash[:alert].should_not be_empty
    end
  end
end
