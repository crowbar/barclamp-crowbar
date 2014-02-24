require 'spec_helper'

describe DocsController do
  describe "GET index" do
    it "is successful" do
      get :index
      response.should be_success
    end
  end
end

