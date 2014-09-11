class ClustersController < ApplicationController
  def index
    render :nothing => true
  end

  def roles
    @nodes    = NodeObject.all
  end
end
