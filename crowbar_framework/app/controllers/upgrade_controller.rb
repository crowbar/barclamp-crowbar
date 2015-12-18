class UpgradeController < ApplicationController
  def upgrade
    respond_to do |format|
      format.html
    end
  end

  def running_upgrade
    respond_to do |format|
      format.html
    end
  end
end
