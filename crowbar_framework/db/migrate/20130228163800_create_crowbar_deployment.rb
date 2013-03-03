class CreateCrowbarDeployment < ActiveRecord::Migration
  def up
    # we cannot run the system w/o a crowbar deployment
    # we are creating it her until there is a more logical place
    bc = Barclamp.find_by_name 'crowbar'
    template = bc.create_proposal
  end
end
