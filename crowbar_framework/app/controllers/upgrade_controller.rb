class UpgradesController < ApplicationController
   def show
     respond_to do |format|
       format.html
     end
   end

   def start
     respond_to do |format|
       format.html {
         redirect_to upgrade_path
       }
     end

     begin
       @service_object = CrowbarService.new logger
       @service_object.prepare_nodes_for_crowbar_upgrade
     rescue => e
       flash[:alert] = e.message
     end
   end

   def update
     respond_to do |format|
       format.html
     end
   end

   def reset
     respond_to do |format|
       format.html {
         redirect_to upgrade_path
       }
     end

     begin
       @service_object = CrowbarService.new logger
       @service_object.revert_nodes_from_crowbar_upgrade
     rescue => e
       flash[:alert] = e.message
     end
   end
end
