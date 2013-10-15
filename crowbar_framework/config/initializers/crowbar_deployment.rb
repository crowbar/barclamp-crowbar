# Copyright 2013, Dell 
# 
# Licensed under the Apache License, Version 2.0 (the "License"); 
# you may not use this file except in compliance with the License. 
# You may obtain a copy of the License at 
# 
#  http://www.apache.org/licenses/LICENSE-2.0 
# 
# Unless required by applicable law or agreed to in writing, software 
# distributed under the License is distributed on an "AS IS" BASIS, 
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
# See the License for the specific language governing permissions and 
# limitations under the License. 
# 
# 


# This initializer makes sure we have a Crowbar Default deployment
# it MUST be run after the migrations
begin
  
  # we cannot run the system w/o a deployment
  # we are creating it here until there is a more logical place
  
  if !defined?(::Rake)
    # create the default deployment
    d = Deployment.find_or_create_by_name(:name=> "system",
                                          :description=>I18n.t('automatic', :default=>"Created Automatically by System"))
    # put this deployment into action because system deployment cannot be proposed
    d.commit if d.proposed?
  end
end
