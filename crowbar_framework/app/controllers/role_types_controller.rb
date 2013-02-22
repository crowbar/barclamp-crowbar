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
class RoleTypesController < ApplicationController

  def index
    render api_index :role_type, RoleType.all
  end

  def show
    render api_show :role_type, RoleType
  end

  def create
    rt = RoleType.create params
    render api_show :role_type, RoleType, nil, nil, rt
  end
  
  def destroy
    render api_delete RoleType
  end
      
end
