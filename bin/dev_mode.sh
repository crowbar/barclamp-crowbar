#!/bin/bash
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

# clean up
service crowbar stop

# start dev version of the server
if [[ pwd = "/tmp/crowbar-dev-test/opt/dell/crowbar_framework" ]]; then
  ~/crowbar/dev reload-unit-tests
else        

  # create/update the database
  export RAILS_ENV=development
  export DEBUG=true 
  cd /opt/dell/crowbar_framework/
  chmod 777 -R .
  chown crowbar -R .
  rake db:create rake db:migrate rake db:schema:dump

  # inject the chef server from the local admin
  echo To install an admin node in dev mode, pleass use
  echo curl --digest -u crowbar:crowbar -X POST http://localhost:3000/api/v2/nodes -d "name=$FQDN" -d 'admin=true' -d 'alive=true'

  # startup the web server
  bundle exec rails s Puma
  
fi
