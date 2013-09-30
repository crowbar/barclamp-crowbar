#!/bin/bash
# Copyright 2011, Dell
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


[[ -f /var/run/crowbar/deploying ]] && exit 0

lockfile="/var/run/crowbar/looper-chef-client.lock"

if ( set -o noclobber; echo "$$" > "$lockfile") 2> /dev/null; then
    trap 'rm -f "$lockfile"; exit $?' INT TERM EXIT
    
    while true; do
	rm -f /var/run/crowbar/chef-client.run
	
	/opt/dell/bin/blocking_chef_client.sh
	
	while [[ ! -f /var/run/crowbar/chef-client.run ]]
	do
	    sleep 1
	done
    done

  # Ideally these won't be hit.
    rm -f "$lockfile"
    trap - INT TERM EXIT
    exit 0
else
    touch /var/run/crowbar/chef-client.run
fi

