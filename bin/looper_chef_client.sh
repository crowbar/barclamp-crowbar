#!/bin/bash
#
# Copyright 2011-2013, Dell
# Copyright 2013-2014, SUSE LINUX Products GmbH
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
#
# TODO: Needs to be replaced by https://github.com/basecamp/sub
#
#

[[ -f /var/run/crowbar/deploying ]] && exit 0

lockfile="/var/run/crowbar/looper-chef-client.lock"
needrunfile="/var/run/crowbar/chef-client.run"

touch "$needrunfile"

obtained_lock=''

# Is there already a lock file?
if [ -e "$lockfile" ]; then
    pid="$(<$lockfile)"
    # Does the process no longer run ?
    if ! printf "%d" "$pid" &>/dev/null || ! kill -0 "$pid" >/dev/null 2>/dev/null; then
        echo "$$" > "$lockfile"
        obtained_lock='true'
    fi
fi

if ( set -o noclobber; echo "$$" > "$lockfile") 2> /dev/null; then
    obtained_lock='true'
fi

if [ -n "$obtained_lock" ]; then
    trap 'rm -f "$lockfile"; exit $?' INT TERM EXIT
    
    while true; do
	rm -f "$needrunfile"
	
	/opt/dell/bin/blocking_chef_client.sh
	
	while [[ ! -f "$needrunfile" ]]
	do
	    sleep 1
	done
    done

  # Ideally these won't be hit.
    rm -f "$lockfile"
    trap - INT TERM EXIT
    exit 0
fi

