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

# If we are deploying the admin node, this is a NOOP.
[[ -f /tmp/deploying ]] && exit

# Otherwise, allow there to be up to 1 process actaully running chef-client
# and 1 process waiting to run chef-client, no matter how many times
# we are invoked.
runlock="/tmp/.chef-client-running.lock"
waitlock="/tmp/.chef-client-waiting.lock"
waitpid="/tmp/.chef-client-waiting.pid"
lockfile="/tmp/chef-client.lock"
{
    if ! flock -n 98; then
        # There is a chef-client running and a chef-client waiting.
        # Wait on the one that is waiting to exit, and then exit ourselves.
        echo "Chef-client run already queued as $(cat "$waitpid"), will wait for it and exit."
        flock 98
        exit
    fi
    if ! flock -n 99; then
        echo "$$" > "$waitpid"
        echo "Waiting on active chef-client run ($(cat "$lockfile")), will run when it is done."
        # No chef-client is waiting, but there may be one running.
        # Wait for the run lock, drop the wait lock, and then run ourselves.
        flock 99
        rm "$waitpid"
    fi
    flock -u 98
    echo "$$" > "$lockfile"
    for loglvl in info; do
        while { chef-client -l "$loglvl"; ret=$?; ((ret >= 128)); }; do
	    echo "Chef-client exited with error code $ret, retrying"
	    sleep 1
        done
        (( ret == 0 )) && break
        case $loglvl in
	    info) echo "Chef client run failed, will retry with debugging.";;
	    debug) echo "Chef client run failed with debug enabled.";;
        esac
    done
    rm -f "$lockfile"
    flock -u 99
} 98>"$waitlock" 99>"$runlock"
exit $ret
