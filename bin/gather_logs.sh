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

# MOVED TO LOGGING BARCLAMP!!!
# DELETE ME (if after 1/30/12)

if [[ -f /etc/crowbar.install.key ]]; then
    export CROWBAR_KEY=$(cat /etc/crowbar.install.key)
fi
mkdir -p /tmp/crowbar-logs
tarname="${1-$(date '+%Y%m%d-%H%M%S')}"
targetdir="/opt/dell/crowbar_framework/public/export"
sort_by_last() {
    local src=() keys=() sorted=() line=""
    while read line; do
	[[ $line && $line != '.' && $line != '..' ]] || continue
	src+=("$line");
	keys+=("${line##*/}")
    done
    while read line; do
	echo "${src[$line]}" |tee -a "$targetdir/debug.log"
    done < <( (for i in "${!keys[@]}"; do 
	    echo "$i ${keys[$i]}"; done) | \
	sort -k 2 | \
	cut -d ' ' -f 1)
}
	
    
(   flock -s 200
    logdir=$(mktemp -d "/tmp/crowbar-logs/$tarname-XXXXX")
    mkdir -p "$logdir"
    mkdir -p "$targetdir"
    cd "$logdir"
    sshopts=(-q -o 'StrictHostKeyChecking no' 
	-o 'UserKnownHostsFile /dev/null')
    logs=(/var/log /etc /opt/dell/crowbar_framework/log /install-logs)
    logs+=(/var/chef/cache /var/cache/chef /opt/dell/crowbar_framework/db)
    curlargs=(-o /dev/null -D - --connect-timeout 30 --max-time 120)
    [[ $CROWBAR_KEY ]] && curlargs+=(--digest -u "$CROWBAR_KEY")
    for to_get in nodes proposals roles; do
	curl "${curlargs[@]}" "http://localhost:3000/$to_get" || :
    done
    for node in $(sudo -H knife node list); do
	mkdir -p "${node%%.*}"
	tarfile="${node%%.*}-${tarname}.tar.gz"
	(   cd "${node%%.*}"
	    sudo ssh "${sshopts[@]}" "${node}" \
		tar czf - "${logs[@]}" |
	    tar xzf -
	)&
    done &>/dev/null
    wait
    cd ..
    find . -depth -print | \
	sort_by_last / | \
	cpio -o -H ustar | \
	bzip2 -9 > "$targetdir/${tarname}"
    rm -rf "$logdir"
) 200>/tmp/crowbar-logs/.lock
