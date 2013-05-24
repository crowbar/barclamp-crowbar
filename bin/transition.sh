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


key_re='crowbar\.install\.key=([^ ]+)'
if [[ $(cat /proc/cmdline) =~ $key_re ]]; then
    export CROWBAR_KEY="${BASH_REMATCH[1]}"
elif [[ -f /etc/crowbar.install.key ]]; then
    export CROWBAR_KEY="$(cat /etc/crowbar.install.key)"
fi

HOST="127.0.0.1"

post_state() {
  local curlargs=(-o "/var/log/crowbar/$1-$2.json" --connect-timeout 60 -s \
      -L -X POST --data-binary "{ \"name\": \"$1\", \"state\": \"$2\" }" \
      -H "Accept: application/json" -H "Content-Type: application/json")
  [[ $CROWBAR_KEY ]] && curlargs+=(-u "$CROWBAR_KEY" --digest --anyauth)
  curl "${curlargs[@]}" "http://${HOST}:3000/crowbar/crowbar/1.0/transition/default"
}

if [ "$1" == "" ]
then
  echo "Please specify a node to transition"
  exit -1
fi

if [ "$2" == "" ]
then
  echo "Please specify a state for $1 to transition"
  exit -1
fi

knife node list | while read line
do
  if [ $1 == $line ]
  then
    echo "Transitioning node $1 to state $2"
    post_state $1 $2
    break
  fi
done

exit 0
