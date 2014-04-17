//
// Copyright 2011-2013, Dell
// Copyright 2013-2014, SUSE LINUX Products GmbH
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

angular
  .module(
    'crowbar.controllers', 
    []
  )

  .controller(
    'ApplicationCtrl', 
    [
      '$scope',
      '$translate',
      '$notification',
      '$websocket',
      function($scope, $translate, $notification, $websocket) {
        $websocket.subscribe('/general/flash', function(data) {
          $notification.genericMessage(data.message, {
            type: data.type ? data.type : 'info',
            allow_dismiss: data.dismiss ? data.dismiss : false
          });
        });

        $websocket.subscribe('/nodes/status', function(data) {
          $translate(
            'nodes.errors.node_transition', 
            { node: data.node.alias, state: data.node.state }
          ).then(function(val) {
            $notification.infoMessage(val);
          });
        });
      }
    ]
  );
