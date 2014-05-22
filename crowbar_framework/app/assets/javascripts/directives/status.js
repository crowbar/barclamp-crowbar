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
    'crowbar.directives', 
    []
  )

  .directive(
    'nodeStatus', 
    [
      '$rootScope',
      '$serverevent',
      function($rootScope, $serverevent) {
        return {
          restrict: 'A',
          link: function(scope, el, attrs, controller) {
            $serverevent.subscribe('/nodes/status', function(data) {
              if (data.node.handle === attrs.nodeStatus) {
                scope.node.state = data.node.state;
                scope.node.status = data.node.status;

                console.log('transition', attrs.nodeStatus, data.node.state);
              }
            });
          }
        }
      }
    ]
  );
