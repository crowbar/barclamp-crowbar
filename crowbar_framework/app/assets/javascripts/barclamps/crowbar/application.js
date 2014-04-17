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
    'crowbar.barclamps',
    []
  )

  .controller(
    'CrowbarCtrl', 
    [
      '$scope',
      function($scope) {
 
      }
    ]
  )

  .controller(
    'DashboardCtrl',
    [
      '$scope',
      '$translate',
      '$notification',
      '$websocket',
      '$dashboard',
      function($scope, $translate, $notification, $websocket, $dashboard) {
        // $websocket.subscribe('/nodes/status', function(data) {
        //   $dashboard.pushNode(data.node)
        //     .then(function(data) {
        //       $scope.groups = data;
        //     });
        // });

        $dashboard.getGroups()
          .then(function(data) {
            $scope.groups = data;
          });

        $dashboard.countNodes()
          .then(function(data) {
            $scope.counter = data;
          });

        $scope.addGroup = function() {
          if (
            $scope.addGroupForm.groupName.$dirty 
            && 
            $scope.addGroupForm.groupName.$invalid
          ) {
            $translate('dashboard.errors.invalid_group').then(function(val) {
              $notification.alertMessage(val);
            });

            return false;
          }

          if (
            $scope.addGroupForm.groupName.$dirty 
            && 
            $scope.groups[$scope.group.name]
          ) {
            $translate('dashboard.errors.existing_group').then(function(val) {
              $notification.alertMessage(val);
            });

            return false;
          }

          $dashboard.addGroup($scope.group);
          $scope.group.name = '';
        }

        $scope.removeGroup = function(handle) {
          $dashboard.removeGroup(handle);
        }

        $scope.onActivate = function(source, target) {
          angular.element(target).addClass('droppable');
        }

        $scope.onDeactivate = function(source, target) {
          angular.element(target).removeClass('droppable');
        }

        $scope.onDrop = function(source, target) {
          var node = angular.element(source);
          var nodeId = node.data('id');

          var group = angular.element(target);
          var groupId = group.data('id');

          var oldId = node.data('parent');

          $dashboard.moveNode(nodeId, oldId, groupId)
            .then(function(data) {
              $scope.groups = data;
            });
        }
      }
    ]
  )

  .factory(
    '$dashboard',
    [
      '$q',
      '$http',
      '$translate',
      function($q, $http, $translate) {
        var service = {
          _groups: null,

          getGroups: function() {
            var d = $q.defer();

            $http.get(Routes.dashboard_path({ format: 'json' }))
              .success(function(data, status) {
                service._groups = data;
                d.resolve(service._groups);
              })
              .error(function(data, status) {
                d.reject(data);
              });

            return d.promise;
          },
          addGroup: function(values) {
            service._groups[values.name] = {
              handle: values.name,
              title: values.name,
              automatic: false,
              nodes: {},
              status: {
                ready: 0,
                failed: 0,
                unknown: 0,
                unready: 0,
                pending: 0
              }
            };
          },
          removeGroup: function(handle) {
            delete service._groups[handle];
          },
          moveNode: function(node, oldGroup, newGroup) {
            var d = $q.defer();

            $http.post(Routes.group_node_path(node, { format: 'json' }), { group: newGroup })
              .success(function(data, status) {
                values = service._groups[oldGroup]['nodes'][node];
                delete service._groups[oldGroup]['nodes'][node];

                if (service._groups[data.group]) {
                  service._groups[data.group]['nodes'][node] = values;
                } else {
                  service.addGroup({ name: data.group })
                  service._groups[data.group]['nodes'][node] = values;
                }

                d.resolve(service._groups);
              })
              .error(function(data, status) {
                d.reject(data);
              });

            return d.promise;
          },
          pushNode: function(node) {
            var d = $q.defer();

            service.getGroups()
              .then(function(data) {
                var nodeId = node.handle;
                var groupId = node.group;

                if (!service._groups[groupId]) {
                  service.addGroup({ name: node.group });
                }

                if (!service._groups[groupId]['nodes'][nodeId]) {
                  service._groups[groupId]['nodes'][node.handle] = node;
                }

                service._groups[groupId]['nodes'][node.handle]['state'] = node.state;
                service._groups[groupId]['nodes'][node.handle]['status'] = node.status;

                d.resolve(service._groups);
              });

            return d.promise;
          },
          countNodes: function() {
            var d = $q.defer();

            service.getGroups()
              .then(function(data) {
                var counter = 0;

                angular.forEach(data, function(group, key) {
                  counter += Object.keys(group.nodes).length;
                });

                switch(counter) {
                  case 1:
                    $translate('dashboard.index.nodes.one', { count: counter }).then(function(val) {
                      d.resolve(val);
                    });
                    break;
                  default:
                    $translate('dashboard.index.nodes.other', { count: counter }).then(function(val) {
                      d.resolve(val);
                    });
                    break;
                }
              });

            return d.promise;
          }
        };

        return service;
      }
    ]
  );




//       $('[data-droppable=true]:last').droppable({
//         hoverClass: 'targeted',
//         drop: function(event, ui) {
//           var $group = $(event.target);
//           var $node = $(ui.draggable.context);

//           var inserted = $.map(
//             $group.find('li[data-draggable=true]'),
//             function(node) {
//               return $(node).data('id');
//             }
//           );

//           if ($.inArray($node.data('id'), inserted) < 0) {
//             $.post(
//               $node.data('update'),
//               {
//                 group: $group.data('id')
//               },
//               function() {
//                 if ($group.data('id') == 'AUTOMATIC') {
//                   location.reload();
//                   return true;
//                 }

//                 var $ul = $group.find('ul');
//                 var $oldUl = $node.parents('ul');

//                 $ul.append(
//                   $node
//                 );

//                 $ul.find('li.empty').remove();

                
//                 After reordering list items we loose the draggable functionality

//                 $ul.html(
//                   $ul.children().sort(function(a, b) {
//                     return $(a).text().toUpperCase().localeCompare($(b).text().toUpperCase());
//                   })
//                 );
                

//                 if ($oldUl.children().length <= 0) {
//                   $oldUl.parents('[data-droppable=true]').remove();
//                 }
//               }
//             );
//           }

//           return false;
//         }
//       });







// jQuery(document).ready(function($) {
//   $('.list-group-item a').on('click', function(event) {
//     $('.list-group-item').removeClass('selected');
//     $(this).parents('.list-group-item').addClass('selected');

//     $('#nodedetails').load(
//       '{0} .panel'.format($(this).data('href'))
//     );
//   });

//   $("[data-draggable=true]").draggable({
//     opacity: 0.9,
//     helper: "clone"
//   });

//   $("[data-droppable=true]").droppable({
//     hoverClass: 'targeted',
//     drop: function(event, ui) {
//       var $group = $(event.target);
//       var $node = $(ui.draggable.context);

//       var inserted = $.map(
//         $group.find('li[data-draggable=true]'),
//         function(node) {
//           return $(node).data('id');
//         }
//       );

//       if ($.inArray($node.data('id'), inserted) < 0) {
//         $.post(
//           $node.data('update'),
//           {
//             group: $group.data('id')
//           },
//           function() {
//             if ($group.data('id') == 'AUTOMATIC') {
//               location.reload();
//               return true;
//             }

//             var $ul = $group.find('ul');
//             var $oldUl = $node.parents('ul');

//             $ul.append(
//               $node
//             );

//             $ul.find('li.empty').remove();

//             /*
//             After reordering list items we loose the draggable functionality

//             $ul.html(
//               $ul.children().sort(function(a, b) {
//                 return $(a).text().toUpperCase().localeCompare($(b).text().toUpperCase());
//               })
//             );
//             */

//             if ($oldUl.children().length <= 0) {
//               $oldUl.parents('[data-droppable=true]').remove();
//             }
//           }
//         );
//       }

//       return false;
//     }
//   });
// });



// jQuery(document).ready(function($) {
//   if ($.queryString['waiting'] != undefined) {
//     setInterval(
//       function() {
//         var meta = $('.row[data-update]');

//         $.getJSON(meta.data('update').replace('FILENAME', meta.data('file')), function(data) {
//           if (data['waiting'] == false) {
//             location.href = meta.data('redirect').replace('FILENAME', meta.data('file'));
//           }
//         });
//       },
//       10000
//     );
//   }
// });
