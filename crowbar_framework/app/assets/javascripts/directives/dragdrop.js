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
    'draggable', 
    [
      '$rootScope',
      function($rootScope) {
        var parseResponse = function(event, ui) {
          return {
            source: ui.helper.context,
            target: event.target,
            offset: ui.offset,
            position: ui.position,
            type: event.type
          }
        }

        return {
          restrict: 'A',
          scope: {
            onDrag: '&',
            onStart: '&',
            onStop: '&'
          },
          link: function(scope, el, attrs, controller) {
            angular.element(el).addClass('draggable');

            $(angular.element(el)).draggable({
              opacity: 1,
              zIndex: 999,
              helper: "clone",
              drag: function(event, ui) {
                return scope.onDrag(parseResponse(event, ui));
              },
              start: function(event, ui) {
                return scope.onStart(parseResponse(event, ui));
              },
              stop: function(event, ui) {
                return scope.onStop(parseResponse(event, ui));
              }
            });
          }
        }
      }
    ]
  )

  .directive(
    'droppable', 
    [
      '$rootScope',
      function($rootScope) {
        var parseResponse = function(event, ui) {
          return {
            source: ui.helper.context,
            target: event.target,
            offset: ui.offset,
            position: ui.position,
            type: event.type
          }
        }

        return {
          restrict: 'A',
          scope: {
            onActivate: '&',
            onDeactivate: '&',
            onOut: '&',
            onOver: '&',
            onDrop: '&'
          },
          link: function(scope, el, attrs, controller) {
            $(angular.element(el)).droppable({
              hoverClass: 'targeted',
              activate: function(event, ui) {
                return scope.onActivate(parseResponse(event, ui));
              },
              deactivate: function(event, ui) {
                return scope.onDeactivate(parseResponse(event, ui));
              },
              out: function(event, ui) {
                return scope.onOut(parseResponse(event, ui));
              },
              over: function(event, ui) {
                return scope.onOver(parseResponse(event, ui));
              },
              drop: function(event, ui) {
                return scope.onDrop(parseResponse(event, ui));
              }
            });
          }
        }
      }
    ]
  );
