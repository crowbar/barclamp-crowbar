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
    'crowbar.factories', 
    []
  )

  .factory(
    '$notification', 
    [
      function() {
        var parseOptions = function(options) {
          return angular.extend(
            {
              type: 'success',
              allow_dismiss: true,
              width: 400,
              delay: 5000,
              offset: {
                from: 'top',
                amount: 70
              }
            },
            options
          );
        }

        var service = {
          successMessage: function(message, options) {
            $.bootstrapGrowl(
              message,
              parseOptions(angular.extend({
                type: 'success'
              }, options))
            );
          },
          infoMessage: function(message, options) {
            $.bootstrapGrowl(
              message,
              parseOptions(angular.extend({
                type: 'info'
              }, options))
            );
          },
          warningMessage: function(message, options) {
            $.bootstrapGrowl(
              message,
              parseOptions(angular.extend({
                type: 'warning'
              }, options))
            );
          },
          alertMessage: function(message, options) {
            $.bootstrapGrowl(
              message,
              parseOptions(angular.extend({
                type: 'danger'
              }, options))
            );
          },
          genericMessage: function(message, options) {
            $.bootstrapGrowl(
              message,
              parseOptions(options)
            );
          }
        };

        return service;
      }
    ]
  );
