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
    'service.localization', 
    []
  )

  .factory(
    'localize',
    [
      '$http', '$rootScope', '$window',
      function($http, $rootScope, $window) {
        'use strict';

        var localize = {
          path: 'translations/',
          language: 'en',
          dictionary: undefined,
          resourceLoaded: false,
          successCallback: function (data) {
            localize.dictionary = data;
            localize.resourceLoaded = true;

            $rootScope.$broadcast('localizationUpdates');
          },
          errorCallback: function() {
            var url = localize.path + 'en.json';

            $http({ 
              method:"GET", 
              url: url, 
              cache: false 
            }).success(
              localize.successCallback
            );
          },
          initLocalization: function () {
            var url = localize.path + localize.language + '.json';

            $http({ 
              method:"GET", 
              url: url, 
              cache: false
            }).success(
              localize.successCallback
            ).error(
              localize.errorCallback
            );
          },
          getLocalizedString: function (value) {
            var translated = '!' + value + '!';

            if (!localize.resourceLoaded) {
              localize.initLocalization();
              localize.resourceLoaded = true;

              return translated;
            }

            if (typeof localize.dictionary === "object") {
              var logUntranslated = false;
              var placeholders = [];

              for (var i = 1; i < arguments.length; i++) {
                placeholders.push(arguments[i]);
              }

              var translate = function(value, placeholders) {
                var placeholders = placeholders || null;
                var translated = localize.dictionary[value];

                if (translated === undefined) {
                  return sprintf(value, placeholders);
                }

                return sprintf(translated, placeholders);
              };

              var result = translate(value, placeholders);

              if (translated !== null && translated != undefined) {
                translated = result;
              }
            }

            return translated;
          },

          replace: function(elm, str) {
            var tag = localize.getLocalizedString(str);

            if (tag !== null && tag !== undefined && tag !== '') {
              elm.html(tag);
            }
          }
        };

        return localize;
      }
    ]
  )

  .filter(
    'i18n',
    [
      'localize',
      function (localize) {
        return function () {
          return localize.getLocalizedString.apply(null, arguments);
        };
      }
    ]
  )

  .directive(
    'i18n',
    [
      'localize',
      function(localize) {
        return {
          restrict: "EAC",
          link: function (scope, elm, attrs) {
            var str = attrs.i18n ? attrs.i18n : elm.html();

            if (localize.resourceFileLoaded) {
              localize.replace(elm, str);
            } else {
              deregister = scope.$on(
                'ngLocalizeUpdates',
                function() {
                  deregister();
                  localize.replace(elm, str);
                }
              );
            }
          }
        }
      }
    ]
  );
