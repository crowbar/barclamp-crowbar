/**
 * Copyright 2011-2013, Dell
 * Copyright 2013, SUSE LINUX Products GmbH
 *
 * Licensed under the Apache License, Version 2.0 (the 'License');
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an 'AS IS' BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Author: Dell Crowbar Team
 * Author: SUSE LINUX Products GmbH
 */
;(function($, doc, win) {
  'use strict';

  function JsonAttribute(el) {
    this.el = $(el);
    this.json = {};

    this.readJson();
  }

  JsonAttribute.prototype.readJson = function() {
    this.json = JSON.parse(
      this.el.val()
    );
  };

  JsonAttribute.prototype.writeJson = function() {
    this.el.val(
      JSON.stringify(this.json)
    ).trigger('change');
  };

  JsonAttribute.prototype.write = function(key, value, type) {
    switch (type) {
      case 'boolean':
        value = value.toLowerCase() == 'true';
        break;
      case 'integer':
        value = parseInt(value);
        break;
      case 'float':
        value = parseFloat(value);
        break;
      case 'array-string':
        value = this.splitString(value);
        break;
      case 'array-boolean':
        var index;
        value = this.splitString(value);

        for (index in value) {
          value[index] = value[index].toLowerCase() == 'true'
        }
        break;
      case 'array-integer':
        var index;
        value = this.splitString(value);

        for (index in value) {
          value[index] = parseInt(value[index]);
        }
        break;
      case 'array-float':
        var index;
        value = this.splitString(value);

        for (index in value) {
          value[index] = parseFloat(value[index]);
        }
        break;
    }

    var data = this.json;
    var keys = key.split('/');

    while (keys.length > 1) {
      var part = keys.shift();

      if (!data[part]) {
        data[part] = {};
      }

      data = data[part];
    }

    data[keys.shift()] = value;
    this.writeJson();
  };

  JsonAttribute.prototype.read = function(key, value, type) {
    var data = this.json;
    var keys = key.split('/');

    try {
      while (keys.length >= 1) {
        var part = keys.shift();

        if (!data[part]) {
          data[part] = {};
        }

        data = data[part];
      }
    } catch(e) {
      return value;
    }

    if (data == undefined) {
      return value;
    } else {
      return data;
    }
  };

  JsonAttribute.prototype.remove = function(key, value, type) {
    var data = this.json;
    var keys = key.split('/');

    try {
      while (keys.length > 1) {
        var part = keys.shift();

        if (!data[part]) {
          data[part] = {};
        }

        data = data[part];
      }

      delete(data[keys.shift()]);
    } catch(e) {
      return false;
    }

    this.writeJson();
  };

  JsonAttribute.prototype.splitString = function(value) {
    return value.replace(/ /g, ',').replace(/,+/g, ',').split(',');
  };

  $.fn.readJsonAttribute = function(key, value, type) {
    var attribute = new JsonAttribute(this);
    return attribute.read(key, value, type)
  };

  $.fn.writeJsonAttribute = function(key, value, type) {
    var attribute = new JsonAttribute(this);
    return attribute.write(key, value, type)
  };

  $.fn.removeJsonAttribute = function(key, value, type) {
    var attribute = new JsonAttribute(this);
    return attribute.remove(key, value, type)
  };
}(jQuery, document, window));
