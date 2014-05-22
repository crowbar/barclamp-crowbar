;(function($, doc, win) {
  'use strict';

  function DynamicTable(el, options) {
    this.root = $(el);
    this.json = {};

    this.options = $.extend(
      {
        disabledSubmits: 'input[source=commit1], input[source=save1]',
        storage: '#proposal_attributes',
        path: this.root.data('namespace'),
        entries: this.root.data('dynamic'),
        invalid: this.root.data('invalid'),
        duplicate: this.root.data('duplicate'),
        optional: this.root.data('optional'),
        key: 'name'
      },
      options
    );

    this.clone = (function() {
      return function(obj) {
        Clone.prototype = obj;
        return new Clone();
      };

      function Clone() {}
    }());

    this.initialize();
  }

  DynamicTable.prototype.initialize = function() {
    this.prepareTemplate();
    this.prepareJson();
    this.registerEvents();
    this.renderEntries();
  };

  DynamicTable.prototype.prepareTemplate = function() {
    this.entriesTemplate = Handlebars.compile(
      $(this.options.entries).html()
    );
  };

  DynamicTable.prototype.prepareJson = function() {
    this.json = JSON.parse(
      $(this.options.storage).val()
    );
  };

  DynamicTable.prototype.writeJson = function() {
    $(this.options.storage).val(
      JSON.stringify(this.json)
    );
  };

  DynamicTable.prototype.duplicateEntry = function() {
    if (this.options.key == false) {
      return false;
    }

    var input = this.root.find(
      'tfoot [data-name={0}]'.format(
        this.options.key
      )
    );

    var data = this.json;
    var path = this.options.path.split('/');

    $.event.trigger({
      type: 'dynamicTableBeforeDuplicate',
      input: input,
      json: data,
      path: path
    });

    while (path.length > 0) {
      var current_path = path.shift();

      if (!data[current_path]) {
        data[current_path] = {};
      }

      data = data[current_path];
    }

    if (data[$(input).val()]) {
      $.event.trigger({
        type: 'dynamicTableGotDuplicate',
        input: input,
        json: data,
        path: path
      });

      return true;
    } else {
      return false;
    }
  };

  DynamicTable.prototype.invalidEntry = function() {
    var optionals = this.options.optional.toString().split(',');
    var isInvalid = false;
    var invalidInput = false

    var inputs = this.root.find('tfoot input');

    $.event.trigger({
      type: 'dynamicTableBeforeInvalid',
      optionals: optionals,
      inputs: inputs
    });

    $.each(inputs, function(index, input) {
      if ($.inArray($(input).data('name'), optionals) < 0) {
        if ($(input).val() == '') {
          isInvalid = true;
          invalidInput = $(input).data('name');
        }
      }
    });

    if (isInvalid) {
      $.event.trigger({
        type: 'dynamicTableGotInvalid',
        optionals: optionals,
        inputs: inputs,
        invalid: invalidInput
      });

      return true;
    } else {
      return false;
    }
  };

  DynamicTable.prototype.registerEvents = function() {
    var self = this;

    self.root.find('[data-add]').on('click', function(event) {
      self.prepareJson();
      event.preventDefault();

      if (self.invalidEntry()) {
        self.root.after(
          $.dangerMessage(
            self.options.invalid,
            true,
            true
          )
        );

        return false;
      }

      if (self.duplicateEntry()) {
        self.root.after(
          $.dangerMessage(
            self.options.duplicate,
            true,
            true
          )
        );

        return false;
      }

      var inputs = self.root.find('tfoot input');

      var data = self.json;
      var path = self.options.path.split('/');
      var values = {};

      var currentKey = false;
      var currentValue = false;

      $.each(inputs, function(index, input) {
        var name = $(input).data('name');

        var value = self.parseValue(
          $(input).data('type'),
          $(input).val()
        );

        if (self.options.key == name) {
          currentKey = name;
          currentValue = value;
        } else {
          values[name] = value;
        }
      });

      while (path.length > 1) {
        var current_path = path.shift();

        if (!data[current_path]) {
          data[current_path] = {};
        }

        data = data[current_path];
      }

      if (currentKey) {
        data[path.shift()][currentValue] = values;
      } else {
        data[path.shift()] = values;
      }

      self.writeJson();

      $.event.trigger({
        type: 'dynamicTableAddedEntry',
        json: data,
        path: path,
        values: values,
        inputs: inputs
      });

      self.renderEntries();

      inputs.val('');
    });

    self.root.find('tfoot input').on('keydown', function(event) {
      if (event.keyCode == 13) {
        event.preventDefault();
        self.root.find('tfoot [data-add]').trigger('click');
      }
    });

    self.root.find('[data-remove]').on('click', function(event) {
      self.prepareJson();
      event.preventDefault();

      var data = self.json;
      var path = self.options.path.split('/');

      while (path.length > 1) {
        var current_path = path.shift();

        if (!data[current_path]) {
          data[current_path] = {};
        }

        data = data[current_path];
      }

      delete data[path.shift()][$(this).data('remove')];

      self.writeJson();

      $.event.trigger({
        type: 'dynamicTableRemovedEntry',
        json: data,
        path: path
      });

      self.renderEntries();
    });

    self.root.find('tbody input').on('change', function(event) {
      self.prepareJson();

      var data = self.json;
      var path = $(this).data('update').toString().split('/');

      var optionals = self.options.optional.toString().split(',');

      if ($.inArray($(this).data('name'), optionals) < 0) {
        if ($(this).val() == '') {
          $(this).parents('td').addClass('has-error');
        } else {
          $(this).parents('td').removeClass('has-error');
        }
      }

      if ($('tbody td.has-error').length > 0) {
        $(self.options.disabledSubmits).attr('disabled', 'disabled');
      } else {
        $(self.options.disabledSubmits).removeAttr('disabled');
      }

      while (path.length > 1) {
        var current_path = path.shift();

        if (!data[current_path]) {
          data[current_path] = {};
        }

        data = data[current_path];
      }

      data[path.shift()] = self.parseValue(
        $(this).data('type'),
        $(this).val()
      );

      self.writeJson();

      $.event.trigger({
        type: 'dynamicTableUpdatedEntry',
        json: data,
        path: path,
        optionals: optionals
      });
    });
  };

  DynamicTable.prototype.renderEntries = function() {
    var self = this;

    var data = self.json;
    var path = self.options.path.split('/');
    var entries = null;

    while (path.length > 1) {
      var current_path = path.shift();

      if (!data[current_path]) {
        data[current_path] = {};
      }

      data = data[current_path];
    }

    try {
      if (self.options.key != false) {
        entries = $.map(data[path.shift()], function(values, key) {
          return $.extend(
            self.clone(values),
            { name: key }
          );
        });
      } else {
        entries = data[path.shift()]
      }
    } catch(e) {
      entries = {};
    }

    self.root.find('tbody').html(
      this.entriesTemplate({
        entries: entries
      })
    );

    self.root.find('tbody input[type=password]').hideShowPassword();

    $.event.trigger({
      type: 'dynamicTableRenderedEntry',
      json: data,
      path: path,
      entries: entries
    });
  };

  DynamicTable.prototype.parseValue = function(type, value) {
    switch (type) {
      case 'string':
        return value.toString();
        break;
      case 'boolean':
        return value.toLowerCase() == 'true';
        break;
      case 'integer':
        return parseInt(value);
        break;
      case 'float':
        return parseFloat(value);
        break;
      case 'array-string':
        return this.splitValue(value);
        break;
      case 'array-boolean':
        value = this.splitValue(value);

        for (i in value) {
          value[i] = value[i].toLowerCase() == 'true'
        }

        return value;
        break;
      case 'array-integer':
        value = this.splitValue(value);

        for (i in value) {
          value[i] = parseInt(value[i]);
        }

        return value;
        break;
      case 'array-float':
        value = this.splitValue(value);

        for (i in value) {
          value[i] = parseFloat(value[i]);
        }

        return value;
        break;
    }
  };

  DynamicTable.prototype.splitValue = function(value) {
    return value.replace(/ /g, ',').replace(/,+/g, ',').split(',');
  };

  $.fn.dynamicTable = function(options) {
    return this.each(function() {
      new DynamicTable(this, options);
    });
  };
}(jQuery, document, window));
