;(function($, doc, win) {
  'use strict';

  function NodeList(el) {
    this.$root = $(el);

    this.dataBag = {
      removedOld: false,
      roleTarget: '.dropzone ul[data-id=\'{0}\']'
    };

    this.init();
  }

  NodeList.prototype.init = function() {
    this.errorTemplate = Handlebars.compile(
      $('#nodelist-alert').html()
    );

    this.warningTemplate = Handlebars.compile(
      $('#nodelist-warning').html()
    );

    this.itemTemplate = Handlebars.compile(
      $('#nodelist-item').html()
    );

    this.retrieveAvailable();
    this.retrieveConstraints();
    this.retrieveInput();
    this.retrieveAllocated();

    this.initDraggable();
    this.initDroppable();

    this.registerEvents();

    this.$root.find("ul, li").disableSelection();
  };

  NodeList.prototype.retrieveAvailable = function() {
    this.available = this.$root.find('.available[data-draggable=true]');
    this.handles = $.map(this.available, function(node, index) { return $(node).data('id'); });
  };

  NodeList.prototype.retrieveConstraints = function() {
    this.constraints = this.$root.data('constraints');
  };

  NodeList.prototype.retrieveInput = function() {
    this.input = $('#proposal_deployment');
    this.json = JSON.parse(this.input.val());
  };

  NodeList.prototype.retrieveAllocated = function() {
    var self = this;

    $.each(self.json.elements, function(role, nodes) {
      var $role = $(self.dataBag.roleTarget.format(role));
      var toRemove = [];

      $.each(nodes, function(index, node) {
        var source = self.$root.find(
          '.dragzone li[data-id=\'{0}\']'.format(node)
        );

        if ($.inArray(node, self.handles) >= 0) {
          self.insertNode(
            role,
            node,
            source.data('alias'),
            source.data('admin'),
            source.data('platform'),
            source.data('platform-version'),
            source.data('cluster'),
            true
          );
        } else {
          // We need to handle the case when the referenced node has been
          // removed by another proposal, and there is no corresponding element
          // in the node list. In that case, the alias is no longer available.
          if (source.length == 0) {
            $.event.trigger('nodeListNodeUnallocated', { role: role, id: node, alias: undefined });
          } else {
            $.event.trigger('nodeListNodeUnallocated', { role: role, id: source.data('id'), alias: source.data('alias') });
          }
          toRemove.push(index);
        }
      });

      if (toRemove.length > 0) {
        $.each(toRemove.reverse(), function(index, value) {
          self.json.elements[role].remove(value);
        });

        self.updateJson();
        self.removedNodes();
      }
    });
  };

  NodeList.prototype.initDraggable = function() {
    var self = this;

    this.$root.find("[data-draggable=true]").draggable({
      opacity: 0.7,
      helper: "clone",
      revert: "invalid"
    });
  };

  NodeList.prototype.initDroppable = function() {
    var self = this;

    this.$root.find("[data-droppable=true]").droppable({
      hoverClass: 'targeted',
      drop: function(event, ui) {
        var $role = $(event.target);
        var $node = $(ui.draggable.context);

        return self.insertNode(
          $role.data('id'),
          $node.data('id'),
          $node.data('alias'),
          $node.data('admin'),
          $node.data('platform'),
          $node.data('platform-version'),
          $node.data('cluster'),
          false
        );
      }
    });
  };

  NodeList.prototype.registerEvents = function() {
    var self = this;

    $('.dropzone .delete').live('click', function(event) {
      event.preventDefault();
      var $node = $(this).parent();

      var id = $node.data('id');
      var alias = $node.data('alias');
      var role = $node.data('role');

      if (self.json.elements[role]) {
        $.event.trigger('nodeListNodeUnallocated', { role: role, id: id, alias: alias });
        self.json.elements[role].removeValue(id);
      }

      self.updateJson();
      $node.remove();
    });

    $('.dropzone .unassign').live('click', function(event) {
      event.preventDefault();
      var role = $(this).data('id');

      var $role = $(
        'ul[data-droppable=true][data-id={0}]'.format(
          role
        )
      );

      var nodes = $role.find('[data-role={0}]'.format(role));

      $.each(nodes, function(index, node) {
        var $node = $(node);
        var id = $node.data('id');
        var alias = $node.data('alias');

        $.event.trigger('nodeListNodeUnallocated', { role: role, id: id, alias: alias });
      });

      $role.html('');
      self.json.elements[role] = [];
      self.updateJson();
    });
  };

  NodeList.prototype.updateJson = function() {
    var self = this;

    self.input.val(
      JSON.stringify(self.json)
    ).trigger('change');
  };

  NodeList.prototype.errorMessage = function(message) {
    var self = this;

    self.$root.before(
      self.errorTemplate({
        message: message
      })
    );

    var message = $('.alert-danger.alert-dismissable.disappear:last');

    win.setTimeout(
      function() {
        message.slideUp(500, function() {
          $(this).remove();
        });
      },
      3000
    );
  };

  NodeList.prototype.insertNode = function(role, id, alias, admin, platform, platform_version, cluster, initial) {
    var self = this;
    var $role = $(self.dataBag.roleTarget.format(role));

    if (self.constraints) {
      var constraints = self.constraints[role];
    } else {
      var constraints = {};
    }

    if (self.json.elements[role] == undefined) {
      self.json.elements[role] = [];
    }

    if (!initial) {
      if ($.inArray(id, self.json.elements[role]) >= 0) {
        if (cluster) {
          var key = 'barclamp.node_selector.cluster_duplicate';
        } else {
          var key = 'barclamp.node_selector.node_duplicate';
        }

        return self.errorMessage(
          key.localize().format(
            alias,
            role
          )
        );
      }
    }

    if (!initial && constraints) {
      if (constraints.admin == undefined || !constraints.admin) {
        if (admin) {
          return self.errorMessage(
            'barclamp.node_selector.no_admin'.localize().format(
              alias,
              role
            )
          );
        }
      }

      if (constraints.cluster == undefined || !constraints.cluster) {
        if (cluster) {
          return self.errorMessage(
            'barclamp.node_selector.no_cluster'.localize().format(
              alias,
              role
            )
          );
        }
      }

      if (constraints.conflicts_with !== undefined && constraints.conflicts_with) {
        var conflicts = $.grep(constraints.conflicts_with, function(conflicting_role) {
          return !!self.json.elements[conflicting_role] && ($.inArray(id, self.json.elements[conflicting_role]) >= 0);
        });

        if (conflicts.length > 0) {
          return self.errorMessage(
            'barclamp.node_selector.conflicting_roles'.localize().format(
              alias,
              role,
              constraints.conflicts_with.join(', ')
            )
          );
        }
      }

      if (constraints.unique !== undefined && constraints.unique)  {
        var inserted = $.unique(
          $.map(
            self.json.elements,
            function(nodes, role) {
              return nodes;
            }
          )
        );

        if ($.inArray(id, inserted) >= 0) {
          return self.errorMessage(
            'barclamp.node_selector.unique'.localize().format(
              alias,
              role
            )
          );
        }
      }

      if (constraints.count !== undefined && constraints.count >= 0) {
        switch (constraints.count) {
          case 0:
            return self.errorMessage(
              'barclamp.node_selector.zero'.localize().format(
                alias,
                role
              )
            );

            break;
          default:
            if (self.json.elements[role].length >= constraints.count) {
              return self.errorMessage(
                'barclamp.node_selector.max_count'.localize().format(
                  alias,
                  role
                )
              );
            }

            break;
        }
      }

      if (constraints.platform !== undefined && constraints.platform) {
        var platforms = $.map(constraints.platform, function(c_version, c_platform) {
          return (platform === c_platform) && self.equalOrMatchVersion(platform_version, c_version);
        });
        var is_any_true = platforms.some(function (element, index, array) {
          return element;
        });
        if (!is_any_true) {
          return self.errorMessage(
            'barclamp.node_selector.platform'.localize().format(
              alias,
              role
            )
          );
        }
      }

      if (constraints.exclude_platform !== undefined && constraints.exclude_platform) {
        var platforms = $.map(constraints.exclude_platform, function(c_version, c_platform) {
          return (platform === c_platform) && self.equalOrMatchVersion(platform_version, c_version);
        });
        var is_any_true = platforms.some(function (element, index, array) {
          return element;
        });
        if (is_any_true) {
          return self.errorMessage(
            'barclamp.node_selector.exclude_platform'.localize().format(
              alias,
              role
            )
          );
        }
      }
    }

    $role.append(
      self.itemTemplate({
        role: role,
        id: id,
        alias: alias,
        admin: admin,
        cluster: cluster
      })
    );

    $role.html(
      $role.children().sort(function(a, b) {
        return $(a).text().toUpperCase().localeCompare($(b).text().toUpperCase());
      })
    );

    if ($.inArray(id, self.json.elements[role]) < 0) {
      self.json.elements[role].push(id);
      self.updateJson();
    }

    $.event.trigger('nodeListNodeAllocated', { role: role, id: id, alias: alias });

    return true;
  };

  NodeList.prototype.removedNodes = function() {
    if (this.dataBag.removedOld) {
      return;
    }

    this.$root.before(
      this.warningTemplate({
        message: 'barclamp.node_selector.outdated'.localize()
      })
    );

    this.dataBag.removedOld = true;
  };

  NodeList.prototype.equalOrMatchVersion = function(value, maybe_regexp) {
    var to_version = function(version) {
      var match = /^(\d+)\.(\d+)\.(\d+)$/.exec(version);
      if (match) {
        return [parseInt(match[1]), parseInt(match[2]), parseInt(match[3])];
      }

      match = /^(\d+)\.(\d+)$/.exec(version);
      if (match) {
        return [parseInt(match[1]), parseInt(match[2]), 0];
      }

      match = /^(\d+)$/.exec(version);
      if (match) {
        return [parseInt(match[1]), 0, 0];
      }

      return [0, 0, 0];
    };

    var cmp = function(op, version1, version2) {
      var as_version1 = to_version(version1);
      var as_version2 = to_version(version2);
      var result = as_version1.map(function(v1, i) {
        switch (op) {
          case ">=":
            return v1 >= as_version2[i];
          case ">":
            return v1 > as_version2[i];
          case "<=":
            return v1 <= as_version2[i];
          case "<":
            return v1 < as_version2[i];
          case "==":
            return v1 == as_version2[i];
          default:
            return false;
        }
      });

      return result.every(function(element, index, array) {
        return element;
      });
    };

    // First group contains the RegExp without pre and post '/'
    var is_re = /^\/(.*)\/$/.exec(maybe_regexp);
    if (is_re) {
      var re = new RegExp(is_re[1]);
      return re.test(value)
    }

    var op_value = /^(>=?|<=?)\s*([.\d]+)$/.exec(maybe_regexp);
    if (op_value) {
      return cmp(op_value[1], value, op_value[2]);
    } else {
      return cmp('==', value, maybe_regexp);
    }
  };

  $.fn.nodeList = function() {
    return this.each(function() {
      new NodeList(this);
    });
  };
}(jQuery, document, window));
