(function() {
  var NodeTypes, ParameterMissing, Utils, defaults,
    __hasProp = {}.hasOwnProperty;

  ParameterMissing = function(message) {
    this.message = message;
  };

  ParameterMissing.prototype = new Error();

  defaults = {
    prefix: "",
    default_url_options: {}
  };

  NodeTypes = {"GROUP":1,"CAT":2,"SYMBOL":3,"OR":4,"STAR":5,"LITERAL":6,"SLASH":7,"DOT":8};

  Utils = {
    serialize: function(object, prefix) {
      var element, i, key, prop, result, s, _i, _len;

      if (prefix == null) {
        prefix = null;
      }
      if (!object) {
        return "";
      }
      if (!prefix && !(this.get_object_type(object) === "object")) {
        throw new Error("Url parameters should be a javascript hash");
      }
      if (window.jQuery) {
        result = window.jQuery.param(object);
        return (!result ? "" : result);
      }
      s = [];
      switch (this.get_object_type(object)) {
        case "array":
          for (i = _i = 0, _len = object.length; _i < _len; i = ++_i) {
            element = object[i];
            s.push(this.serialize(element, prefix + "[]"));
          }
          break;
        case "object":
          for (key in object) {
            if (!__hasProp.call(object, key)) continue;
            prop = object[key];
            if (!(prop != null)) {
              continue;
            }
            if (prefix != null) {
              key = "" + prefix + "[" + key + "]";
            }
            s.push(this.serialize(prop, key));
          }
          break;
        default:
          if (object) {
            s.push("" + (encodeURIComponent(prefix.toString())) + "=" + (encodeURIComponent(object.toString())));
          }
      }
      if (!s.length) {
        return "";
      }
      return s.join("&");
    },
    clean_path: function(path) {
      var last_index;

      path = path.split("://");
      last_index = path.length - 1;
      path[last_index] = path[last_index].replace(/\/+/g, "/");
      return path.join("://");
    },
    set_default_url_options: function(optional_parts, options) {
      var i, part, _i, _len, _results;

      _results = [];
      for (i = _i = 0, _len = optional_parts.length; _i < _len; i = ++_i) {
        part = optional_parts[i];
        if (!options.hasOwnProperty(part) && defaults.default_url_options.hasOwnProperty(part)) {
          _results.push(options[part] = defaults.default_url_options[part]);
        }
      }
      return _results;
    },
    extract_anchor: function(options) {
      var anchor;

      anchor = "";
      if (options.hasOwnProperty("anchor")) {
        anchor = "#" + options.anchor;
        delete options.anchor;
      }
      return anchor;
    },
    extract_options: function(number_of_params, args) {
      var last_el;

      last_el = args[args.length - 1];
      if (args.length > number_of_params || ((last_el != null) && "object" === this.get_object_type(last_el) && !this.look_like_serialized_model(last_el))) {
        return args.pop();
      } else {
        return {};
      }
    },
    look_like_serialized_model: function(object) {
      return "id" in object || "to_param" in object;
    },
    path_identifier: function(object) {
      var property;

      if (object === 0) {
        return "0";
      }
      if (!object) {
        return "";
      }
      property = object;
      if (this.get_object_type(object) === "object") {
        if ("to_param" in object) {
          property = object.to_param;
        } else if ("id" in object) {
          property = object.id;
        } else {
          property = object;
        }
        if (this.get_object_type(property) === "function") {
          property = property.call(object);
        }
      }
      return property.toString();
    },
    clone: function(obj) {
      var attr, copy, key;

      if ((obj == null) || "object" !== this.get_object_type(obj)) {
        return obj;
      }
      copy = obj.constructor();
      for (key in obj) {
        if (!__hasProp.call(obj, key)) continue;
        attr = obj[key];
        copy[key] = attr;
      }
      return copy;
    },
    prepare_parameters: function(required_parameters, actual_parameters, options) {
      var i, result, val, _i, _len;

      result = this.clone(options) || {};
      for (i = _i = 0, _len = required_parameters.length; _i < _len; i = ++_i) {
        val = required_parameters[i];
        if (i < actual_parameters.length) {
          result[val] = actual_parameters[i];
        }
      }
      return result;
    },
    build_path: function(required_parameters, optional_parts, route, args) {
      var anchor, opts, parameters, result, url, url_params;

      args = Array.prototype.slice.call(args);
      opts = this.extract_options(required_parameters.length, args);
      if (args.length > required_parameters.length) {
        throw new Error("Too many parameters provided for path");
      }
      parameters = this.prepare_parameters(required_parameters, args, opts);
      this.set_default_url_options(optional_parts, parameters);
      anchor = this.extract_anchor(parameters);
      result = "" + (this.get_prefix()) + (this.visit(route, parameters));
      url = Utils.clean_path("" + result);
      if ((url_params = this.serialize(parameters)).length) {
        url += "?" + url_params;
      }
      url += anchor;
      return url;
    },
    visit: function(route, parameters, optional) {
      var left, left_part, right, right_part, type, value;

      if (optional == null) {
        optional = false;
      }
      type = route[0], left = route[1], right = route[2];
      switch (type) {
        case NodeTypes.GROUP:
          return this.visit(left, parameters, true);
        case NodeTypes.STAR:
          return this.visit_globbing(left, parameters, true);
        case NodeTypes.LITERAL:
        case NodeTypes.SLASH:
        case NodeTypes.DOT:
          return left;
        case NodeTypes.CAT:
          left_part = this.visit(left, parameters, optional);
          right_part = this.visit(right, parameters, optional);
          if (optional && !(left_part && right_part)) {
            return "";
          }
          return "" + left_part + right_part;
        case NodeTypes.SYMBOL:
          value = parameters[left];
          if (value != null) {
            delete parameters[left];
            return this.path_identifier(value);
          }
          if (optional) {
            return "";
          } else {
            throw new ParameterMissing("Route parameter missing: " + left);
          }
          break;
        default:
          throw new Error("Unknown Rails node type");
      }
    },
    visit_globbing: function(route, parameters, optional) {
      var left, right, type, value;

      type = route[0], left = route[1], right = route[2];
      if (left.replace(/^\*/i, "") !== left) {
        route[1] = left = left.replace(/^\*/i, "");
      }
      value = parameters[left];
      if (value == null) {
        return this.visit(route, parameters, optional);
      }
      parameters[left] = (function() {
        switch (this.get_object_type(value)) {
          case "array":
            return value.join("/");
          default:
            return value;
        }
      }).call(this);
      return this.visit(route, parameters, optional);
    },
    get_prefix: function() {
      var prefix;

      prefix = defaults.prefix;
      if (prefix !== "") {
        prefix = (prefix.match("/$") ? prefix : "" + prefix + "/");
      }
      return prefix;
    },
    _classToTypeCache: null,
    _classToType: function() {
      var name, _i, _len, _ref;

      if (this._classToTypeCache != null) {
        return this._classToTypeCache;
      }
      this._classToTypeCache = {};
      _ref = "Boolean Number String Function Array Date RegExp Object Error".split(" ");
      for (_i = 0, _len = _ref.length; _i < _len; _i++) {
        name = _ref[_i];
        this._classToTypeCache["[object " + name + "]"] = name.toLowerCase();
      }
      return this._classToTypeCache;
    },
    get_object_type: function(obj) {
      if (window.jQuery && (window.jQuery.type != null)) {
        return window.jQuery.type(obj);
      }
      if (obj == null) {
        return "" + obj;
      }
      if (typeof obj === "object" || typeof obj === "function") {
        return this._classToType()[Object.prototype.toString.call(obj)] || "object";
      } else {
        return typeof obj;
      }
    },
    namespace: function(root, namespaceString) {
      var current, parts;

      parts = (namespaceString ? namespaceString.split(".") : []);
      if (!parts.length) {
        return;
      }
      current = parts.shift();
      root[current] = root[current] || {};
      return Utils.namespace(root[current], parts.join("."));
    }
  };

  Utils.namespace(window, "Routes");

  window.Routes = {
// attribute_node => /nodes/:id/attribute/*path(.:format)
  attribute_node_path: function(_id, _path, options) {
  return Utils.build_path(["id","path"], ["format"], [2,[2,[2,[2,[2,[2,[2,[2,[7,"/",false],[6,"nodes",false]],[7,"/",false]],[3,"id",false]],[7,"/",false]],[6,"attribute",false]],[7,"/",false]],[5,[3,"*path",false],false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// available_barclamps => /crowbar/modules/1.0(.:format)
  available_barclamps_path: function(options) {
  return Utils.build_path([], ["format"], [2,[2,[2,[2,[2,[2,[2,[2,[7,"/",false],[6,"crowbar",false]],[7,"/",false]],[6,"modules",false]],[7,"/",false]],[6,"1",false]],[8,".",false]],[6,"0",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// bulk_nodes => /nodes/bulk(.:format)
  bulk_nodes_path: function(options) {
  return Utils.build_path([], ["format"], [2,[2,[2,[2,[7,"/",false],[6,"nodes",false]],[7,"/",false]],[6,"bulk",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// clear_swift_reports => /swift/reports/clear(.:format)
  clear_swift_reports_path: function(options) {
  return Utils.build_path([], ["format"], [2,[2,[2,[2,[2,[2,[7,"/",false],[6,"swift",false]],[7,"/",false]],[6,"reports",false]],[7,"/",false]],[6,"clear",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// clear_tempest_reports => /tempest/reports/clear(.:format)
  clear_tempest_reports_path: function(options) {
  return Utils.build_path([], ["format"], [2,[2,[2,[2,[2,[2,[7,"/",false],[6,"tempest",false]],[7,"/",false]],[6,"reports",false]],[7,"/",false]],[6,"clear",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// commit_proposal => /crowbar/:controller/1.0/proposals/commit/:id(.:format)
  commit_proposal_path: function(_controller, _id, options) {
  return Utils.build_path(["controller","id"], ["format"], [2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[7,"/",false],[6,"crowbar",false]],[7,"/",false]],[3,"controller",false]],[7,"/",false]],[6,"1",false]],[8,".",false]],[6,"0",false]],[7,"/",false]],[6,"proposals",false]],[7,"/",false]],[6,"commit",false]],[7,"/",false]],[3,"id",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// create_proposal => /crowbar/:controller/1.0/proposals(.:format)
  create_proposal_path: function(_controller, options) {
  return Utils.build_path(["controller"], ["format"], [2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[7,"/",false],[6,"crowbar",false]],[7,"/",false]],[3,"controller",false]],[7,"/",false]],[6,"1",false]],[8,".",false]],[6,"0",false]],[7,"/",false]],[6,"proposals",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// dashboard => /dashboard(.:format)
  dashboard_path: function(options) {
  return Utils.build_path([], ["format"], [2,[2,[7,"/",false],[6,"dashboard",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// deactivate_proposal => /crowbar/:controller/1.0/proposals/deactivate/:id(.:format)
  deactivate_proposal_path: function(_controller, _id, options) {
  return Utils.build_path(["controller","id"], ["format"], [2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[7,"/",false],[6,"crowbar",false]],[7,"/",false]],[3,"controller",false]],[7,"/",false]],[6,"1",false]],[8,".",false]],[6,"0",false]],[7,"/",false]],[6,"proposals",false]],[7,"/",false]],[6,"deactivate",false]],[7,"/",false]],[3,"id",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// delete_proposal => /crowbar/:controller/1.0/proposals/delete/:id(.:format)
  delete_proposal_path: function(_controller, _id, options) {
  return Utils.build_path(["controller","id"], ["format"], [2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[7,"/",false],[6,"crowbar",false]],[7,"/",false]],[3,"controller",false]],[7,"/",false]],[6,"1",false]],[8,".",false]],[6,"0",false]],[7,"/",false]],[6,"proposals",false]],[7,"/",false]],[6,"delete",false]],[7,"/",false]],[3,"id",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// dequeue_proposal => /crowbar/:controller/1.0/proposals/dequeue/:id(.:format)
  dequeue_proposal_path: function(_controller, _id, options) {
  return Utils.build_path(["controller","id"], ["format"], [2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[7,"/",false],[6,"crowbar",false]],[7,"/",false]],[3,"controller",false]],[7,"/",false]],[6,"1",false]],[8,".",false]],[6,"0",false]],[7,"/",false]],[6,"proposals",false]],[7,"/",false]],[6,"dequeue",false]],[7,"/",false]],[3,"id",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// docs => /docs(.:format)
  docs_path: function(options) {
  return Utils.build_path([], ["format"], [2,[2,[7,"/",false],[6,"docs",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// docs_barclamp => /docs/:controller/1.0(.:format)
  docs_barclamp_path: function(_controller, options) {
  return Utils.build_path(["controller"], ["format"], [2,[2,[2,[2,[2,[2,[2,[2,[7,"/",false],[6,"docs",false]],[7,"/",false]],[3,"controller",false]],[7,"/",false]],[6,"1",false]],[8,".",false]],[6,"0",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// edit_node => /nodes/:id/edit(.:format)
  edit_node_path: function(_id, options) {
  return Utils.build_path(["id"], ["format"], [2,[2,[2,[2,[2,[2,[7,"/",false],[6,"nodes",false]],[7,"/",false]],[3,"id",false]],[7,"/",false]],[6,"edit",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// edit_proposal => /crowbar/:controller/1.0/proposals/:id(.:format)
  edit_proposal_path: function(_controller, _id, options) {
  return Utils.build_path(["controller","id"], ["format"], [2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[7,"/",false],[6,"crowbar",false]],[7,"/",false]],[3,"controller",false]],[7,"/",false]],[6,"1",false]],[8,".",false]],[6,"0",false]],[7,"/",false]],[6,"proposals",false]],[7,"/",false]],[3,"id",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// edit_ucs => /ucs/edit(.:format)
  edit_ucs_path: function(options) {
  return Utils.build_path([], ["format"], [2,[2,[2,[2,[7,"/",false],[6,"ucs",false]],[7,"/",false]],[6,"edit",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// elements => /crowbar/:controller/1.0/elements(.:format)
  elements_path: function(_controller, options) {
  return Utils.build_path(["controller"], ["format"], [2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[7,"/",false],[6,"crowbar",false]],[7,"/",false]],[3,"controller",false]],[7,"/",false]],[6,"1",false]],[8,".",false]],[6,"0",false]],[7,"/",false]],[6,"elements",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// export_barclamp => /export/:controller/1.0(.:format)
  export_barclamp_path: function(_controller, options) {
  return Utils.build_path(["controller"], ["format"], [2,[2,[2,[2,[2,[2,[2,[2,[7,"/",false],[6,"export",false]],[7,"/",false]],[3,"controller",false]],[7,"/",false]],[6,"1",false]],[8,".",false]],[6,"0",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// families_nodes => /nodes/families(.:format)
  families_nodes_path: function(options) {
  return Utils.build_path([], ["format"], [2,[2,[2,[2,[7,"/",false],[6,"nodes",false]],[7,"/",false]],[6,"families",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// file_utils => /utils/:file(.:format)
  file_utils_path: function(_file, options) {
  return Utils.build_path(["file"], ["format"], [2,[2,[2,[2,[7,"/",false],[6,"utils",false]],[7,"/",false]],[3,"file",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// group_node => /nodes/:id/group(.:format)
  group_node_path: function(_id, options) {
  return Utils.build_path(["id"], ["format"], [2,[2,[2,[2,[2,[2,[7,"/",false],[6,"nodes",false]],[7,"/",false]],[3,"id",false]],[7,"/",false]],[6,"group",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// grouped_barclamps => /crowbar/:controller/1.0(.:format)
  grouped_barclamps_path: function(_controller, options) {
  return Utils.build_path(["controller"], ["format"], [2,[2,[2,[2,[2,[2,[2,[2,[7,"/",false],[6,"crowbar",false]],[7,"/",false]],[3,"controller",false]],[7,"/",false]],[6,"1",false]],[8,".",false]],[6,"0",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// help => /crowbar/:barclamp/1.0/help(.:format)
  help_path: function(_barclamp, options) {
  return Utils.build_path(["barclamp"], ["format"], [2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[7,"/",false],[6,"crowbar",false]],[7,"/",false]],[3,"barclamp",false]],[7,"/",false]],[6,"1",false]],[8,".",false]],[6,"0",false]],[7,"/",false]],[6,"help",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// help_barclamp => /crowbar/:controller/1.0/help(.:format)
  help_barclamp_path: function(_controller, options) {
  return Utils.build_path(["controller"], ["format"], [2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[7,"/",false],[6,"crowbar",false]],[7,"/",false]],[3,"controller",false]],[7,"/",false]],[6,"1",false]],[8,".",false]],[6,"0",false]],[7,"/",false]],[6,"help",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// hit_node => /nodes/:id/hit/:req(.:format)
  hit_node_path: function(_id, _req, options) {
  return Utils.build_path(["id","req"], ["format"], [2,[2,[2,[2,[2,[2,[2,[2,[7,"/",false],[6,"nodes",false]],[7,"/",false]],[3,"id",false]],[7,"/",false]],[6,"hit",false]],[7,"/",false]],[3,"req",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// list_nodes => /nodes/list(.:format)
  list_nodes_path: function(options) {
  return Utils.build_path([], ["format"], [2,[2,[2,[2,[7,"/",false],[6,"nodes",false]],[7,"/",false]],[6,"list",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// login_ucs => /ucs/login(.:format)
  login_ucs_path: function(options) {
  return Utils.build_path([], ["format"], [2,[2,[2,[2,[7,"/",false],[6,"ucs",false]],[7,"/",false]],[6,"login",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// logout_ucs => /ucs/logout(.:format)
  logout_ucs_path: function(options) {
  return Utils.build_path([], ["format"], [2,[2,[2,[2,[7,"/",false],[6,"ucs",false]],[7,"/",false]],[6,"logout",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// network_barclamp => /network/:controller/1.0(.:format)
  network_barclamp_path: function(_controller, options) {
  return Utils.build_path(["controller"], ["format"], [2,[2,[2,[2,[2,[2,[2,[2,[7,"/",false],[6,"network",false]],[7,"/",false]],[3,"controller",false]],[7,"/",false]],[6,"1",false]],[8,".",false]],[6,"0",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// node => /nodes/:id(.:format)
  node_path: function(_id, options) {
  return Utils.build_path(["id"], ["format"], [2,[2,[2,[2,[7,"/",false],[6,"nodes",false]],[7,"/",false]],[3,"id",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// nodes_barclamp => /nodes/:controller/1.0(.:format)
  nodes_barclamp_path: function(_controller, options) {
  return Utils.build_path(["controller"], ["format"], [2,[2,[2,[2,[2,[2,[2,[2,[7,"/",false],[6,"nodes",false]],[7,"/",false]],[3,"controller",false]],[7,"/",false]],[6,"1",false]],[8,".",false]],[6,"0",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// on_barclamp => /crowbar/:controller/1.0/:action/:id(.:format)
  on_barclamp_path: function(_controller, _action, _id, options) {
  return Utils.build_path(["controller","action","id"], ["format"], [2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[7,"/",false],[6,"crowbar",false]],[7,"/",false]],[3,"controller",false]],[7,"/",false]],[6,"1",false]],[8,".",false]],[6,"0",false]],[7,"/",false]],[3,"action",false]],[7,"/",false]],[3,"id",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// proposals => /crowbar/:controller/1.0/proposals(.:format)
  proposals_path: function(_controller, options) {
  return Utils.build_path(["controller"], ["format"], [2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[7,"/",false],[6,"crowbar",false]],[7,"/",false]],[3,"controller",false]],[7,"/",false]],[6,"1",false]],[8,".",false]],[6,"0",false]],[7,"/",false]],[6,"proposals",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// rails_info => /rails/info(.:format)
  rails_info_path: function(options) {
  return Utils.build_path([], ["format"], [2,[2,[2,[2,[7,"/",false],[6,"rails",false]],[7,"/",false]],[6,"info",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// rails_info_properties => /rails/info/properties(.:format)
  rails_info_properties_path: function(options) {
  return Utils.build_path([], ["format"], [2,[2,[2,[2,[2,[2,[7,"/",false],[6,"rails",false]],[7,"/",false]],[6,"info",false]],[7,"/",false]],[6,"properties",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// rails_info_routes => /rails/info/routes(.:format)
  rails_info_routes_path: function(options) {
  return Utils.build_path([], ["format"], [2,[2,[2,[2,[2,[2,[7,"/",false],[6,"rails",false]],[7,"/",false]],[6,"info",false]],[7,"/",false]],[6,"routes",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// rails_mailers => /rails/mailers(.:format)
  rails_mailers_path: function(options) {
  return Utils.build_path([], ["format"], [2,[2,[2,[2,[7,"/",false],[6,"rails",false]],[7,"/",false]],[6,"mailers",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// root => /
  root_path: function(options) {
  return Utils.build_path([], [], [7,"/",false], arguments);
  },
// settings_ucs => /ucs/settings(.:format)
  settings_ucs_path: function(options) {
  return Utils.build_path([], ["format"], [2,[2,[2,[2,[7,"/",false],[6,"ucs",false]],[7,"/",false]],[6,"settings",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// show_element => /crowbar/:controller/1.0/elements/:id(.:format)
  show_element_path: function(_controller, _id, options) {
  return Utils.build_path(["controller","id"], ["format"], [2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[7,"/",false],[6,"crowbar",false]],[7,"/",false]],[3,"controller",false]],[7,"/",false]],[6,"1",false]],[8,".",false]],[6,"0",false]],[7,"/",false]],[6,"elements",false]],[7,"/",false]],[3,"id",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// show_proposal => /crowbar/:controller/1.0/proposals/show/:id(.:format)
  show_proposal_path: function(_controller, _id, options) {
  return Utils.build_path(["controller","id"], ["format"], [2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[7,"/",false],[6,"crowbar",false]],[7,"/",false]],[3,"controller",false]],[7,"/",false]],[6,"1",false]],[8,".",false]],[6,"0",false]],[7,"/",false]],[6,"proposals",false]],[7,"/",false]],[6,"show",false]],[7,"/",false]],[3,"id",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// status => /crowbar/:barclamp/1.0/status(.:format)
  status_path: function(_barclamp, options) {
  return Utils.build_path(["barclamp"], ["format"], [2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[7,"/",false],[6,"crowbar",false]],[7,"/",false]],[3,"barclamp",false]],[7,"/",false]],[6,"1",false]],[8,".",false]],[6,"0",false]],[7,"/",false]],[6,"status",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// status_barclamp => /crowbar/:controller/1.0/status(.:format)
  status_barclamp_path: function(_controller, options) {
  return Utils.build_path(["controller"], ["format"], [2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[7,"/",false],[6,"crowbar",false]],[7,"/",false]],[3,"controller",false]],[7,"/",false]],[6,"1",false]],[8,".",false]],[6,"0",false]],[7,"/",false]],[6,"status",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// status_node => /nodes/:id/status(.:format)
  status_node_path: function(_id, options) {
  return Utils.build_path(["id"], ["format"], [2,[2,[2,[2,[2,[2,[7,"/",false],[6,"nodes",false]],[7,"/",false]],[3,"id",false]],[7,"/",false]],[6,"status",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// status_nodes => /nodes/status(.:format)
  status_nodes_path: function(options) {
  return Utils.build_path([], ["format"], [2,[2,[2,[2,[7,"/",false],[6,"nodes",false]],[7,"/",false]],[6,"status",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// status_proposal => /crowbar/:controller/1.0/proposals/status/:id(.:format)
  status_proposal_path: function(_controller, _id, options) {
  return Utils.build_path(["controller","id"], ["format"], [2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[7,"/",false],[6,"crowbar",false]],[7,"/",false]],[3,"controller",false]],[7,"/",false]],[6,"1",false]],[8,".",false]],[6,"0",false]],[7,"/",false]],[6,"proposals",false]],[7,"/",false]],[6,"status",false]],[7,"/",false]],[3,"id",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// swift_report => /swift/reports/:id(.:format)
  swift_report_path: function(_id, options) {
  return Utils.build_path(["id"], ["format"], [2,[2,[2,[2,[2,[2,[7,"/",false],[6,"swift",false]],[7,"/",false]],[6,"reports",false]],[7,"/",false]],[3,"id",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// swift_reports => /swift/reports(.:format)
  swift_reports_path: function(options) {
  return Utils.build_path([], ["format"], [2,[2,[2,[2,[7,"/",false],[6,"swift",false]],[7,"/",false]],[6,"reports",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// switch_network => /networks/:id/switch(.:format)
  switch_network_path: function(_id, options) {
  return Utils.build_path(["id"], ["format"], [2,[2,[2,[2,[2,[2,[7,"/",false],[6,"networks",false]],[7,"/",false]],[3,"id",false]],[7,"/",false]],[6,"switch",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// tempest_report => /tempest/reports/:id(.:format)
  tempest_report_path: function(_id, options) {
  return Utils.build_path(["id"], ["format"], [2,[2,[2,[2,[2,[2,[7,"/",false],[6,"tempest",false]],[7,"/",false]],[6,"reports",false]],[7,"/",false]],[3,"id",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// tempest_reports => /tempest/reports(.:format)
  tempest_reports_path: function(options) {
  return Utils.build_path([], ["format"], [2,[2,[2,[2,[7,"/",false],[6,"tempest",false]],[7,"/",false]],[6,"reports",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// topic_docs => /docs/*id(.:format)
  topic_docs_path: function(_id, options) {
  return Utils.build_path(["id"], ["format"], [2,[2,[2,[2,[7,"/",false],[6,"docs",false]],[7,"/",false]],[5,[3,"*id",false],false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// transition_barclamp => /crowbar/:controller/1.0/transition/:id(.:format)
  transition_barclamp_path: function(_controller, _id, options) {
  return Utils.build_path(["controller","id"], ["format"], [2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[7,"/",false],[6,"crowbar",false]],[7,"/",false]],[3,"controller",false]],[7,"/",false]],[6,"1",false]],[8,".",false]],[6,"0",false]],[7,"/",false]],[6,"transition",false]],[7,"/",false]],[3,"id",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// translations => /translations(.:format)
  translations_path: function(options) {
  return Utils.build_path([], ["format"], [2,[2,[7,"/",false],[6,"translations",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// unallocated_nodes => /nodes/unallocated(.:format)
  unallocated_nodes_path: function(options) {
  return Utils.build_path([], ["format"], [2,[2,[2,[2,[7,"/",false],[6,"nodes",false]],[7,"/",false]],[6,"unallocated",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// update_proposal => /crowbar/:controller/1.0/proposals/:id(.:format)
  update_proposal_path: function(_controller, _id, options) {
  return Utils.build_path(["controller","id"], ["format"], [2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[2,[7,"/",false],[6,"crowbar",false]],[7,"/",false]],[3,"controller",false]],[7,"/",false]],[6,"1",false]],[8,".",false]],[6,"0",false]],[7,"/",false]],[6,"proposals",false]],[7,"/",false]],[3,"id",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// update_ucs => /ucs(.:format)
  update_ucs_path: function(options) {
  return Utils.build_path([], ["format"], [2,[2,[7,"/",false],[6,"ucs",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// util => /utils/:id(.:format)
  util_path: function(_id, options) {
  return Utils.build_path(["id"], ["format"], [2,[2,[2,[2,[7,"/",false],[6,"utils",false]],[7,"/",false]],[3,"id",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// utils => /utils(.:format)
  utils_path: function(options) {
  return Utils.build_path([], ["format"], [2,[2,[7,"/",false],[6,"utils",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// utils_barclamp => /utils/:controller/1.0(.:format)
  utils_barclamp_path: function(_controller, options) {
  return Utils.build_path(["controller"], ["format"], [2,[2,[2,[2,[2,[2,[2,[2,[7,"/",false],[6,"utils",false]],[7,"/",false]],[3,"controller",false]],[7,"/",false]],[6,"1",false]],[8,".",false]],[6,"0",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  },
// vlan_network => /networks/:id/vlan(.:format)
  vlan_network_path: function(_id, options) {
  return Utils.build_path(["id"], ["format"], [2,[2,[2,[2,[2,[2,[7,"/",false],[6,"networks",false]],[7,"/",false]],[3,"id",false]],[7,"/",false]],[6,"vlan",false]],[1,[2,[8,".",false],[3,"format",false]],false]], arguments);
  }}
;

  window.Routes.options = defaults;

}).call(this);
