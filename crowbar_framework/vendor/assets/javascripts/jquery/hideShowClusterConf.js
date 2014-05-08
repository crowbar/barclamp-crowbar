;(function($, doc, win) {
  'use strict';

  function HideShowClusterConf(el, options) {
    this.root = $(el);
    this.clusters_allocated = 0;
    this.options = $.extend(
      {
        deployment_storage: '#proposal_deployment',
        deployment_path: 'elements/{0}'.format(this.root.data('elements-path'))
      },
      options
    );

    this.initialize();
  }

  // HUGE FIXME: refactor this into a pacemaker JS plugin that exposes an API
  // for working with clusters.
  HideShowClusterConf.prototype.isCluster = function(node_id) {
     return !!node_id.match(/:/);
  };

  HideShowClusterConf.prototype.initialize = function() {
    var self = this;

    var clusters = $.grep($(this.options.deployment_storage).readJsonAttribute(this.options.deployment_path), this.isCluster);
    this.clusters_allocated = clusters.length;

    if (this.clusters_allocated == 0) { this.root.hide(); }

    $(document).on('nodeListNodeAllocated', function(evt, data) {
      if (self.isCluster(data.id)) {
        if (self.clusters_allocated == 0) { self.root.show(); }
        self.clusters_allocated += 1;
      }
    });

    $(document).on('nodeListNodeUnallocated', function(evt, data) {
      if (self.isCluster(data.id)) {
        self.clusters_allocated -= 1;
        if (self.clusters_allocated == 0) { self.root.hide(); }
      }
    });
  }

  $.fn.hideShowClusterConf = function(options) {
    return this.each(function() {
      new HideShowClusterConf(this, options);
    });
  }
}(jQuery, document, window));
