# -*- encoding : utf-8 -*-
maintainer        "Opscode, Inc."
maintainer_email  "cookbooks@opscode.com"
license           "Apache 2.0"
description       "Helper library to determine whether distribution-only packages are installed"
version           "0.9.2"

recipe "packages", "Empty, this cookbook provides a library for helping determine whther distribution only packages should be installed"

%w{redhat centos}.each do |os|
  supports os
end

attribute "packages",
  :display_name => "Packages",
  :description => "Hash of Packages attributes",
  :type => "hash"

attribute "packages/dist_only",
  :display_name => "Packages Distribution Only?",
  :description => "Set to only use distribution-provided packages",
  :default => "false"

