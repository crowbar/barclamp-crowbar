# -*- encoding : utf-8 -*-
maintainer        "Opscode, Inc."
maintainer_email  "cookbooks@opscode.com"
license           "Apache 2.0"
description       "Installs and configures nscd"
version           "0.7"
suggests          "openldap"

recipe "nscd", "Installs and configures nscd"

%w{ redhat centos debian ubuntu }.each do |os|
  supports os
end
