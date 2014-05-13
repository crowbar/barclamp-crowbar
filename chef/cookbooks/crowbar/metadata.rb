name "crowbar"
maintainer "Dell, Inc."
maintainer_email "crowbar@dell.com"
license "Apache 2.0"
description "Installs crowbar"
long_description IO.read(File.join(File.dirname(__FILE__), "README.md"))
version "0.9.5"

depends "apache2"
depends "ruby"
depends "rails"
depends "bluepill"

supports "debian"
supports "ubuntu"
supports "redhat"
supports "centos"
supports "suse"
