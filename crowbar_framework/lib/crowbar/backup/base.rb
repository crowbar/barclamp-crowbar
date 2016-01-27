#
# Copyright 2011-2013, Dell
# Copyright 2013-2015, SUSE LINUX GmbH
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

require "logger"

module Crowbar
  class Backup
    class Base
      attr_accessor :logger

      def logger
        @logger ||= ::Logger.new(STDOUT).tap do |logger|
          logger.level = ::Logger::DEBUG
        end
      end

      class << self
        def restore_files
          [
            [
              "root/.gnupg",
              "/root/.gnupg"
            ],
            [
              "root/.ssh",
              "/root/.ssh"
            ],
            [
              "root/.chef",
              "/root/.chef"
            ],
            [
              "data",
              "/var/lib/crowbar"
            ],
            [
              "configs/hosts",
              "/etc/hosts"
            ],
            [
              "configs/hostname",
              "/etc/HOSTNAME"
            ],
            [
              "configs/resolv.conf.forwarders",
              "/etc/resolv.conf"
            ],
            [
              "keys/crowbar-client.pem",
              "/opt/dell/crowbar_framework/config/client.pem"
            ],
            [
              "keys/webui.pem",
              "/etc/chef/webui.pem"
            ],
            [
              "keys/chef-client.pem",
              "/etc/chef/client.pem"
            ],
            [
              "keys/crowbar.install.key",
              "/etc/crowbar.install.key"
            ],
            [
              "keys/chef-validation.pem",
              "/etc/chef/validation.pem"
            ],
            [
              "keys/tftp-validation.pem",
              "/srv/tftpboot/validation.pem"
            ],
            [
              "keys/cert.pem",
              "/etc/chef/certificates/cert.pem"
            ],
            [
              "keys/key.pem",
              "/etc/chef/certificates/key.pem"
            ]
          ]
        end

        def export_files
          restore_files.map(&:reverse)
        end
      end
    end
  end
end
