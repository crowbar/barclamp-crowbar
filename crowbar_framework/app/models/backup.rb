#
# Copyright 2015, SUSE LINUX GmbH
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

class Backup
  include ActiveModel::Model

  attr_accessor :name, :created_at, :filename, :path

  validates_presence_of :name, :created_at
  validates :created_at, exclusion: { in: [nil, ""] }

  def initialize(options = {})
    @name = options.fetch :name, "crowbar_upgrade_data"
    @created_at = options.fetch :created_at, Time.zone.now.strftime("%Y%m%d-%H%M%S")
    @filename = "#{@name}-#{@created_at}.tar.gz"
    @path = Backup.image_dir.join(@filename)
  end

  def as_json(options: {})
    result = super
    result["path"] = path.to_s
    result
  end

  def save
    valid? && create
  end

  def delete
    @path.delete
  end

  def size
    path.size if path.file?
  end

  class << self
    def image_dir
      if Rails.env.production?
        Pathname.new("/var/lib/crowbar/backup")
      else
        Rails.root.join("storage")
      end
    end
  end

  protected

  def create
    saved = false
    dir = Dir.mktmpdir

    Crowbar::Backup::Export.new(dir).export
    Dir.chdir(dir) do
      system(
        "sudo tar czf #{path} *"
      )
      saved = true
    end
    FileUtils.rm_rf(dir)
    saved
  end
end
