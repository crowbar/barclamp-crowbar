# -*- encoding : utf-8 -*-
#
# Copyright 2011-2013, Dell
# Copyright 2013-2014, SUSE LINUX Products GmbH
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

class DocsController < ApplicationController
  before_action :generate_books

  def index
    @index = YAML.load_file(
      docs_index
    )
  end

  def show
    @index = YAML.load_file(
      docs_index
    )

    @topic = @index

    params[:id].split("/").each do |segment|
      @parent = @topic
      @topic = @topic["topics"][segment]
    end

    if @topic["file"]
      @topic["file"] = Rails.root.join(@topic["file"])

      @markdown = Documentation::Generator.render(
        @topic["file"].readlines.join,
      )

      @topic.deep_merge! @markdown.metadata
    else
      @markdown = false
    end
  rescue
    flash[:alert] = I18n.t("docs.show.topic_missing")
    redirect_to docs_url
  end
  
  protected
  
  def docs_index
    Rails.root.join(
      "config", 
      "docs.yml"
    )
  end

  def docs_path
    Rails.root.join(
      "doc"
    )
  end

  def generate_books
    return if docs_index.file? and not Rails.env.development?

    @books = {}.tap do |books|
      meta_root = {
        "author" => "Multiple authors", 
        "license" => "Apache 2", 
        "copyright" => "#{Date.today.strftime("%Y")} by Dell, Inc", 
        "date" => I18n.t("unknown"), 
        "order" => "alpha", 
        "url" => "/", 
        "format" => "markdown" 
      }

      crowbar_path = docs_path.join("crowbar.yml")

      templates = [
        docs_path.children.delete(crowbar_path),
        docs_path.children
      ].flatten.compact

      templates.each do |book|
        next unless book.extname == ".yml"

        begin
          content = YAML.load_file(
            book
          )
        rescue
          next
        end
        
        next if content["root"].nil?

        topic = content["root"]
        meta_data = meta_root.deep_merge topic["topic_meta_data"]
        barclamp = book.basename(".yml").to_s.gsub("-", "_")
        children = topic.reject { |k, v| k == "topic_meta_data" }

        generate_topics books, meta_data, barclamp, [], children
      end
    end

    File.open(docs_index, "w") do |output|
      YAML.dump(@books, output)
    end
  end

  def generate_topics(books, meta_data, barclamp, parent, topics)
    return if topics.nil?

    topics.each do |id, details|
      next if id == "topic_meta_data"

      topic_meta_data = if details["topic_meta_data"].is_a? Hash
        meta_data.deep_merge details["topic_meta_data"]
      else
        meta_data
      end

      documentation = docs_path.join("default", barclamp, "#{topic_meta_data["file"] || id}.md")

      path = parent.dup
      path.push id

      if documentation.file?
        title = File.open(
          documentation, 
          "r"
        ).readline[/(#*)(.*)/, 2].strip rescue id.humanize

        order = if topic_meta_data["order"].nil?
          "%06d %s" % [
            9999,
            title
          ]
        else
          "%06d %s" % [
            topic_meta_data["order"].to_i,
            title
          ]
        end

        t = { 
          "id" => id,
          "title" => title,
          "file" => documentation.to_s.gsub("#{Rails.root}/", ""),
          "path" => path.join("/"),
          "sort" => order
        }
      else
        t = {
          "id" => id,
          "title" => "File is missing",
          "path" => path.join("/"),
          "sort" => "999999"
        } 
      end

      topic_meta_data.deep_merge! t
      reference = books

      path.each do |segment|
        reference["topics"] ||= {}
        reference["topics"][segment] ||= {}

        reference = reference["topics"][segment]
      end

      %w(id title path sort).each do |key|
        topic_meta_data.delete(key) if reference.keys.include? key
      end

      reference.deep_merge! topic_meta_data

      children = details.reject { |k, v| k == "topic_meta_data" }
      generate_topics books, meta_data, barclamp, path, children

      sorted = books["topics"].to_a.sort do |x, y| 
        x.last["sort"] <=> y.last["sort"]
      end

      books["topics"] = Hash[sorted]
    end
  end
end
