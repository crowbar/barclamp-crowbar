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

module Documentation
  module Generator
    Data = Struct.new(:metadata, :output)

    def render(text, renderer = nil)
      return redcarpet(text) if renderer.nil?

      text =~ /^(---\s*\n.*?\n?)^(---\s*$\n?)/m

      metadata = YAML.load(
        Regexp.last_match(0)
      ) if Regexp.last_match(0)

      Data.new(
        metadata, 
        renderer.render(Regexp.last_match(0) ? $' : text)
      )
    end

    module_function :render

    def redcarpet(text)
      renderer = Renderer.new

      markdown = Redcarpet::Markdown.new(
        renderer, with_toc_data: true
      )

      Data.new(
        renderer.metadata, 
        markdown.render(text)
      )
    end

    module_function :redcarpet
  end

  class Renderer < Redcarpet::Render::HTML
    include ActionView::Helpers::TagHelper

    def preprocess(document)
      document =~ /^(---\s*\n.*?\n?)^(---\s*$\n?)/m

      @metadata = YAML.load(
        Regexp.last_match(0)
      ) if Regexp.last_match(0)

      Regexp.last_match(0) ? $' : document
    end

    def metadata
      @metadata || {}
    end

    def header(text, header_level, anchor)
      content = content_tag(
        ["h", header_level + 1].join,
        content_tag(
          :a,
          text.html_safe,
          href: ["#", anchor].join,
          id: anchor
        )
      )

      if [1, 2].include? header_level
        content_tag(
          :div,
          content,
          class: "panel-heading"
        )
      else
        content
      end
    end

    def postprocess(document)
      doc = ::Nokogiri::HTML::Document.new
      result = ::Nokogiri::XML::NodeSet.new doc

      ::Nokogiri::HTML::DocumentFragment.parse(document).tap do |parsed|
        children = parsed.children
        injects = []

        children.each do |node|
          if node.name == "div"
            unless injects.empty?
              parent = Nokogiri::XML::Node.new "div", doc
              parent["class"] = "panel-body"

              injects.each do |inject|
                parent.add_child inject
              end

              result.push parent
              injects = []
            end

            result.push node
          else
            injects.push node

            if children.last == node and not injects.empty?
              parent = Nokogiri::XML::Node.new "div", doc
              parent["class"] = "panel-body"

              injects.each do |inject|
                parent.add_child inject
              end

              result.push parent
              injects = []
            end
          end
        end
      end

      result.to_html.html_safe
    end
  end
end
