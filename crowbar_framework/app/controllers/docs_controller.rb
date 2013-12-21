# Copyright 2013, Dell
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#  http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

class DocsController < ApplicationController

  # no login required for docs!
  skip_before_filter :crowbar_auth

  def index
    if Settings.docs.rebuild or params.has_key?(:rebuild)
      # for dev, we want to be able to turn off rebuilds
      Doc.delete_all unless params[:rebuild].eql? "false"
      Doc.gen_doc_index
    end
    respond_to do |format|
      format.html # index.html.haml
      format.json { render :json => Doc.all }
    end
  end

  def fix_encoding!
    ['UTF-8', 'Windows-1252', 'ASCII'].each do |encoding|
      s = @raw.force_encoding encoding
      if s.valid_encoding?
        @raw = s
        return
      end
    end
    if @raw.encoding == Encoding::UTF_8
      (0...@raw.length).each do |i|
        @raw[c] = "\ufffd" unless @raw[c].valid_encoding?
      end
    end
  end

  def show
    begin
      id = params[:id]
      @doc = Doc.find_key id
      if @doc
        @nav_up = @doc.parent
        brothers = Doc.where(:parent_id=>@doc.parent_id).sort
        @nav_prev = nil
        @nav_next = nil
        reached = false
        brothers.each do |x|
          if reached
            @nav_next = x
            break
          elsif x.id == @doc.id
            reached = true
          else
          @nav_prev = x
          end
        end
        if not reached
          @nav_prev = nil
        end
        @file = File.join Doc.root_directory, @doc.name
      else
        raise "doc not found: #{id}"
        # @file = File.join Doc.root_directory, id
      end
      html = !params.has_key?(:source)
      image = false
      # navigation items
      if File.exist? @file
        if @file =~ /\.md$/
          @raw = IO.read(@file)
          fix_encoding! unless @raw.valid_encoding?
          @raw.encode!('UTF-8', :invalid=>:replace)
          @text = (html ? BlueCloth.new(@raw).to_html : @raw)
          # @text += Doc.topic_expand(@doc.name, html) if params.has_key? :expand
        elsif @file =~ /\.(jpg|png)$/
          html = false
          image = true
        end
      else
        @text = I18n.t('.topic_missing', :scope=>'docs.topic') + ": " + @file
      end
    rescue
      @text = I18n.t('.topic_error', :scope=>'docs.topic')  + ": " + @file
      flash[:notice] = @text
    end
    if image
      render :text=>open(@file, "rb").read, :content_type => :image, :content_disposition => "inline"
    elsif params.has_key? :expand
      if html
        render :layout => 'doc_export'
      else
        render :text=>@text, :content_type => :text
      end
    end
  end

  def export
    @docs = params[:q].split(/,/).map do |id|
      params[:id] = id
      show
      @text
    end
  end

end
