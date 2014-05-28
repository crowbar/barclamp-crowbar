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

module BarclampsHelper
  def barclamp_collapse_list(title, collapsed = false)
    icon_class = if collapsed
      "chevron-down"
    else
      "chevron-right"
    end

    "#{icon_tag(icon_class)} #{title}"
  end

  def display_led_for(state, property, path = false)
    options = {
      :class => "led #{state}",
      :id => property,
      :title => t(state.to_s, :scope => "proposal.status")
    }

    options["data-ledupdate"] = path if path

    content_tag(
      :span,
      "",
      options
    )
  end

  def display_name_for(barclamp)
    @display_name_for ||= {}

    @display_name_for[barclamp] ||= begin
      if barclamp.respond_to? :display_name
        barclamp.display_name
      else
        BarclampCatalog.display_name(barclamp)
      end
    end
  end

  def show_barclamp_button(barclamp, inst, options = {})
    link_to t(".show"), "/crowbar/#{barclamp}/1.0/#{inst}", { :class => "btn btn-default show" }.merge(options)
  end

  def edit_barclamp_button(barclamp, inst, options = {})
    link_to t(".edit"), "/crowbar/#{barclamp}/1.0/proposals/#{inst}", { :class => "btn btn-default edit" }.merge(options)
  end

  def cancel_barclamp_button(barclamp)
    link_to t("cancel"), barclamp_modules_path(:id => barclamp), :class => "btn btn-default cancel"
  end

  def render_barclamp_show_attributes(barclamp, raw)
    if raw
      render :partial => "barclamp/show_attributes_raw"
    else
      render :partial => "barclamp/#{barclamp}/show_attributes"
    end
  rescue ::ActionView::MissingTemplate
    render :partial => "barclamp/show_attributes_raw"
  end

  def render_barclamp_show_deployment(barclamp, raw)
    if raw
      render :partial => "barclamp/show_deployment_raw"
    else
      render :partial => "barclamp/#{barclamp}/show_deployment"
    end
  rescue ::ActionView::MissingTemplate
    render :partial => "barclamp/show_deployment_raw"
  end

  def render_barclamp_edit_attributes(proposal, raw)
    if raw
      render :partial => 'barclamp/edit_attributes_raw'
    else
      if Rails.root.join("app", "views", "barclamp", proposal.barclamp, "_edit_attributes.html.haml").file?
        render :partial => "barclamp/#{proposal.barclamp}/edit_attributes"
      else
        # Currently this view does not exist, but should be the 
        # fallback way to show a generic form or message
        render :partial => "barclamp/general/edit_attributes"
      end
    end
  rescue ::ActionView::MissingTemplate
    render :partial => "barclamp/edit_attributes_raw"
  rescue ::StandardError => e
    puts "Attribute Exception #{e.class}: #{e.message}"
    puts e.backtrace.join("\n")

    raise e if Rails.env.development?
    render :partial => "barclamp/edit_attributes_raw"
  rescue ::SyntaxError => e
    raise e
  end

  def render_barclamp_edit_deployment(proposal, raw)
    if raw
      render :partial => 'barclamp/edit_deployment_raw'
    else
      if Rails.root.join("app", "views", "barclamp", proposal.barclamp, "_edit_deployment.html.haml").file?
        render :partial => "barclamp/#{proposal.barclamp}/edit_deployment"
      else
        render :partial => "barclamp/general/edit_deployment"
      end
    end
  rescue ::ActionView::MissingTemplate
    render :partial => "barclamp/edit_deployment_raw"
  rescue ::StandardError => e
    puts "Deployment Exception #{e.class}: #{e.message}"
    puts e.backtrace.join("\n")

    raise e if Rails.env.development?
    render :partial => "barclamp/edit_deployment_raw"
  rescue ::SyntaxError => e
    raise e
  end

  def render_barclamp_index(barclamp)
    render :partial => "barclamp/#{barclamp}/index"
  rescue ::ActionView::MissingTemplate
    render :partial => "barclamp/index"
  end
end
