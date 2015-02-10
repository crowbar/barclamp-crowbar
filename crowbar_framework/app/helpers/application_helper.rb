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

module ApplicationHelper
  # Check if we are using a quite old bad internet explorer, currently used for
  # disableing drag and drop for this old browser
  def bad_explorer?
    @bad_explorer ||= request.env["HTTP_USER_AGENT"].downcase =~ /msie ([1-8])/
  end

  # Check if we are running on a suse system
  def suse_system?
    @suse_system ||= File.exist?("/etc/SuSE-release")
  end

  # A simple wrapper to access the branding configuration directly, looks much
  # cleaner within the views
  def branding_config
    @branding_config ||= begin
      config = YAML.load_file(Rails.root.join("config", "branding.yml")) rescue {}
      Hashie::Mash.new(config)
    end
  end

  # Generate the meta title that gets displayed on the page meta information
  def meta_title
    title = [branding_config[:page_title], branding_config[:page_slogan]].compact.join(" ")
    "#{title}: #{controller.action_name.titleize}"
  end

  # This method gets extended in the future to include anywhere registered
  # stylesheets to get the stylesheets more dynamic included
  def registered_stylesheets
    @registered_stylesheets ||= begin
      files = [
        "application"
      ]

      barclamp_name = if @service_object
        @service_object.bc_name rescue "crowbar"
      else
        "crowbar"
      end

      if barclamp_name == "barclamp"
        barclamp_name = "crowbar"
      end

      barclamp_path = Rails.root.join(
        "app",
        "assets",
        "stylesheets",
        "barclamps",
        barclamp_name,
        "application.css.scss"
      )

      if barclamp_path.file?
        files.push "barclamps/#{barclamp_name}/application"
      end

      files
    end
  end

  # Register more stylesheets to the collection, they are getting included in
  # the document head
  def register_stylesheets(*stylesheets)
    registered_stylesheets.push(stylesheets).flatten!
  end

  # This method gets extended in the future to include anywhere registered
  # javascripts to get the javascripts more dynamic included
  def registered_javascripts
    @registered_javascripts ||= begin
      files = [
        "application"
      ]

      barclamp_name = if @service_object
        @service_object.bc_name rescue "crowbar"
      else
        "crowbar"
      end

      if barclamp_name == "barclamp"
        barclamp_name = "crowbar"
      end

      barclamp_path = Rails.root.join(
        "app",
        "assets",
        "javascripts",
        "barclamps",
        barclamp_name,
        "application.js"
      )

      if barclamp_path.file?
        files.push "barclamps/#{barclamp_name}/application"
      end

      files
    end
  end

  # Register more javascripts to the collection, they are getting included in
  # the document head
  def register_javascripts(*javascripts)
    registered_javascripts.push(javascripts).flatten!
  end

  # Generate the page title that gets displayed on every page within the header
  def page_title
    [].tap do |output|
      output.push content_tag(
        :span,
        branding_config[:page_title],
        :class => "title"
      )

      unless branding_config[:page_slogan].empty?
        output.push content_tag(
          :span,
          branding_config[:page_slogan],
          :class => "slogan"
        )
      end
    end.join("\n").html_safe
  end

  # Include required meta tags like csrf token, viewport and such stuff
  def meta_tags
    [].tap do |output|
      output.push tag(
        :meta,
        :charset => "utf-8"
      )

      output.push tag(
        :meta,
        :content => "IE=edge",
        "http-equiv" => "X-UA-Compatible"
      )

      output.push tag(
        :meta,
        :name => "viewport",
        :content => "width=device-width, initial-scale=1.0"
      )

      if protect_against_forgery?
        output.push tag(
          :meta,
          :name => "csrf-param",
          :content => Rack::Utils.escape_html(request_forgery_protection_token)
        )

        output.push tag(
          :meta,
          :name => "csrf-token",
          :content => Rack::Utils.escape_html(form_authenticity_token)
        )
      end
    end.join("\n").html_safe
  end

  def have_openstack
    @have_openstack ||= begin
      defined? OpenstackService
    end
  end

  # Build a wrapper for the crowbar service options, feels much better
  # within the views to simply call a method for getting the hash
  def crowbar_service
    @crowbar_service ||= begin
      CrowbarService
    end
  end

  # Build a wrapper for the crowbar service options, feels much better
  # within the views to simply call a method for getting the hash
  def crowbar_options
    @crowbar_options ||= begin
      crowbar_service.read_options
    end
  end

  def flash_for(value)
    case value.to_sym
    when :notice
      "success"
    when :alert
      "danger"
    else
      value.to_s
    end
  end

  def dash_or(value)
    if value.nil? or value.empty?
      content_tag(
        :span,
        "&mdash;".html_safe,
        :class => "empty"
      )
    else
      value.html_safe
    end
  end

  def value_for(value, fallback, condition = nil)
    is_empty = if condition.nil?
      value.nil? or value.empty?
    else
      ! condition
    end

    if is_empty
      content_tag(
        :span,
        fallback.html_safe,
        :class => "empty"
      )
    else
      value.html_safe
    end
  end

  def default_platform
    NodeObject.default_platform
  end

  def available_platforms
    NodeObject.available_platforms
  end

  def disabled_platforms
    NodeObject.disabled_platforms
  end
end
