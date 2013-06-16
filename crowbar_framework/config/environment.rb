# Be sure to restart your server when you modify this file

# Specifies gem version of Rails to use when vendor/rails is not present
# RAILS_GEM_VERSION = '2.3.5' unless defined? RAILS_GEM_VERSION

# Bootstrap the Rails environment, frameworks, and default configuration
require 'thread'
require File.join(File.dirname(__FILE__), 'boot')

Rails::Initializer.run do |config|
  # Settings in config/environments/* take precedence over those specified here.
  # Application configuration should go into files in config/initializers
  # -- all .rb files in that directory are automatically loaded.

  # Add additional load paths for your own custom dirs
  # config.load_paths += %W( #{RAILS_ROOT}/extras )

  unless AppConfig[:use_bundler]
    config.gem 'haml'
    config.gem 'sass'
    config.gem 'simple-navigation'
    config.gem 'i18n'
    config.gem 'json'
    config.gem 'compass-rails'
    config.gem 'bootstrap-sass'
  end

  # Load bootstrap and compass (needed for bootstrap when on Rails 2)
  require 'compass'
  require 'bootstrap-sass'

  # Only load the plugins named here, in the order given (default is alphabetical).
  # :all can be used as a placeholder for all plugins not explicitly named
  # config.plugins = [ :exception_notification, :ssl_requirement, :all ]

  # Skip frameworks you're not going to use. To use Rails without a database,
  # you must remove the Active Record framework.
  # config.frameworks -= [ :active_record, :active_resource, :action_mailer ]

  # Activate observers that should always be running
  # config.active_record.observers = :cacher, :garbage_collector, :forum_observer

  # Set Time.zone default to the specified zone and make Active Record auto-convert to this zone.
  # Run "rake -D time" for a list of tasks for finding time zone names.
  config.time_zone = 'UTC'

  # Log with a nice timestamp
  CROWBAR_LOG_DIR = "/var/log/crowbar" unless defined? CROWBAR_LOG_DIR
  config.logger = Logger.new("/var/log/crowbar/#{RAILS_ENV}.log")
  config.logger.formatter = Logger::Formatter.new

  # The default locale is :en and all translations from config/locales/*.rb,yml are auto loaded.
  # config.i18n.load_path += Dir[Rails.root.join('my', 'locales', '*.{rb,yml}')]
  # config.i18n.default_locale = :de

  CHEF_CLIENT_KEY = "/opt/dell/crowbar_framework/config/client.pem" unless defined? CHEF_CLIENT_KEY
  CHEF_NODE_NAME ="crowbar" unless defined? CHEF_NODE_NAME
  CHEF_SERVER_URL = "http://localhost:4000" unless defined? CHEF_SERVER_URL
  CHEF_ONLINE = true unless defined? CHEF_ONLINE
  OFFLINE_FILES_DIR = 'db' unless defined? OFFLINE_FILES_DIR
  CROWBAR_VERSION = '0.0.1' unless defined? CROWBAR_VERSION
  CONVERGED_ADMIN = true   #flag indicating at we can assume all Crowbar services on a single server
  HAVE_CHEF_WEBUI = true   #flag indicating whether it's okay to link to the chef webui
  SERVER_PID = %x[ps ax | grep "rainbows master" | grep -v grep].split(' ')[0]  # get a consistent number that changes when the server restarts
end
