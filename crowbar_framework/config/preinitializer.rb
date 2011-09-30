begin
  require "rubygems"
  require "bundler" if defined? RAILS_VERSION and RAILS_VERSION.starts_with? == '3.'
rescue LoadError
  raise "Could not load the bundler gem. Install it with `gem install bundler`."
end

if defined? RAILS_VERSION and RAILS_VERSION.starts_with? == '3.'
  if Gem::Version.new(Bundler::VERSION) <= Gem::Version.new("0.9.24")
    raise RuntimeError, "Your bundler version is too old for Rails 2.3." +
     "Run `gem install bundler` to upgrade."
  end
end

begin
  # Set up load paths for all bundled gems
  ENV["BUNDLE_GEMFILE"] = File.expand_path("../../Gemfile", __FILE__)
  Bundler.setup if defined? RAILS_VERSION and RAILS_VERSION.starts_with? == '3.'
rescue Bundler::GemNotFound
  raise RuntimeError, "Bundler couldn't find some gems." +
    "Did you run `bundle install`?"
end
