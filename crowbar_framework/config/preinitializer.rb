if AppConfig[:use_bundler]
  begin
    require 'bundler'
  rescue LoadError
    raise "Could not load the bundler gem.\n" +
      "Install it with `gem install bundler` and then `bundle install`."
  end

  if Gem::Version.new(Bundler::VERSION) <= Gem::Version.new("0.9.24")
    raise RuntimeError, "Your bundler version is too old for Rails 2.3.\n" +
     "Run `gem install bundler` to upgrade."
  end

  begin
    # Set up load paths for all bundled gems
    ENV["BUNDLE_GEMFILE"] = File.expand_path("../../Gemfile", __FILE__)
    Bundler.setup
  rescue Bundler::GemNotFound
    raise RuntimeError, "Bundler couldn't find some gems.\n" +
      "Did you run `bundle install`?"
  end
end
