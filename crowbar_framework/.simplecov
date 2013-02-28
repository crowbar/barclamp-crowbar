if RUBY_VERSION != '1.8.7'
  require 'simplecov'
  SimpleCov.start 'rails' do
    use_merging true
  end
end
