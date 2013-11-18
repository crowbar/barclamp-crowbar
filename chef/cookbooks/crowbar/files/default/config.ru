ENV["RAILS_ENV"] ||= ENV["RACK_ENV"]
require ::File.expand_path("../config/environment", __FILE__)

map "/assets" do
  run Utils::SprocketsEnvironment.env
end

map "/" do
  use Rails::Rack::Static
  run ActionController::Dispatcher.new
end
