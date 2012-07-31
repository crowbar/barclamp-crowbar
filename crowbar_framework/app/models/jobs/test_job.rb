
class Jobs::TestJob < Jobs::BaseJob

  def perform
    super
    puts "GREG: Jobs was tested"
  end

end


