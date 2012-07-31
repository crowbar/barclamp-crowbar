
class Events::JobEvent < Events::BaseEvent

  attr_accessor :msg

  def initialize(str)
    super("job_event")
    @msg = str
  end

  def to_hash
    { "msg" => @msg }
  end

end

