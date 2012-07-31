
class Events::WebEvent < Events::BaseEvent

  attr_accessor :msg

  def initialize(str)
    super("web_event")
    @msg = str
  end

  def to_hash
    { "msg" => @msg }
  end

end

