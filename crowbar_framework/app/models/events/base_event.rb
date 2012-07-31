
class Events::BaseEvent
  attr_accessor :event_type

  def initialize(t) 
    @event_type = t
  end

  def to_hash
    {}
  end

end

