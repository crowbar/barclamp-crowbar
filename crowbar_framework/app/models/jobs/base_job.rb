

class Jobs::BaseJob

  def enqueue(job)
    record_stat 'enqueue'
  end

  def perform
    record_stat 'perform'
  end

  def before(job)
    record_stat 'start'
  end

  def after(job)
    record_stat 'after'
  end

  def success(job)
    record_stat 'success'
  end

  def error(job, exception)
    record_stat 'error'
  end

  def failure
    record_stat 'failure'
  end

private

  def record_stat(item)
    str = "#{self.class.name.demodulize}/#{item}"
    EventQueue.publish(Events::JobEvent.new(str))
  end

end


