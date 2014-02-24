class FileLock
  def self.acquire(name, options = {})
    logger = options.fetch(:logger) { self.logger }

    logger.debug("Acquire #{name} lock enter as uid #{Process.uid}")
    path = "tmp/#{name}.lock"
    begin
      f = File.new(path, File::RDWR|File::CREAT, 0644)
    rescue
      logger.error("Couldn't open #{path} for locking: #$!")
      logger.error("cwd was #{Dir.getwd})")
      raise "Couldn't open #{path} for locking: #$!"
    end
    logger.debug("Acquiring #{name} lock")
    rc = false
    count = 0
    while rc == false do
      count = count + 1
      logger.debug("Attempt #{name} Lock: #{count}")
      rc = f.flock(File::LOCK_EX|File::LOCK_NB)
      sleep 1 if rc == false
    end
    logger.debug("Acquire #{name} lock exit: #{f.inspect}, #{rc}")
    f
  end

  def self.release(f, options = {})
    logger = options.fetch(:logger) { self.logger }
    logger.debug("Release lock enter: #{f.inspect}")
    if f
      f.flock(File::LOCK_UN)
      f.close
    else
      logger.warn("release_lock called without valid file")
    end
    logger.debug("Release lock exit")
  end

  def self.logger
    defined?(Rails) ? Rails.logger : Logger.new(STDERR)
  end
end

