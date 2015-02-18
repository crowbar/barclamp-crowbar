module Deprecate
  def deprecate_warning(new_method, file, line, options = {})
    old     = caller[0].match(/`(.*)'/)[1]
    logger  = options.fetch(:logger) { Rails.logger }
    message = options.fetch(:message) { "[DEPRECATED] #{old}, defined at #{file.gsub(Rails.root.to_s, '')} #{line} is deprecated and will be removed, please use #{new_method} instead.\nCalled from #{caller[3]}" }
    logger.warn(message)
  end
end
