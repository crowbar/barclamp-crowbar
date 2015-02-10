class PlatformRequirement
  attr_reader :required_version, :required_platform

  attr_reader :cmp_operator, :required_version_string

  def initialize(required_platform, required_version)
    @required_version  = required_version
    @required_platform = required_platform

    @cmp_operator, @required_version_string = parse(required_version)

    @cmp_operator ||= "=="
    @required_version_string ||= required_version
  end

  def satisfied_by?(platform, version)
    if required_platform == platform
      if is_regexp?
        regexp_compare(version)
      else
        version_compare(version)
      end
    else
      false
    end
  end

  private

  def version_compare(version)
    passed   = PlatformVersion.new(version.to_s)
    required = PlatformVersion.new(required_version_string)

    if passed.respond_to?(cmp_operator.to_sym)
      passed.send(cmp_operator.to_sym, required)
    else
      false
    end
  end

  def regexp_compare(version)
    Regexp.new(regexp_body).match(version.to_s)
  end

  def parse(string)
    captures = string.scan(/^(>=?|<=?)\s*([.\d]+)$/)
    if captures.length > 0
      captures.first
    else
      []
    end
  end

  def regexp_body
    required_version[1..-2]
  end

  def is_regexp?
    required_version.start_with?('/') && required_version.end_with?('/')
  end
end

