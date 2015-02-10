class PlatformRequirement
  attr_reader :as_string

  attr_reader :cmp_operator, :required_version_string

  def initialize(string)
    @as_string = string

    @cmp_operator, @required_version_string = parse(string)

    @cmp_operator ||= "=="
    @required_version_string ||= string
  end

  def satisfied_by?(version)
    if is_regexp?
      regexp_compare(version)
    else
      version_compare(version)
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
    as_string[1..-2]
  end

  def is_regexp?
    as_string.start_with?('/') && as_string.end_with?('/')
  end
end

