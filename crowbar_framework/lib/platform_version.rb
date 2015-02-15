class PlatformVersion
  include Comparable

  attr_reader :as_array, :as_string

  def initialize(version)
    @as_string = version
    @as_array  = parse(version)
  end

  def to_s
    as_string
  end

  def to_a
    as_array
  end

  def <=>(other)
    pairwise_comparisons = to_a.zip(other.to_a).map do |v1_part, v2_part|
      v1_part <=> v2_part
    end
    pairwise_comparisons.find { |cmp| cmp != 0 } || 0
  end

  private

  def parse(version)
    case version.to_s
    when /^(\d+)\.(\d+)\.(\d+)$/
      [ $1.to_i, $2.to_i, $3.to_i ]
    when /^(\d+)\.(\d+)$/
      [ $1.to_i, $2.to_i, 0 ]
    when /^(\d+)$/
      [ $1.to_i, 0, 0 ]
    else
      [0, 0, 0]
    end
  end
end
