require 'spec_helper'

describe PlatformVersion do
  it "correctly implements ordering" do
    cases = {
      ["1.2.0", "1.1.1"] => ["1.1.1", "1.2.0"],
      ["0.0.1", "0.1.0"] => ["0.0.1", "0.1.0"],
      ["1.0.0", "0.9.9"] => ["0.9.9", "1.0.0"],
    }

    cases.each do |versions, sorted|
      sorted_versions = versions.map { |v| PlatformVersion.new(v) }.sort.map(&:to_s)
      expect(sorted_versions).to eq(sorted)
    end
  end

  it "responds to other ops" do
    ver = PlatformVersion.new("1.2.1")
    expect(ver).to respond_to(:>)
  end
end
