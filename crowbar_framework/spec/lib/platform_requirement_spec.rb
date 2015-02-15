require 'spec_helper'

describe PlatformRequirement do
  it "works when specified as regexp" do
    req = PlatformRequirement.new("ubuntu", "/1.2.1/")

    expect(req).to be_satisfied_by("ubuntu", "1.2.1")
    expect(req).to_not be_satisfied_by("ubuntu", "1.3.1")
  end

  it "works when specified as string" do
    req = PlatformRequirement.new("ubuntu", "1.2.1")

    expect(req).to be_satisfied_by("ubuntu", "1.2.1")
    expect(req).to_not be_satisfied_by("ubuntu", "1.3.1")

    req = PlatformRequirement.new("ubuntu", "10.10.0")

    expect(req).to be_satisfied_by("ubuntu", "10.10.0")
    expect(req).to_not be_satisfied_by("ubuntu", "1.3.1")
  end

  it "works when specified using an operator" do
    req = PlatformRequirement.new("ubuntu", ">= 1.2.1")

    expect(req).to be_satisfied_by("ubuntu", "1.2.1")
    expect(req).to be_satisfied_by("ubuntu", "1.3.1")
    expect(req).to_not be_satisfied_by("ubuntu", "0.9.0")
    expect(req).to_not be_satisfied_by("ubuntu", "1.2.0")
  end

  it "returns false for incorrect input" do
    req = PlatformRequirement.new("ubuntu", ">= 1.2.1")

    expect(req).to_not be_satisfied_by("ubuntu", "sdfsdf")

    req = PlatformRequirement.new("ubuntu", "sdfsdf")

    expect(req).to_not be_satisfied_by("ubuntu", "1.2.1")
  end
end
