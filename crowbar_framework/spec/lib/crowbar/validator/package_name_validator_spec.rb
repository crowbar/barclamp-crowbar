require 'spec_helper'

describe Crowbar::Validator::PackageNameValidator do
  it "allows valid package names" do
    valid_names = [
      "letters",
      "numbers123",
      "with-dash",
      "with_underscore",
      "with+plus",
      "with.dot",
      "version_OP>=1.2-3_a",
      "version_OP<=something",
      "version_OP<foo",
      "version_OP>bar",
      "version_OP=baz",
      "all._at+-0nce<=V-e+r.s_1on"
    ] 
    valid_names.each do |package_name|
      expect(Crowbar::Validator::PackageNameValidator.new.validate(package_name)).to be_truthy
    end
  end

  it "doesn't allow invalid package names" do
    invalid_names = [
      "-leading-non-wordchar",
      "bad;char1",
      "bad&char2",
      "new\nline",
      "tab\ttab",
      "blank space",
      "broken_version_OP>>1",
      "broken_version_OP<<2",
      "broken_version_OP=>3",
      "broken_version_OP=<4",
      "broken_version_OP==5",
      "broken>version_OP>6",
      "broken<version_OP<7",
    ]
    invalid_names.each do |package_name|
      expect(Crowbar::Validator::PackageNameValidator.new.validate(package_name)).to be_falsey
    end
  end
end
