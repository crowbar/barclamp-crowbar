module Crowbar
  module Validator
    class PackageNameValidator

      def validate(package_name)
        package_name =~ /\A(\w[\w\+\.-]+(?:[<>]?=?[\w\+\.-]+)?)\z/
      end

    end
  end
end
