module Crowbar
  module Generate
    class Dependencies
      attr_accessor :target
      attr_accessor :indent

      def initialize(target, indent = 2)
        self.target = target
        self.indent = indent
      end

      def process
        dependencies.each do |dependency|
          if dependency.name == "rails"
            railsinclude_processing dependency
          else
            dependencies_processing dependency
          end
        end

        content = target.read

        start, ends = railsinclude_marker
        raw_content = [
          start,
          railsinclude_result.join("\n\n"),
          "#{indent}#{ends}"
        ]

        content.gsub! /#{start}\n(.*)#{ends}/m, raw_content.join("\n")

        start, ends = dependencies_marker
        raw_content = [
          start,
          dependencies_result.join("\n\n"),
          "#{indent}#{ends}"
        ]

        content.gsub! /#{start}\n(.*)#{ends}/m, raw_content.join("\n")

        target.open("w") do |f|
          f.rewind
          f.write content
        end
      end

      protected

      def railsinclude_result
        @railsinclude_result ||= []
      end

      def railsinclude_processing(dependency)
        version = dependency.requirement.requirements.first.last.to_s
        railsinclude_result.push [
          "#{indent}gem \"#{dependency.name}\", version: \"#{version}\"",
          "#{indent}require \"rails/all\""
        ].join("\n")
      end

      def railsinclude_marker
        [
          "# RAILSINCLUDE START",
          "# RAILSINCLUDE END"
        ]
      end

      def dependencies_result
        @dependencies_result ||= []
      end

      def dependencies_processing(dependency)
        single_result = [
          "#{indent}gem \"#{dependency.name}\", version: \"#{dependency.requirement}\""
        ]

        if dependency.autorequire.nil?
          single_result.push "#{indent}require \"#{dependency.name}\""
        else
          unless dependency.autorequire.empty?
            single_result.push "#{indent}require \"#{dependency.autorequire.join}\""
          end
        end

        dependencies_result.push single_result.join("\n")
      end

      def dependencies_marker
        [
          "# DEPENDENCIES START",
          "# DEPENDENCIES END"
        ]
      end

      def dependencies
        parser.dependencies.select do |dependency|
          dependency.groups.include? :default
        end
      end

      def parser
        @parser = begin
          Gemnasium::Parser.gemfile(
            gemfile
          )
        end
      end

      def gemfile
        @gemfile ||= begin
          File.open(ENV["BUNDLE_GEMFILE"], "rb") do |f| 
            f.read
          end
        end
      end

      def indent
        " " * @indent
      end
    end
  end
end
