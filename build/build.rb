#!/usr/bin/env ruby

require 'yaml'
require 'rubygems'
require 'fileutils'

puts ">>> Collect pips required by crowbar.yml of all barclamps"

pip_cache_path = "#{ENV['BC_CACHE']}/files/pip_cache"
pip_requires = []

def pip_downloaded?(pip, cache = "./")
  pip = pip.strip
  name, requires = pip.scan(/^([\w\.-]*)(.*)$/).flatten
  requires = requires.split(",")
  in_cache = false
  FileUtils.cd(cache) do
    packages = (Dir.glob("#{name}*") + Dir.glob("#{name.gsub('_','-')}*")).uniq
    versions = packages.collect{|package| package.scan(/([0-9a-zA-Z\.]+)\.tar\.gz/).flatten.first}
    begin
      in_cache = versions.select do |version|
        requires.select { |require| not Gem::Dependency.new(name,require.gsub(/[a-zA-Z]/,"")).match?(name,version.gsub(/[a-zA-Z]/,"")) }.empty?
      end.any?
    rescue
      in_cache = false
    end
  end
  in_cache
end

Dir.glob("#{ENV['CROWBAR_DIR']}/barclamps/*/crowbar.yml").each do |file|
  crowbar = YAML.load_file(file)
  next if crowbar["pips"].nil?
  pip_requires += crowbar["pips"].collect{|i| i.strip}
end

pip_requires = pip_requires.select{|i| not i.strip.start_with?("#") and not i.strip.empty? }
pip_requires = pip_requires.uniq.sort
puts ">>> Pips to download: #{pip_requires.join(", ")}"

system("mkdir -p #{pip_cache_path}")
pip_requires.each do |pip|
  10.times do |attempt|
    if pip_downloaded?(pip,pip_cache_path)
      puts ">>> Skip #{pip}, already in cache"
      break
    end
    puts ">>> Try download pip: #{pip} (attempt: #{attempt+1}/10)"
    unless system("pip2tgz #{pip_cache_path} '#{pip}'")
      if attempt >= 9
        exit(1)
      else
        puts ">>> Retry exec pip2tgz"
      end
    else
      break
    end
  end
end

if File.directory?(pip_cache_path)
  raise "failed to package pip reqs" unless system("dir2pi #{pip_cache_path}")
end
puts ">>> Success build pips cache required by crowbar.yml of all barclamps"
