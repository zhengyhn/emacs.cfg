# -*- coding: utf-8; mode: ruby -*-
#
# Copyright (C) 2013  Haruka Yoshihara <yshr04hrk@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

base_dir = File.dirname(__FILE__)
$LOAD_PATH.unshift(File.expand_path("lib", base_dir))
require "evemacs/version"

version = Evemacs::VERSION.dup

readme_path = File.join(base_dir, "README.md")
entries = File.read(readme_path).split(/^##\s(.*)$/)
entry = lambda do |entry_title|
  entries[entries.index(entry_title) + 1]
end

clean_white_space = lambda do |entry|
  entry.gsub(/(\A\n+|\n+\z)/, '') + "\n"
end
description = clean_white_space.call(entry.call("Description"))
summary, description = description.split(/\n\n+/, 2)

Gem::Specification.new do |spec|
  spec.name = "evemacs"
  spec.version = version
  spec.authors = "Haruka Yoshihara"
  spec.email = "yshr04hrk@gmail.com"
  spec.summary = summary
  spec.description = description

  spec.files = ["README.md", "Rakefile", "Gemfile", "COPYING"]
  spec.files += Dir.glob("lib/**/*.rb")
  spec.files += Dir.glob("doc/text/*.*")
  spec.test_files = Dir.glob("test/**/*.rb")
  Dir.chdir("bin") do
    spec.executables = Dir.glob("*")
  end

  spec.licenses = ["GPLv3"]

  spec.add_runtime_dependency("evernote_oauth")
  spec.add_development_dependency("rake")
  spec.add_development_dependency("yard")
  spec.add_development_dependency("test-unit")
  spec.add_development_dependency("test-unit-notify")
  spec.add_development_dependency("test-unit-rr")
  spec.add_development_dependency("bundler")
end

