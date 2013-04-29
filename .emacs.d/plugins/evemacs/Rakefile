# -*- coding: utf-8 -*-
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

namespace :test do
  desc "Run tests with elisp (using emacs)."
  task :elisp do
    emacs = ENV["EMACS"] || "emacs"
    @elisp_tests_exit_status = system("test/run-elisp-test.sh", emacs)
  end

  desc "Run tests with ruby."
  task :ruby do
    @ruby_tests_exit_status = ruby("test/run-ruby-test.rb")
  end
end

desc "Run tests."
task :test => ["test:ruby", "test:elisp"] do
  exit(@elisp_tests_exit_status && @ruby_tests_exit_status)
end

task :default => :test
