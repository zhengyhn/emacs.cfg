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

require "optparse"

module Evemacs
  class Options
    attr_reader :token ,:message, :notebook

    def initialize(argv)
      @token = nil
      @message = nil
      @notebook = nil

      parser = OptionParser.new

      parser.on("-t", "--taken=TOKEN", "evenote token") do |token|
        @token = token
      end

      parser.on("-m", "--message=MESSAGE", "added message") do |message|
        @message = message.force_encoding("UTF-8")
      end

      parser.on("-n", "--notebook=NOTEBOOK",
                "notebook to add message") do |notebook|
        @notebook = notebook.force_encoding("UTF-8")
      end

      parser.parse!(argv)
    end
  end
end
