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

require "evernote-client"
require "evemacs/options"

module Evemacs
  class Executor
    attr_reader :client

    def initialize
      @options = nil
      @now = Time.now
    end

    def run(argv)
      @options = Evemacs::Options.new(argv)

      title = @now.strftime("%Y/%m/%d")

      @client = ::EvernoteClient.new(@options.token)
      message_notebook_guid = fetch_notebook_guid(@options.notebook)
      today_note = fetch_today_note(message_notebook_guid, title)

      if today_note.nil?
        create_message_note(message_notebook_guid, title, @options.message)
      else
        update_message_note(today_note.guid, title, @options.message)
      end
    end

    def fetch_today_note(message_notebook_guid, date)
      options = {:guid => message_notebook_guid, :words => "intitle:#{date}"}
      today_note_list = @client.find_notes(options)
      if today_note_list.empty?
        nil
      else
        today_note_list.first
      end
    end

    def fetch_notebook_guid(notebook)
      if notebook.nil?
        return @client.default_notebook.guid
      end

      existing_notebooks = @client.notebooks
      message_notebooks = existing_notebooks.select do |existing_notebook|
        existing_notebook.name.force_encoding("UTF-8")
        existing_notebook.name == notebook
      end
      message_notebooks.first.guid
    end

    def create_message_note(message_notebook_guid, title, message)
      header = <<-EOH
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE en-note SYSTEM "http://xml.evernote.com/pub/enml2.dtd">
EOH
      today_content =
        "#{header}" +
        "<en-note>#{entry(message)}</en-note>"
      @client.create_note(message_notebook_guid, title, today_content)
    end

    def update_message_note(today_note_guid, title, message)
      today_content = @client.note_content(today_note_guid)
      if /\<en-note\>(.+)\<\/en-note\>/m =~ today_content
        existing_content = $1.force_encoding("UTF-8")
        content = "#{existing_content}#{entry(message)}"
        today_content = today_content.gsub(/(\<en-note\>).+(\<\/en-note\>)/m,
                                             "\\1#{content}\\2")
        @client.update_note(today_note_guid, title, today_content)
      end
    end

    def entry(message)
      # TODO: URL escape?
      time = @now.strftime("%H:%M:%S")
      "<p>#{message}<br/><font color=\"#777\">#{time}</font></p>"
    end
  end
end
