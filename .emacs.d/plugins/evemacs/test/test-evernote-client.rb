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

class TestEvernoteClient < Test::Unit::TestCase
  def setup
    @token = "token"
  end

  def test_default_notebook
    mock_with_evernote_oauth_client do
      mock(EvernoteOAuth::NoteStore).getDefaultNotebook(@token) { "notebook" }
    end

    assert_equal("notebook", EvernoteClient.new(@token).default_notebook)
  end

  def test_valid_guid?

  end

  def test_notebooks
    notebook_list = ["Inbox", "Archives", "ToDoList"]
    mock_with_evernote_oauth_client do
      mock(EvernoteOAuth::NoteStore).listNotebooks { notebook_list }
    end
    assert_equal(notebook_list, EvernoteClient.new(@token).notebooks)
  end

  def test_find_notes

  end

  def test_create_note

  end

  def mock_with_evernote_oauth_client
    mock(EvernoteOAuth::Client).new(:token => @token, :sandbox => false) do
      mock(EvernoteOAuth::Client).note_store do
        yield
      end
    end
  end
end

