# Evemacs

## Description

Tool like "PostEver" (for Android/iOS app) for Emacs.

This is used to make a note from one's day posts in Evernote.

## Installation

Please clone this repository.

  $ git clone git://github.com/yoshihara/evemacs.git

Then, please put below configuration to your .emacs or init.el.

    (add-to-list 'load-path "/path/to/evemacs_clone_directory")
    (require 'evemacs)

## Setting

You can specify a notebook name in your Evernote. Evemacs uses this
notebook to make notes. You can set it like below:

  (setq evemacs-notebook-name "My Notebook")

If you don't set it, Evemacs uses your default notebook.
Then, you should specify key-binding for 'evemacs-send-message
function like below:

  (global-set-key (kbd "C-c h") 'evemacs-send-message)

## Usage

1. Type ```M-x evemacs-send-message RET``` (or your key bind)
2. Type your message in a mini buffer

## Dependencies

This list shows libraries for runtime only. Please see evemacs.gemspec for development dependencies.

* ruby
* Ruby gems
  * evernote_oauth (Please run ```gem install evernote_oauth```)

## Author

Haruka Yoshihara (yshr04hrk at gmail.com)

## Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request
