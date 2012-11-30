;the cfg for coding

;tab distance
(setq tab-width 4
      c-basic-offset 4)

;cedet
(load-file "~/.emacs.d/plugins/cedet-1.1/common/cedet.el");
;(require 'cedet)
;(global-ede-mode 1)
(semantic-load-enable-code-helpers)
;(semantic-load-enable-gaudy-code-helpers)
;(semantic-load-enable-semantic-debugging-helpers)
;(global-srecode-minor-mode 1)

;code jump
(global-set-key (kbd "C-'") 'semantic-ia-fast-jump)

;code complete
;(global-set-key (kbd "M-n") 'semantic-ia-complete-symbol-menu)


;ecb
(add-to-list 'load-path "~/.emacs.d/plugins/ecb-2.40")
(require 'ecb)
;(require 'ecb-autoloads)
(custom-set-variables
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2)))
(custom-set-faces)
(setq stack-trace-on-error nil)
(setq ecb-auto-activate nil
      ecb-tip-of-the-day nil)
;C-; to start ecb
(defun ecb-open-close()
  (interactive)
  (if ecb-minor-mode
      (ecb-deactivate)
      (ecb-activate)
   )
)
(global-set-key (kbd "C-;") 'ecb-open-close)


;web-mode
(load-file "~/.emacs.d/plugins/web-mode.el")
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
