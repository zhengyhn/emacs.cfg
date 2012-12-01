;the cfg for coding

;======================================================
;code style
;======================================================
;tab distance
(setq tab-width 4
      c-basic-offset 4)
;c/c++ style
(setq c-default-style
      '((c-mode . "k&r")
	(c++-mode . "ellemtel")
       )
)      
;======================================================


;======================================================
;smart complie
;======================================================

(load-file "~/.emacs.d/plugins/smart-compile.el")
(global-set-key [f9] 'smart-compile)
(global-set-key [f10] 'gdb)

;======================================================


;===================================================================
;auto-complete
;===================================================================

(add-to-list 'load-path "~/.emacs.d/plugins/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/plugins/auto-complete//ac-dict")
(ac-config-default)

;======================================================================


;======================================================================
;cedet
;======================================================================

(load-file "~/.emacs.d/plugins/cedet-1.1/common/cedet.el");
;(global-ede-mode 1)

;semantic
;(semantic-load-enable-code-helpers)
;(semantic-load-enable-gaudy-code-helpers)
;(semantic-load-enable-semantic-debugging-helpers)
;(global-srecode-minor-mode 1)

;(require 'semantic/ia)    ;name completion

;(require 'semantic/bovine/gcc)    ;finding c/c++ header files

;(defun my-cedet-hook()
;  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
;  (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
;  (local-set-key "\C-c>" 'semantic-ia-complete-analyze-inline)
;  (local-set-key "\C-cp" 'semantic-ia-analyze-proto-impl-toggle)
;  (local-set-key "." 'semantic-complete-self-insert)
;  (local-set-key ">" 'semantic-complete-self-insert))

;(add-hook 'c-mode-common-hook 'my-cedet-hook)

;code jump
;(global-set-key (kbd "C-'") 'semantic-ia-fast-jump)

;code complete
;(global-set-key (kbd "M-n") 'semantic-ia-complete-symbol-menu)

;=========================================================================


;=======================================================================
;ecb
;=======================================================================

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

;==================================================================


;==========================================================
;web-mode
;==========================================================

(load-file "~/.emacs.d/plugins/web-mode.el")
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;===========================================================
