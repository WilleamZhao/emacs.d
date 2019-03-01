;;; init-html.el --- Editing HTML -*- lexical-binding: t -*-
;;; Commentary:

;; ERB is configured separately in init-ruby

;;; Code:

(require-package 'tagedit)
(after-load 'sgml-mode
  (tagedit-add-paredit-like-keybindings)
  (define-key tagedit-mode-map (kbd "M-?") nil)
  (define-key tagedit-mode-map (kbd "M-s") nil)
  (add-hook 'sgml-mode-hook (lambda () (tagedit-mode 1))))

(add-auto-mode 'html-mode "\\.\\(jsp\\|tmpl\\|html\\|htm\\)\\'")
;;(add-hook 'html-mode-hook
;;          (lambda ()
;;            ;;(google-set-c-style)
;;            (setq tab-width 2)
;;            (setq c-basic-offset 2)
;;            )
;;          )

(provide 'init-html)
;;; init-html.el ends here
