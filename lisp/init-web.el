;; web-mode
(require-package 'web-mode)
(require 'web-mode)

;;
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; php
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; asp/gsp/jsp
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
;; aspx/ascx
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; erb
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; mustache
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; djhtml
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;; html
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

(setq web-mode-enable-current-column-highlight t)

;;(require 'sgml-mode)
;; 预览

(when (maybe-require-package 'sgml-mode)
;;  (add-hook 'web-mode-hook
;;          (lambda ()
;;             (define-key web-mode-map (kbd "C-c C-v") 'browse-url-of-buffer)
;;    (define-key set-map (kbd "C-c C-v") 'browse-url-of-buffer)
;;              )
;;          )
  (define-key web-mode-map (kbd "C-c C-v") 'browse-url-of-buffer)
  )
(provide 'init-web)
