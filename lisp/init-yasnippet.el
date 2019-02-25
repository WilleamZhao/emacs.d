(require-package 'yasnippet)
(require 'yasnippet)
;;(require-package 'yasnippet-snippets)
;;(require 'yasnippet-snippets)

;; 使用Ctrl-c k作为唯一的触发快捷键
;;(define-key yas-minor-mode-map (kbd "<tab>") nil)
;;(define-key yas-minor-mode-map (kbd "TAB") nil)
;;(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets/java/"                 ;; personal snippets
        "~/.emacs.d/elpa-26.1/yasnippet-snippets-20190202.2145/snippets/"
        ))
(yas-global-mode t)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

(provide 'init-yasnippet)
