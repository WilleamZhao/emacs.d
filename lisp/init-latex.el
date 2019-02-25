;;; auctex
(require-package 'auctex)

(load "auctex.el" nil t t)
(require 'tex-mik)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-PDF-mode t)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(require 'tex)
(TeX-global-PDF-mode t)

; XeLaTeX
(add-hook 'LaTeX-mode-hook (lambda()
                             (setq TeX-global-PDF-mode t TeX-engine 'xetex)
                             (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
                             (setq TeX-command-default "XeLaTeX")
                             (setq TeX-save-query  nil )
                             (setq TeX-show-compilation t)
                             ))

(setq TeX-clean-confirm nil)

(provide 'init-latex)
