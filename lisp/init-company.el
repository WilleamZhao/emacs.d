;;; init-company.el --- Completion with company -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; WAITING: haskell-mode sets tags-table-list globally, breaks tags-completion-at-point-function
;; TODO Default sort order should place [a-z] before punctuation

(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(when (maybe-require-package 'company)
  (add-hook 'after-init-hook 'global-company-mode)
  (after-load 'company
    (dolist (backend '(company-eclim company-semantic))
      (delq backend company-backends))
    (diminish 'company-mode)
    (define-key company-mode-map (kbd "M-/") 'company-complete)
    (define-key company-active-map (kbd "M-/") 'company-other-backend)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    (setq-default company-dabbrev-other-buffers 'all
                  company-tooltip-align-annotations t))
  (global-set-key (kbd "M-C-/") 'company-complete)
  (when (maybe-require-package 'company-quickhelp)
    (add-hook 'after-init-hook 'company-quickhelp-mode)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(global-set-key [tab] 'tab-indent-or-complete)

;; Suspend page-break-lines-mode while company menu is active
;; (see https://github.com/company-mode/company-mode/issues/416)
;;(after-load 'company
;;  (after-load 'page-break-lines
;;    (defvar-local sanityinc/page-break-lines-on-p nil)

;;    (defun sanityinc/page-break-lines-disable (&rest ignore)
;;      (when (setq sanityinc/page-break-lines-on-p (bound-and-true-p page-break-lines-mode))
;;        (page-break-lines-mode -1)))

;;    (defun sanityinc/page-break-lines-maybe-reenable (&rest ignore)
;;      (when sanityinc/page-break-lines-on-p
;;        (page-break-lines-mode 1)))

;;    (add-hook 'company-completion-started-hook 'sanityinc/page-break-lines-disable)
;;    (add-hook 'company-after-completion-hook 'sanityinc/page-break-lines-maybe-reenable)))

(advice-add 'company-complete-common :before (lambda ()
                                (setq my-company-point (point))))
(advice-add 'company-complete-common :after (lambda ()
                                              (when (equal my-company-point (point)) (yas-expand))))

;;(add-hook ‘after-init-hook ‘global-company-mode)
;;(setq company-idle-delay 0.5)
;; weight by frequency
;;(setq company-transformers ‘(company-sort-by-occurrence))

;;(defvar company-mode/enable-yas t "Enable yasnippet for all backends.")

;;(defun company-mode/backend-with-yas (backend)
;;  (if (or (not company-mode/enable-yas) (and (listp backend)    (member ‘company-yasnippet backend)))
;;      backend
;;(append (if (consp backend) backend (list backend))
;;        ‘(:with company-yasnippet))))

;;(setq company-backends (mapcar #‘company-mode/backend-with-yas company-backends))

(provide 'init-company)
;;; init-company.el ends here
