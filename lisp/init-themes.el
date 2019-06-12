;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'color-theme-sanityinc-solarized)
(require-package 'color-theme-sanityinc-tomorrow)

;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(sanityinc-tomorrow-bright))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)


;;------------------------------------------------------------------------------
;; Toggle between light and dark
;;------------------------------------------------------------------------------
(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(ityinc-tomorrow-day))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
  (reapply-themes))


(when (maybe-require-package 'dimmer)
  (setq-default dimmer-fraction 0.15)
  (add-hook 'after-init-hook 'dimmer-mode)
  ;; TODO: file upstream as a PR
  (after-load 'dimmer
    (advice-add 'frame-set-background-mode :after (lambda (&rest args) (dimmer-process-all)))))

;; show number line
;;(global-linum-mode t)

;; cursor-type
(setq-default cursor-type 'bar)

;; (setq cursor-type 'bar)

;; powerline-start
(require-package 'powerline)
(require 'powerline)
(powerline-default-theme)

(setq powerline-arrow-shape 'arrow) ;; the default
(setq powerline-arrow-shape 'curve) ;; give your mode-line curves
(setq powerline-arrow-shape 'arrow14) ;; best for small fonts

(custom-set-faces
 '(mode-line ((t (:foreground "#fff" :background "#444" :box nil))))
 '(mode-line-inactive ((t (:foreground "#d0d0d0" :background "#666666" :box nil)))))

(setq powerline-color1 "grey22")
(setq powerline-color2 "grey40")
;; powerline-end

;; 打开org-indent mode
;;(setq org-startup-indented t)

;; 设置bullet list
;;(setq org-bullets-bullet-list '("☰" "☷" "☯" "☭"))

;; 调试好久的颜色，效果超赞！ todo keywords 增加背景色
;;(setf org-todo-keyword-faces '(("TODO" . (:foreground "white" :background "#95A5A6"   :weight bold))
;;                               ("HAND" . (:foreground "white" :background "#2E8B57"  :weight bold))
;;                               ("DONE" . (:foreground "white" :background "#3498DB" :weight bold))))

;; 配置归档文件的名称和Headline格式
;; (setq org-archive-location "%s_archive::date-tree")

(provide 'init-themes)
;;; init-themes.el ends here
