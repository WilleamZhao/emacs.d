;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; projectile
(when (maybe-require-package 'projectile)
  (add-hook 'after-init-hook 'projectile-mode)

  ;; Shorter modeline
  (setq-default projectile-mode-line-prefix " Proj")

  (after-load 'projectile
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (define-key projectile-mode-map [?\s-a] 'projectile-find-dir)
    (define-key projectile-mode-map [?\s-p] 'projectile-switch-project)
    (define-key projectile-mode-map [?\s-f] 'projectile-find-file)
    (define-key projectile-mode-map [?\s-g] 'projectile-grep)
    )
  (maybe-require-package 'ibuffer-projectile))
;;(ffip-project-root)
;;(projectile-project-p)
;;(projectile-project-dirs)
(provide 'init-projectile)
;;; init-projectile.el ends here
