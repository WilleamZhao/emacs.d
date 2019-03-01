;; neotree
(require-package 'neotree)
(require 'neotree)
;; all-the-icons
(require-package 'all-the-icons)
(require 'all-the-icons)

;; open neotree
(global-set-key [f8] 'neotree-toggle)

;; 切换项目时自动切换neotree目录
(setq projectile-switch-project-action 'neotree-projectile-action)

;; find-file-in-project
(require-package 'find-file-in-project)
(require 'find-file-in-project)

;; 定位到文件
(after-load 'projectile
  (defun neotree-ffip-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (if project-dir
          (progn
            (neotree-dir project-dir)
            (neotree-find file-name))
        (message "Could not find git project root."))))

  (define-key projectile-mode-map (kbd "C-c C-p") 'neotree-ffip-project-dir)
  )

(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(provide 'init-tree)
