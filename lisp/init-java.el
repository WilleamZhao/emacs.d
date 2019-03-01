(require-package 'meghanada)
(require 'meghanada)

(require-package 'google-c-style)
(require 'google-c-style)

(require-package 'hideshow-org)
(require 'hideshow-org)


;;(require-package 'google-make-newline-indent)
;;(require 'google-make-newline-indent)

(require-package 'realgud)
;;(require 'realgud)

(add-hook 'java-mode-hook
          (lambda ()

            (google-set-c-style)
            (google-make-newline-indent)
            (realgud t)
            (imenu-add-menubar-index)
            (meghanada-mode t)
            (smartparens-mode t)
            (rainbow-delimiters-mode t)
            (highlight-symbol-mode t)

            (flycheck-mode +1)

            ;; use code format
            (c-set-style "java")
            (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)

            (require 'realgud)
            (setq indent-tabs-mode nil)
            (setq tab-width 2)
            (setq c-basic-offset 2)
            (setq meghanada-server-remote-debug t)
            (setq meghanada-javac-xlint "-Xlint:all,-processing")

            ;; hook
            (add-hook 'java-mode-hook 'hs-minor-mode)
            (add-hook 'java-mode-hook 'meghanada-mode)
            )
          )
;;(add-hook 'java-mode-hook
;;          (lambda ()
;;            (realgud t)
;;            (setq tab-width 2)
;;            (setq c-basic-offset 2)
;;            (setq meghanada-server-remote-debug t)
;;            (setq meghanada-javac-xlint "-Xlint:all,-processing")
;;          )
;;          )

(cond
   ((eq system-type 'windows-nt)
    (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME")))
    (setq meghanada-maven-path "mvn.cmd"))
   (t
    (setq meghanada-java-path "java")
    (setq meghanada-maven-path "mvn")))

;;(use-package meghanada
;;  :defer t
;;  :init
;;  (add-hook 'java-mode-hook
;;            (lambda ()
;;              (google-set-c-style)
;;              (google-make-newline-indent)
;;              (meghanada-mode t)
;;              (smartparens-mode t)
;;              (rainbow-delimiters-mode t)
;;              (highlight-symbol-mode t)
;;              (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))

;;  :config
;;  (use-package realgud
;;    :ensure t)
;;  (setq indent-tabs-mode nil)
;;  (setq tab-width 2)
;; (setq c-basic-offset 2)
;;  (setq meghanada-server-remote-debug t)
;;  (setq meghanada-javac-xlint "-Xlint:all,-processing")
;;  :bind
;;  (:map meghanada-mode-map
;;        ("C-S-t" . meghanada-switch-testcase)
;;        ("M-RET" . meghanada-local-variable)
;;        ("C-M-." . helm-imenu)
;;        ("M-r" . meghanada-reference)
;;        ("M-t" . meghanada-typeinfo)
;;        ("C-z" . hydra-meghanada/body))
;;  :commands
;;  (meghanada-mode))

(provide 'init-java)
