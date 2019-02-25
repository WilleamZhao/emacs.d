(defun random-alphanum ()
  (let* ((charset "abcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*()")
         (x (random 46)))
    (char-to-string (elt charset x))))

;; 创建密码for org-capture
(defun create-password-org-capture ()
  (let ((value ""))
    (dotimes (number 16 value)
      (setq value (concat value (random-alphanum))))))

(defun create-password ()
  "create-password function, create 16 password"
  (interactive)
  (insert (create-password-org-capture))
  )

(defun get-or-create-password ()
  (setq password (read-string "Password: "))
  (if (string= password "")
      (create-password-org-capture)
    password))

;; epa-start
;;(setq epg-gpg-program "/usr/local/bin/gpg")
(require 'epa-file)
(epa-file-enable)
;; 脚本位置
(custom-set-variables '(epg-gpg-program "/usr/local/bin/gpg"))
;; 总是使用对称加密
(setq epa-file-encrypt-to "393B76F8FD82DC7B7A5E79AB3251A10218FB9FDB")

;; 允许缓存密码，否则编辑时每次保存都要输入密码
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
;; 不用每次都提醒
(setq epa-file-select-keys 0)
;; 禁用自动保存
(setq epa-file-inhibit-auto-save t)
;; home
(setq epg-gpg-home-directory "/Users/sourcod/.gnupg/")

;; emacs内部输入密码
(setq epa-pinentry-mode 'loopback)

;; Do not use gpg agent when runing in terminal
(defadvice epg--start (around advice-epg-disable-agent activate)
  (let ((agent (getenv "GPG_AGENT_INFO")))
    (when (not (display-graphic-p))
      (setenv "GPG_AGENT_INFO" nil))
    ad-do-it
    (when (not (display-graphic-p))
      (setenv "GPG_AGENT_INFO" agent))))

(ad-enable-advice 'epg--start 'around 'advice-epg-disable-agent)
(ad-activate 'epg--start)

;; kill emacs after kill gpg-agent
(add-hook 'kill-emacs-hook (defun personal--kill-gpg-agent ()
                             (shell-command "pkill gpg-agent")))

(provide 'init-password)
