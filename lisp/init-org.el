;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when *is-a-mac*
  (maybe-require-package 'grab-mac-link))

(maybe-require-package 'org-cliplink)

(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)

;; Various preferences
(setq org-log-done t
      org-edit-timestamp-down-means-later t
      org-hide-emphasis-markers t
      org-catch-invisible-edits 'show
      org-export-coding-system 'utf-8
      org-fast-tag-selection-single-key 'expert
      org-html-validation-link nil
      org-export-kill-product-buffer-when-displayed t
      org-tags-column 80)


;; Lots of stuff from http://doc.norang.ca/org-mode.html

;; TODO: fail gracefully
(defun sanityinc/grab-ditaa (url jar-name)
  "Download URL and extract JAR-NAME as `org-ditaa-jar-path'."
  ;; TODO: handle errors
  (message "Grabbing %s for org." jar-name)
  (let ((zip-temp (make-temp-name "emacs-ditaa")))
    (unwind-protect
        (progn
          (when (executable-find "unzip")
            (url-copy-file url zip-temp)
            (shell-command (concat "unzip -p " (shell-quote-argument zip-temp)
                                   " " (shell-quote-argument jar-name) " > "
                                   (shell-quote-argument org-ditaa-jar-path)))))
      (when (file-exists-p zip-temp)
        (delete-file zip-temp)))))

(after-load 'ob-ditaa
  (unless (and (boundp 'org-ditaa-jar-path)
               (file-exists-p org-ditaa-jar-path))
    (let ((jar-name "ditaa0_9.jar")
          (url "http://jaist.dl.sourceforge.net/project/ditaa/ditaa/0.9/ditaa0_9.zip"))
      (setq org-ditaa-jar-path (expand-file-name jar-name (file-name-directory user-init-file)))
      (unless (file-exists-p org-ditaa-jar-path)
        (sanityinc/grab-ditaa url jar-name)))))

(after-load 'ob-plantuml
  (let ((jar-name "plantuml.jar")
        (url "http://jaist.dl.sourceforge.net/project/plantuml/plantuml.jar"))
    (setq org-plantuml-jar-path (expand-file-name jar-name (file-name-directory user-init-file)))
    (unless (file-exists-p org-plantuml-jar-path)
      (url-copy-file url org-plantuml-jar-path))))


;; Re-align tags when window shape changes
(after-load 'org-agenda
  (add-hook 'org-agenda-mode-hook
            (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t))))




(maybe-require-package 'writeroom-mode)

(define-minor-mode prose-mode
  "Set up a buffer for prose editing.
This enables or modifies a number of settings so that the
experience of editing prose is a little more like that of a
typical word processor."
  nil " Prose" nil
  (if prose-mode
      (progn
        (when (fboundp 'writeroom-mode)
          (writeroom-mode 1))
        (setq truncate-lines nil)
        (setq word-wrap t)
        (setq cursor-type 'bar)
        (when (eq major-mode 'org)
          (kill-local-variable 'buffer-face-mode-face))
        (buffer-face-mode 1)
        ;;(delete-selection-mode 1)
        (set (make-local-variable 'blink-cursor-interval) 0.6)
        (set (make-local-variable 'show-trailing-whitespace) nil)
        (set (make-local-variable 'line-spacing) 0.2)
        (set (make-local-variable 'electric-pair-mode) nil)
        (ignore-errors (flyspell-mode 1))
        (visual-line-mode 1))
    (kill-local-variable 'truncate-lines)
    (kill-local-variable 'word-wrap)
    (kill-local-variable 'cursor-type)
    (kill-local-variable 'blink-cursor-interval)
    (kill-local-variable 'show-trailing-whitespace)
    (kill-local-variable 'line-spacing)
    (kill-local-variable 'electric-pair-mode)
    (buffer-face-mode -1)
    ;; (delete-selection-mode -1)
    (flyspell-mode -1)
    (visual-line-mode -1)
    (when (fboundp 'writeroom-mode)
      (writeroom-mode 0))))

;;(add-hook 'org-mode-hook 'buffer-face-mode)


(setq org-support-shift-select t)

;;; Capturing

(global-set-key (kbd "C-c c") 'org-capture)

;;(setq org-capture-templates
;;      `(("t" "todo" entry (file "")  ; "" => `org-default-notes-file'
;;         "* NEXT %?\n%U\n" :clock-resume t)
;;        ("n" "note" entry (file "")
;;         "* %? :NOTE:\n%U\n%a\n" :clock-resume t)
;;        ))



;;; Refiling

(setq org-refile-use-cache nil)

;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

(after-load 'org-agenda
  (add-to-list 'org-agenda-after-show-hook 'org-show-entry))

(advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

;; Exclude DONE state tasks from refile targets
(defun sanityinc/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'sanityinc/verify-refile-target)

(defun sanityinc/org-refile-anywhere (&optional goto default-buffer rfloc msg)
  "A version of `org-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-refile goto default-buffer rfloc msg)))

(defun sanityinc/org-agenda-refile-anywhere (&optional goto rfloc no-update)
  "A version of `org-agenda-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-agenda-refile goto rfloc no-update)))

;; Targets start with the file name - allows creating level 1 tasks
;;(setq org-refile-use-outline-path (quote file))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)


;;; To-do settings

;; 多状态工作流程
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
              (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
      org-todo-repeat-to-state "NEXT")

(setq org-todo-keyword-faces
      (quote (("NEXT" :inherit warning)
              ("PROJECT" :inherit font-lock-string-face))))


;;; Agenda views

(setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))


(let ((active-project-match "-INBOX/PROJECT"))

  (setq org-stuck-projects
        `(,active-project-match ("NEXT")))

  (setq org-agenda-compact-blocks t
        org-agenda-sticky t
        org-agenda-start-on-weekday nil
        org-agenda-span 'day
        org-agenda-include-diary nil
        org-agenda-sorting-strategy
        '((agenda habit-down time-up user-defined-up effort-up category-keep)
          (todo category-up effort-up)
          (tags category-up effort-up)
          (search category-up))
        org-agenda-window-setup 'current-window
        org-agenda-custom-commands
        `(("N" "Notes" tags "NOTE"
           ((org-agenda-overriding-header "Notes")
            (org-tags-match-list-sublevels t)))
          ("g" "GTD"
           ((agenda "" nil)
            (tags "INBOX"
                  ((org-agenda-overriding-header "Inbox")
                   (org-tags-match-list-sublevels nil)))
            (stuck ""
                   ((org-agenda-overriding-header "Stuck Projects")
                    (org-agenda-tags-todo-honor-ignore-options t)
                    (org-tags-match-list-sublevels t)
                    (org-agenda-todo-ignore-scheduled 'future)))
            (tags-todo "-INBOX"
                       ((org-agenda-overriding-header "Next Actions")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
                                (org-agenda-skip-entry-if 'nottodo '("NEXT")))))
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(todo-state-down effort-up category-keep))))
            (tags-todo ,active-project-match
                       ((org-agenda-overriding-header "Projects")
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-INBOX/-NEXT"
                       ((org-agenda-overriding-header "Orphaned Tasks")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("PROJECT" "HOLD" "WAITING" "DELEGATED"))
                                (org-agenda-skip-subtree-if 'nottododo '("TODO")))))
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "/WAITING"
                       ((org-agenda-overriding-header "Waiting")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "/DELEGATED"
                       ((org-agenda-overriding-header "Delegated")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-INBOX"
                       ((org-agenda-overriding-header "On Hold")
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("WAITING"))
                                (org-agenda-skip-entry-if 'nottodo '("HOLD")))))
                        (org-tags-match-list-sublevels nil)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            ;; (tags-todo "-NEXT"
            ;;            ((org-agenda-overriding-header "All other TODOs")
            ;;             (org-match-list-sublevels t)))
            )))))


(add-hook 'org-agenda-mode-hook 'hl-line-mode)

;;; Org clock

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(after-load 'org
  (org-clock-persistence-insinuate))
(setq org-clock-persist t)
(setq org-clock-in-resume t)

;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Save state changes in the LOGBOOK drawer
(setq org-log-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Show clock sums as hours and minutes, not "n days" etc.
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))



;;; Show the clocked-in task - if any - in the header line
(defun sanityinc/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun sanityinc/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
(add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
(add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

(after-load 'org-clock
  (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
  (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))


(when (and *is-a-mac* (file-directory-p "/Applications/org-clock-statusbar.app"))
  (add-hook 'org-clock-in-hook
            (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                (concat "tell application \"org-clock-statusbar\" to clock in \"" org-clock-current-task "\""))))
  (add-hook 'org-clock-out-hook
            (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                "tell application \"org-clock-statusbar\" to clock out"))))

;; bullets
(require-package 'org-bullets)
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;;   org-indent mode
(setq org-startup-indented t)

;;   bullet list
(setq org-bullets-bullet-list '("☰" "☷" "☯" "☭"))


;; TODO: warn about inconsistent items, e.g. TODO inside non-PROJECT
;; TODO: nested projects!


;;; Archiving

(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archive")

;;(defun sourcod-org-packages/post-init-org()
;;  (progn
;;    (require 'ox-publish)
;;    ))

(setq org-path "/Users/sourcod/workspace/org/")
(setq org-note-path  (concat org-path "notes/"))


(setq org-default-notes-file (concat org-path "inbox.org"))

;;(setq org-capture-templates
;;      '(("t" "Todo" entry (file+headline ("~/workspace/org/gtd.org") "Tasks")
;;         "* TODO [#B] %?\n  %i\n"
;;         :empty-lines 1
;;         )
;;        ("tg" "Todo1" entry (file+headline "~/workspace/org/gtd.org" "Tasks")
;;         "* TODO [#B] %?\n  %i\n"
;;         :empty-lines 1
;;         )
;;        ("n" "notes" entry (file+headline "~/workspace/org/notes.org" "Quick notes")
;;         "* %?\n  %i\n %U"
;;         :empty-lines 1
;;         )
;;        ("B" "Blog Ideas" entry (file+headline "~/workspace/org/notes.org" "Blog Ideas")
;;         "* TODO [#B] %?\n  %i\n %U"
;;         :empty-lines 1
;;         )
;;        ("s" "Code Snippet" entry (file "~/workspace/org/snippets.org")
;;         "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
;;
;;        ("k" "tlkj" entry (file+headline "~/workspace/org/gtd.org" "tlkj")
;;         "* TODO [#B] %?\n  %i\n %U"
;;         :empty-lines 1)
;;        ("w" "work" entry (file+headline "~/workspace/org/gtd.org" "work")
;;         "* TODO [#B] %?\n  %i\n %U"
;;         :empty-lines 1)
;;        ("l" "links" entry (file+headline "~/workspace/org/notes.org" "link notes")
;;         "* TODO [#C] %?\n  %i\n %a \n %U"
;;         :empty-lines 1)
;;        ("j" "Journal Entry"
;;         entry (file+datetree "~/workspace/org/journal.org")
;;         "* %?"
;;         :empty-lines 1)
;;        ("b" "Books" entry (file+headline "~/workspace/org/books.org" "book notes")
;;         "* TODO [#D] %?\n  %i\n %U"
;;         :empty-lines 1)
;;        ))
;; billing
(defun get-year-and-month ()
  (list (format-time-string "%Y年") (format-time-string "%m月")))


(defun find-month-tree ()
  (let* ((path (get-year-and-month))
         (level 1)
         end)
    (unless (derived-mode-p 'org-mode)
      (error "Target buffer \"%s\" should be in Org mode" (current-buffer)))
    (goto-char (point-min))             ;移动到 buffer 的开始位置
    ;; 先定位表示年份的 headline，再定位表示月份的 headline
    (dolist (heading path)
      (let ((re (format org-complex-heading-regexp-format
                        (regexp-quote heading)))
            (cnt 0))
        (if (re-search-forward re end t)
            (goto-char (point-at-bol))  ;如果找到了 headline 就移动到对应的位置
          (progn                        ;否则就新建一个 headline
            (or (bolp) (insert "\n"))
            (if (/= (point) (point-min)) (org-end-of-subtree t t))
            (insert (make-string level ?*) " " heading "\n"))))
      (setq level (1+ level))
      (setq end (save-excursion (org-end-of-subtree t t))))
    (org-end-of-subtree)))

;; protocol
(require 'org-protocol)

;; dynamic href
(setq org-capture-templates
      `(("c" "Contacts" table-line (file ,(concat org-path "contact.org"))
         "| %U | %^{姓名} | %^{主手机号} | %^{次手机号} | %^{邮箱} | %^{公司} | %^{标签} | %^{备注} | %^{生日} |")
        ("t" "GTD")
        ("ta" "Tasks" entry (file+headline ,(concat org-path "gtd.org") "Tasks")
         "* TODO [#B] %?\n  %i\n"
         :empty-lines 1
         )
        ("tw" "work" entry (file+headline ,(concat org-path "gtd.org") "work")
         "* TODO [#B] %?\n %i\n"
         :empty-lines 1
         )
        ;; 临时笔记
        ("n" "notes")
        ("nc" "名言警句" entry (file+headline ,(concat org-path "notes.org") "catchphrase")
         "* %?\n  %i\n %U"
         :empty-lines 1
         )
        ("nn" "notes" entry (file+headline ,(concat org-path "notes.org") "Quick notes")
         "* %?\n  %i\n %U"
         :empty-lines 1
         )
        ;; 记账
        ("i" "Billing" table-line (file+datetree+prompt ,(concat org-path "billing.org"))
         "| %U | %^{类别} | %^{描述} | %^{金额} | " :kill-buffer t
         :empty-lines 1
         )
        ("B" "create blog" plain (file ,(concat org-path "blog/" (format-time-string "%Y-%m-%d.org")))
         ,(concat "#+TITLE: %^{标题}\n"
                  "#+TAGS: %^{标签}\n"
                  "#+SETUPFILE: index.org\n"
                  "#+EMAIL: zhaochunjie@sourcod.com\n"
                  "#+AUTHOR: willeam\n"
                  "#+HTML: <div class=outline-2 id=\"meta\">\n"
                  "| Author | {{{author}}} ({{{email}}})    |\n"
                  "| Date   | {{{time(%Y-%m-%d %H:%ML%S)}}} |\n"
                  "#+HTML: </div>\n"
                  "#+options: ^:{}\n"
                  "#+options: \\n:t\n"
                  "#+TOC: headlines 3\n")
         )
        ;; 代码片段
        ("s" "Code Snippet" entry (file ,(concat org-path "snippets.org"))
         "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
        ;; 工作
        ("w" "work" entry (file+headline ,(concat org-path "gtd.org") "work")
         "* TODO [#B] %?\n  %i\n %U"
         :empty-lines 1)
        ;; 连接收藏夹
        ("l" "links" entry (file ,(concat org-path "link.org"))
         "* TODO [#C] %?\n  %i\n %a \n %U"
         :empty-lines 1)
        ;; 日记
        ("j" "Journal Entry"
         entry (file+datetree ,(concat org-path "journal.org"))
         "* %U | 标题 -> %^{标题} | 天气 -> %^{天气} | 心情 -> %^{心情}\n%?"
         :empty-lines 1
         )
        ;; 书
        ("b" "Books" entry (file+headline ,(concat org-path "books.org") "book notes")
         "* TODO [#D] %?\n  %i\n %U"
         :empty-lines 1)
        ;; protocol

        ;; projects
        ("p" "projects")
        ;; 天蓝科技
        ("pt" "天蓝科技")
        ("pt1" "zdqdp" entry (file+olp ,(concat org-path "project.org") "天蓝科技" "自动抢订票")
         "* TODO [D] %?")

        ("pz" "自己")
        ("pz1" "WXBootstrap" entry (file+olp ,(concat org-path "project.org") "my" "WXBootstrap")
         "* TODO [D] %?")


        ("pw" "外部")
        ("pw2" "富胜科技" entry (file+olp ,(concat org-path "project.org") "富胜科技") "* TODO [D] %?")

        ("pw1" "古联")
        ;; gulian
        ("pw11" "gulian-app" entry (file+olp ,(concat org-path "project.org") "gulian" "app")
         "* TODO [D] %?")
        ;; gulian
        ("pw12" "gulian-wx" entry (file+olp ,(concat org-path "project.org") "gulian" "wx")
         "* TODO [D] %?")
        ("P" "Password" entry (file ,(concat org-path "password.org.cpt"))
         "* %U - %^{title} %^G\n\n  - 用户名: %^{用户名}\n  - 密码: %(get-or-create-password)"
         :empty-lines 1
         :kill-buffer t
         )
        )

      )

;; templete
;; ("pw11" "gulian-app" entry (file+olp ,(concat org-path "project.org") "gulian" "app") "* TODO [D] %?")


(setq org-agenda-custom-commands
      '(
        ("w" . "任务安排")
        ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
        ("wb" "重要且不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
        ("wc" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")
        ("wd" "不重要且不紧急的任务" tags-todo "+PRIORITY=\"D\"")
        ("b" "Blog" tags-todo "BLOG")

        ("p" . "项目安排")

        ;; 外部项目
        ("pg" tags-todo "PROJECT+WORK+CATEGORY=\"gulian\"")
        ("pga" tags-todo "PROJECT+WORK+CATEGORY=\"gulian-wx\"")
        ("pgw" tags-todo "PROJECT+WORK+CATEGORY=\"gulian-app\"")

        ;; tlkj
        ("pt" "天蓝科技" tags-todo "PROJECT+WORK+CATEGORY=\"tlkj\"")
        ("pt1" "自动抢订票" tags-todo "PROJECT+WORK+CATEGORY=\"tlkj-zdqdp\"")
        ("pt2" "codframe框架" tags-todo "PROJECT+WORK+CATEGORY=\"tlkj-codframe\"")
        ("pt3" "爬虫项目" tags-todo "PROJECT+WORK+CATEGORY=\"tlkj-crawler\"")

        ;; link
        ("pl" tags-todo "PROJECT+DREAM+CATEGORY=\"willeam\"")
        ("W" "Weekly Review"
         ((stuck "") ;; review stuck projects as designated by org-stuck-projects
          (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
          ))))

;; find org-note
(setq org-agenda-files (list
                        ;; list 转string

                        (mapconcat 'identity (directory-files (concat org-path "notes") t "\.org$")
                                   ",")
                        (mapconcat 'identity (directory-files (concat org-path "journal") t "\.org$")
                                   ",")
                        (mapconcat 'identity (directory-files (concat org-path "gtd") t "\.org$")
                                   ",")
                        (mapconcat 'identity (directory-files (concat org-path "books") t "\.org$")
                                   ",")
                        (mapconcat 'identity (directory-files (concat org-path "other") t "\.org$")
                                   ",")

                        (concat org-path "gtd.org")
                        (concat org-path "notes.org")
                        (concat org-path "books.org")
                        (concat org-path "journal.org")))
;; push
;;(setq org-agenda-files ((push
;;                         (mapconcat 'identity
;;                                    (directory-files (concat org-path "notes") t "\.org$")
;;                                    ",")
;;                         normal)
;;                        ))

;;(setq org-agenda-files (list "~/workspace/org/gtd.org"
;;                             "~/workspace/org/notes.org"
;;                             "~/workspace/org/books.org"
;;                             "~/workspace/org/journal.org"
;;                             ))


;; Screenshot
;; 截图
(defun sourcod/capture-screenshot (basename)
  "Take a screenshot into a time stamped unique-named file in the
  same directory as the org-buffer/markdown-buffer and insert a link to this file."
  (interactive "sScreenshot name: ")
  (if (equal basename "")
      (setq basename (format-time-string "%Y%m%d_%H%M%S")))
  (setq fullpath
        ;; (file-name-directory (buffer-file-name)
        (concat
         (expand-file-name "~/workspace/org/images/")
         (file-name-base (buffer-file-name))
         "_"
         basename))
  (setq relativepath
        (concat (file-name-base (buffer-file-name))
                "_"
                basename
                ".png"))
  (if (file-exists-p (file-name-directory fullpath))
      (progn
        (setq final-image-full-path (concat fullpath ".png"))
        (call-process "screencapture" nil nil nil "-s" final-image-full-path)
        (if (executable-find "convert")
            (progn
              (setq resize-command-str (format "convert %s -resize 800x600 %s" final-image-full-path final-image-full-path))
              (shell-command-to-string resize-command-str)))
        (sourcod//insert-org-or-md-img-link "http://image.sourcod.com/hexo/" relativepath))
    (progn
      (call-process "screencapture" nil nil nil "-s" (concat basename ".png"))
      (sourcod//insert-org-or-md-img-link "./" (concat basename ".png"))))
  (insert "\n"))

;; 截图并最小画
(defun sourcod/capture-screenshot-min (basename)
  "Take a screenshot into a time stamped unique-named file in the
  same directory as the org-buffer/markdown-buffer and insert a link to this file."
  (interactive "sScreenshot name: ")
  (if (equal basename "")
      (setq basename (format-time-string "%Y%m%d_%H%M%S")))
  (setq fullpath
        ;; (file-name-directory (buffer-file-name)
        (concat
         (expand-file-name "~/workspace/org/images/")
         (file-name-base (buffer-file-name))
         "_"
         basename))
  (setq relativepath
        (concat (file-name-base (buffer-file-name))
                "_"
                basename
                ".png"))
  (if (file-exists-p (file-name-directory fullpath))
      (progn
        (setq final-image-full-path (concat fullpath ".png"))
        ;;(iconify-or-deiconify-frame)
        ;; appleScript mini Item
        (shell-command "/usr/bin/osascript  ~/workspace/scriptProjects/applescript/minimizeIterm.scpt")
        (call-process "screencapture" nil nil nil "-s" final-image-full-path)
        (if (executable-find "convert")
            (progn
              (setq resize-command-str (format "convert %s -resize 800x600 %s" final-image-full-path final-image-full-path))
              (shell-command-to-string resize-command-str)))
        (sourcod//insert-org-or-md-img-link "http://image.sourcod.com/hexo/" relativepath))
    (progn
      (call-process "screencapture" nil nil nil "-s" (concat basename ".png"))
      (sourcod//insert-org-or-md-img-link "./" (concat basename ".png"))))
  (insert "\n")
  (shell-command "/usr/bin/osascript  ~/workspace/scriptProjects/applescript/maximizeIterm.scpt")
  )

(defun sourcod//insert-org-or-md-img-link (prefix imagename)
  (if (equal (file-name-extension (buffer-file-name)) "org")
      (insert (format "[[%s%s]]" prefix imagename))
    (insert (format "![%s](%s%s)" imagename prefix imagename))))

(global-set-key (kbd "C-x C-a") 'sourcod/capture-screenshot)


;; 发布
;;(require 'org-publish)

(setq org-publish-project-alist
      '(("org-notes"
         :base-directory "~/workspace/org/blog"
         :publishing-directory "~/workspace/org/blog/publish"
         :section-numbers nil
         :recursive t
         :publishing-function org-html-publish-to-html
         ;;org-html-publish-to-html
         :headline-levels 4
         :table-of-contents nil
         :style "<link rel=\"stylesheet\" href=\"css/style.css\"  type=\"text/css\"/>"
         :html-head "<link rel=\"stylesheet\" href=\"css/style.css\"  type=\"text/css\"/>"
         :author "willeam"
         :email "zhaochunjie@sourcod.com"
         :auto-sitemap t
         ;;:sitemap-filename "sitemap.org"
         ;;:sitemap-title "我的wiki"
         ;;:sitemap-sort-files anti-chronologically
         ;;:sitemap-file-entry-format "%t" ; %d to output date, we don't need date here
         )
        ("org-static"
         :base-directory "~/workspace/org/blog"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf|doc"
         :publishing-directory "~/workspace/org/blog/publish"
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("org" :components ("org-notes" "org-static"))

        ;;
        ;;("org-51xny-notes"
        ;; :base-directory "~/workspace/org/blog"
        ;; :publishing-directory "~/workspace/org/blog/publish"
        ;; :section-numbers nil
        ;; :recursive t
        ;; :publishing-function org-html-publish-to-html
        ;; ;;org-html-publish-to-html
        ;; :headline-levels 4
        ;; :table-of-contents nil
        ;; :style "<link rel=\"stylesheet\" href=\"css/style.css\"  type=\"text/css\"/>"
        ;; :html-head "<link rel=\"stylesheet\" href=\"css/style.css\"  type=\"text/css\"/>"
        ;; :author "willeam"
        ;; :email "zhaochunjie@sourcod.com"
        ;; :auto-sitemap t
        ;; ;;:sitemap-filename "sitemap.org"
        ;; ;;:sitemap-title "我的wiki"
        ;; ;;:sitemap-sort-files anti-chronologically
        ;; ;;:sitemap-file-entry-format "%t" ; %d to output date, we don't need date here
        ;; )
        ;;("org-51xny-static"
        ;; :base-directory "~/workspace/org/blog"
        ;; :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf|doc"
        ;; :publishing-directory "~/workspace/org/blog/publish"
        ;; :recursive t
        ;; :publishing-function org-publish-attachment
        ;; )
        ;;("org-51xny" :components ("org-51xny-notes" "org-51xny-static"))
        )
      )

;; org-crypt
;;(require-package 'org-crypt)
(require 'org-crypt)

;; 當被加密的部份要存入硬碟時，自動加密回去
(org-crypt-use-before-save-magic)

;; 設定要加密的 tag 標籤為 secret
(setq org-crypt-tag-matcher "secret")

;; 避免 secret 這個 tag 被子項目繼承 造成重複加密
;; (但是子項目還是會被加密喔)
(setq org-tags-exclude-from-inheritance (quote ("secret")))

;; 用於加密的 GPG 金鑰
;; 可以設定任何 ID 或是設成 nil 來使用對稱式加密 (symmetric encryption)
(setq org-crypt-key "393B76F8FD82DC7B7A5E79AB3251A10218FB9FDB")
;;(setq org-crypt-key "nil")

(setq org-duration-format (quote h:mm))

(require-package 'org-pomodoro)
(setq org-pomodoro-keep-killed-pomodoro-time t)
(after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro))


;; ;; Show iCal calendars in the org agenda
;; (when (and *is-a-mac* (require 'org-mac-iCal nil t))
;;   (setq org-agenda-include-diary t
;;         org-agenda-custom-commands
;;         '(("I" "Import diary from iCal" agenda ""
;;            ((org-agenda-mode-hook #'org-mac-iCal)))))

;;   (add-hook 'org-agenda-cleanup-fancy-diary-hook
;;             (lambda ()
;;               (goto-char (point-min))
;;               (save-excursion
;;                 (while (re-search-forward "^[a-z]" nil t)
;;                   (goto-char (match-beginning 0))
;;                   (insert "0:00-24:00 ")))
;;               (while (re-search-forward "^ [a-z]" nil t)
;;                 (goto-char (match-beginning 0))
;;                 (save-excursion
;;                   (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
;;                 (insert (match-string 0))))))


(after-load 'org
  (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
  (when *is-a-mac*
    (define-key org-mode-map (kbd "M-h") nil)
    (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))

(after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   `((R . t)
     (ditaa . t)
     (dot . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (haskell . nil)
     (latex . t)
     (ledger . t)
     (ocaml . nil)
     (octave . t)
     (plantuml . t)
     (python . t)
     (ruby . t)
     (screen . nil)
     (,(if (locate-library "ob-sh") 'sh 'shell) . t)
     (sql . nil)
     (sqlite . t))))


(provide 'init-org)
;;; init-org.el ends here
