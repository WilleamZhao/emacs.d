;; ========================================================================
;; Config for Checking by Date
;; ========================================================================

(add-to-list 'org-agenda-custom-commands
             `("r" . "按日期及状态查看...") t)

(add-to-list 'org-agenda-custom-commands
             `("rq" "Qingdao Project All Tasks (grouped by Due Date)"
               (
                (tags-todo "DEADLINE<\"<+0d>\""
                           ((org-agenda-overriding-header "OVERDUE")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))))
                (tags-todo "DEADLINE=\"<+0d>\""
                           ((org-agenda-overriding-header "DUE TODAY")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))))
                (tags-todo "DEADLINE=\"<+1d>\""
                           ((org-agenda-overriding-header "DUE TOMORROW")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))))
                (tags-todo "DEADLINE>\"<+1d>\"+DEADLINE<=\"<+7d>\""
                           ((org-agenda-overriding-header "DUE WITHIN A WEEK")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))))
                (tags-todo "DEADLINE>\"<+7d>\"+DEADLINE<=\"<+28d>\""
                           ((org-agenda-overriding-header "DUE WITHIN A MONTH")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))))
                (tags-todo "DEADLINE>\"<+28d>\""
                           ((org-agenda-overriding-header "DUE LATER")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'notdeadline))))


                ;; (todo ""
                ;;            ((org-agenda-overriding-header "NO DUE DATE")
                ;;             (org-agenda-skip-function
                ;;              '(org-agenda-skip-entry-if 'deadline))))
                (tags-todo "TODO={STRT}"
                           ((org-agenda-overriding-header "NO DUE DATE / STARTED")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'deadline))))
                (tags-todo "TODO<>{STRT\\|WAIT\\|SDAY}"
                           ((org-agenda-overriding-header "NO DUE DATE / NEXT")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'deadline))))
                (tags-todo "TODO={WAIT}"
                           ((org-agenda-overriding-header "NO DUE DATE / WAITING FOR")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'deadline))))
                (tags-todo "TODO={SDAY}"
                           ((org-agenda-overriding-header "NO DUE DATE / SOMEDAY")
                            (org-agenda-skip-function
                             '(org-agenda-skip-entry-if 'deadline)))))
               ((org-agenda-sorting-strategy '(priority-down time-down))
                ;;(org-agenda-files '("~/workspace/github/work-notes/qingdao-projects"))
                (org-agenda-write-buffer-name "All Tasks (grouped by Due Date)"))) t)

(setq org-agenda-custom-commands
      '(("g" . "GTD contexts")
        ("go" "Office" tags-todo "office")
        ("gc" "Computer" tags-todo "computer")
        ("gp" "Phone" tags-todo "phone")
        ("gh" "Home" tags-todo "home")
        ("ge" "Errands" tags-todo "errands")
        ("G" "GTD Block Agenda"
         ((tags-todo "office")
          (tags-todo "computer")
          (tags-todo "phone")
          (tags-todo "home")
          (tags-todo "errands"))
         nil                      ;; i.e., no local settings
         ("~/next-actions.html")) ;; exports block to this file with C-c a e
       ;; ..other commands here
        ))

(provide 'init-agenda)
