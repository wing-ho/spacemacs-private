;;; packages.el --- wing Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(defconst wing-packages
      '(
        org
        blog-admin
        (ob-redis :location (recipe
                               :fetcher github
                               :repo "stardiviner/ob-redis"))
        ))

;; List of packages to exclude.
(setq wing-excluded-packages '())

;; For each package, define a function wing/init-<package-name>

(defun wing/init-blog-admin ()
  (use-package blog-admin
    :defer t
    :commands blog-admin-start
    :init
    (progn
      (setq blog-admin-backend-path "~/Blog")
      (setq blog-admin-backend-type 'hexo)
      (setq blog-admin-backend-new-post-in-drafts t) ;; create new post in drafts by default
      (setq blog-admin-backend-new-post-with-same-name-dir t)
      )))
(defun wing/init-ob-redis ()
  (use-package ob-redis
    :defer t
      ))
(defun wing/post-init-org ()
  (with-eval-after-load 'org
    ;; FIXME: workaround
    ;; https://github.com/syl20bnr/spacemacs/issues/11798
    (when (version<= "9.2" (org-version))
      (require 'org-tempo))
    (setq org-babel--insert-results-keyword nil)
    (setq org-ditaa-jar-path (expand-file-name "ditaa.jar" dotspacemacs-directory))
    (setq org-plantuml-jar-path (expand-file-name "plantuml.jar" dotspacemacs-directory))
    (setq org-startup-with-inline-images nil)
    (setq org-image-actual-width '(600))
    (setq org-src-fontify-natively t)
    (setq org-superstar-headline-bullets-list '("◉" "○" "■" "◆" "▲" "▶"))
    (setq org-confirm-babel-evaluate nil)
    ;;org文档中插入代码支持那种语言
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((C . t)
       (redis .t)
       (ditaa . t)
       (plantuml . t)
       (dot . t)
       (http . t)
       (java . t)
       (js . t)
       (lisp . t)
       (shell . t)
       (python . t)
       (ruby . t)
       (awk . t)
       (R . t)
       (calc . t)
       (perl . t)
       (sql . t)))
    (with-eval-after-load 'org-download
      (defun org-download-annotate-default (link)
        "Annotate LINK with the time of download."
        (format "#+DOWNLOADED: %s @ %s"
                (org-link-unescape link)
                (format-time-string "%Y-%m-%d %H:%M:%S")))
      (defun org-download-dnd (uri action)
        "When in `org-mode' and URI points to image, download it.
Otherwise, pass URI and ACTION back to dnd dispatch."
        (cond ((eq major-mode 'org-mode)
               (condition-case nil
                   (org-download-image uri)
                 (error
                  (org-download-dnd-fallback uri action))))
              ((eq major-mode 'markdown-mode)
               (condition-case nil
                   (org-download-image uri)
                 (error
                  (org-download-dnd-fallback uri action))))
              ((eq major-mode 'dired-mode)
               (org-download-dired uri))
              ;; redirect to someone else
              (t
               (org-download-dnd-fallback uri action))))
      (defun my-org-download-method  (link)
        (let ((filename
               (file-name-nondirectory
                (car (url-path-and-query
                      (url-generic-parse-url (org-link-unescape link))))))
              (dirname (file-name-sans-extension (buffer-name)) ))
          ;; if directory not exist, create it
          (unless (file-exists-p dirname)
            (make-directory dirname))
          ;; return the path to save the download files
          (expand-file-name filename dirname))
        )
      (setq org-download-method 'my-org-download-method)
      )
    ;;(add-hook 'markdown-mode-hook 'org-download-enable)
    (add-hook 'markdown-mode-hook (lambda()
                                    (setq-local org-download-link-format "![](%s)")
                                    (org-download-enable)
                                    ))
    ;;在org-mode中自动换行
    (add-hook 'org-mode-hook (lambda ()
                               (setq truncate-lines nil)))
    (with-eval-after-load "ob-java"
      (defun org-babel-execute:java (body params)
        (let* ((classname (or (cdr (assq :classname params))
                              (error
                               "Can't compile a java block without a classname")))
               (packagename (file-name-directory classname))
               (src (if  (assq :src params) (concat (cdr(assq :src params)) "/") ""))
               (src-file (concat src classname ".java"))
               (cmpflag (or (cdr (assq :cmpflag params)) ""))
               (cmdline (or (cdr (assq :cmdline params)) ""))
               (full-body (org-babel-expand-body:generic body params)))
          (with-temp-file src-file (insert full-body))
          (org-babel-eval
           (concat org-babel-java-compiler " " cmpflag " " src-file) "")
          ;; created package-name directories if missing
          (unless (or (not packagename) (file-exists-p packagename))
            (make-directory packagename 'parents))
          (let ((results (org-babel-eval (concat org-babel-java-command
                                                 " " cmdline " " classname) "")))
            (org-babel-reassemble-table
             (org-babel-result-cond (cdr (assq :result-params params))
               (org-babel-read results)
               (let ((tmp-file (org-babel-temp-file "c-")))
                 (with-temp-file tmp-file (insert results))
                 (org-babel-import-elisp-from-file tmp-file)))
             (org-babel-pick-name
              (cdr (assq :colname-names params)) (cdr (assq :colnames params)))
             (org-babel-pick-name
              (cdr (assq :rowname-names params)) (cdr (assq :rownames params)))))))
      )
    (setq org-export-backends (quote (ascii html icalendar latex md)))
    (setq org-agenda-dir "~/Notes")
    (setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
    (setq org-agenda-file-inbox (expand-file-name "inbox.txt" org-agenda-dir))

    (setq org-agenda-files `(,org-agenda-file-gtd ,org-agenda-file-inbox))

    (setq org-default-notes-file org-agenda-file-gtd)
    (setq org-todo-keywords
          '((sequence "INBOX(i)" "TODO(t)" "|" "WAITTING(w)" "NOTE(n)""DONE(d)")
            (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
            (sequence "|" "CANCELED(c)")))

    (setq org-refile-targets
          '(("gtd.org" :maxlevel . 1)))

    (setq org-log-into-drawer t)

    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline org-agenda-file-gtd "Daily Tasks")
             "* TODO %?\n  %i\n"
             :empty-lines 1)
            ("i" "Inbox" entry (file+headline org-agenda-file-inbox "Inbox")
             "* INBOX %?\n  %i\n"
             :empty-lines 1)
            ("n" "Quick Notes" entry (file+headline org-agenda-file-gtd "Quick notes")
             "* NOTE %?\n  %i\n %U"
             :empty-lines 1)
            ("b" "Blog Ideas" entry (file+headline org-agenda-file-gtd "Blog Ideas")
             "* TODO %?\n  %i\n %U"
             :empty-lines 1)
            ("w" "work" entry (file+headline org-agenda-file-gtd "Programming")
             "* TODO %?\n  %i\n %U"
             :empty-lines 1)
            ("j" "Journal Entry"
             entry (file+datetree "~/KuaiPan/org-notes/journal.org")
             "* %?"
             :empty-lines 1)))

    (setq org-agenda-custom-commands
          '(
            ("i" "Inbox" todo "INBOX")
            ("w" . " 任务安排 ")
            ("wa" " 重要且紧急的任务 " tags-todo "+PRIORITY=\"A\"")
            ("wb" " 重要且不紧急的任务 " tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
            ("wc" " 不重要且紧急的任务 " tags-todo "+PRIORITY=\"C\"")
            ("b" "Blog" tags-todo "BLOG")
            ("p" . " 项目安排 ")
            ("pw" tags-todo "PROJECT+WORK+CATEGORY=\"programming\"")
            ("pl" tags-todo "PROJECT+DREAM+CATEGORY=\"wing\"")
            ("W" "Weekly Review"
             ((stuck "")            ;; review stuck projects as designated by org-stuck-projects
              (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
              ))))

    (defun org-summary-todo (n-done n-not-done)
      "Switch entry to DONE when all subentries are done, to TODO otherwise."
      (let (org-log-done org-log-states)  ; turn off logging
        (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

    (add-hook'org-after-todo-statistics-hook 'org-summary-todo)
    ;; used by org-clock-sum-today-by-tags
    (defun filter-by-tags ()
      (let ((head-tags (org-get-tags-at)))
        (member current-tag head-tags)))

    (defun org-clock-sum-today-by-tags (timerange &optional tstart tend noinsert)
      (interactive "P")
      (let* ((timerange-numeric-value (prefix-numeric-value timerange))
             (files (org-add-archive-files (org-agenda-files)))
             (include-tags'("PROG" "EMACS" "DREAM" "WRITING" "MEETING" "BLOG"
                            "LIFE" "PROJECT"))
             (tags-time-alist (mapcar (lambda (tag) `(,tag . 0)) include-tags))
             (output-string "")
             (tstart (or tstart
                         (and timerange (equal timerange-numeric-value 4) (- (org-time-today) 86400))
                         (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "Start Date/Time:"))
                         (org-time-today)))
             (tend (or tend
                       (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "End Date/Time:"))
                       (+ tstart 86400)))
             h m file item prompt donesomething)
        (while (setq file (pop files))
          (setq org-agenda-buffer (if (file-exists-p file)
                                      (org-get-agenda-file-buffer file)
                                    (error "No such file %s" file)))
          (with-current-buffer org-agenda-buffer
            (dolist (current-tag include-tags)
              (org-clock-sum tstart tend'filter-by-tags)
              (setcdr (assoc current-tag tags-time-alist)
                      (+ org-clock-file-total-minutes (cdr (assoc current-tag tags-time-alist)))))))
        (while (setq item (pop tags-time-alist))
          (unless (equal (cdr item) 0)
            (setq donesomething t)
            (setq h (/ (cdr item) 60)
                  m (- (cdr item) (* 60 h)))
            (setq output-string (concat output-string (format "[-%s-] %.2d:%.2d\n" (car item) h m)))))
        (unless donesomething
          (setq output-string (concat output-string "[-Nothing-] Done nothing!!!\n")))
        (unless noinsert
          (insert output-string))
        output-string))

    ;; Resume clocking task when emacs is restarted
    (org-clock-persistence-insinuate)
    ;; Save the running clock and all clock history when exiting Emacs, load it on startup
    (setq org-clock-persist t)
    ;; Do not prompt to resume an active clock
    (setq org-clock-persist-query-resume nil)
    ))
