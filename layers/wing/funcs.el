(require 'cl-lib)
(require 'url-util)

(setq hexo-dir "~/Blog")


(defun wing/hexo-publish (commit-msg)
  "git add . & git commit & git push & hexo d -g"
  (interactive "Input commit message:")
  (async-shell-command (format "cd %s ;git add . ;git commit -m \"%s\" ;git push ;hexo d -g"
                               hexo-dir
                               commit-msg)))

(defun wing/org-babel-remove-header-args()
  (interactive)
  (org-babel-goto-src-block-head)
  (evil-find-char 1 (string-to-char ":"))
  (kill-line)
  )
(defun wing/save-chrome-session()
  "Reads Google Chrome current session and generate org-mode heading with items."
  (interactive)
  (save-excursion
    (insert (do-applescript "
set strResult to \"* \" & (do shell script \"date '+%Y-%m-%d %T'\") & \"\n\"
tell application \"Google Chrome\"
	repeat with theWindow in every window
		set theTabIndex to 0
		repeat with theTab in every tab of theWindow
			set theTabIndex to theTabIndex + 1
			set strResult to strResult & \"  - [[\" & theTab's URL & \"][\" & theTab's title & \"]]\n\"
		end repeat
	end repeat
end tell
return strResult
"))))
(defun wing/restore-chrome-session ()
  "Restore session, by openning each link in list with (browse-url).
  Make sure to put cursor on date heading that contains list of urls."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\*")
      (forward-line 1)
      (while (looking-at "^  - \\[\\[http[^]]*\\]\\[")
        (let ((ln (thing-at-point 'line t)))
          (when (string-match  "^  - \\[\\[\\([^]]*\\)\\]\\[" ln)
            (setq ln (match-string 1 ln))
            (browse-url ln)))
        (forward-line 1)))))
(defun wing/open-url-in-chrome (url)
  "Open URL in Google Chrome.  I use AppleScript to do several things:
  1. I tell Chrome to come to the front. If Chrome wasn't launched, this will also launch it.
  2. If Chrome has no windows open, I tell it to create one.
  3. If Chrome has a tab showing URL, I tell it to reload the tab, make that tab the active tab in its window, and bring its window to the front.
  4. If Chrome has no tab showing URL, I tell Chrome to make a new tab (in the front window) showing URL."
  (when (symbolp url)
    ; User passed a symbol instead of a string.  Use the symbol name.
    (setq url (symbol-name url)))
  (do-applescript (format "
tell application \"Google Chrome\"
	activate
	set theUrl to %S

	if (count every window) = 0 then
		make new window
	end if

	set found to false
	set theTabIndex to -1
	repeat with theWindow in every window
		set theTabIndex to 0
		repeat with theTab in every tab of theWindow
			set theTabIndex to theTabIndex + 1
			if theTab's URL = theUrl then
				set found to true
				exit
			end if
		end repeat

		if found then
			exit repeat
		end if
	end repeat

	if found then
		tell theTab to reload
		set theWindow's active tab index to theTabIndex
		set index of theWindow to 1
	else
		tell window 1 to make new tab with properties {URL:theUrl}
	end if
end tell
  " url)))

(defvar wing/open-in-chrome-url nil
  "*The URL that the wing/open-in-chrome function will send to Google Chrome.")

(defun wing/open-in-chrome (arg)
  "Open or reload a file in Google Chrome.  If you give me a prefix argument, I get Chrome's currently-displayed URL and save it for the future.  If you don't give me a prefix argument, I send the previously-saved URL to Chrome for reloading."
  (interactive "P")
  (cond
   (arg (setq wing/open-in-chrome-url (do-applescript "tell application \"Google Chrome\" to get window 1's active tab's URL")))
   ((not wing/open-in-chrome-url) (error "You haven't set a URL for me to send to the browser."))
   (t (save-buffer)
      (wing/open-url-in-chrome wing/open-in-chrome-url))))

(defun wing/run-tangle()
    (interactive)
    (let* (langs lang tangle-file filename ext dir)
      (setq langs '(
                    ("python" . "py")
                    ("shell" . "sh")
                    ("emacs-lisp" . "el")
                    ))
      ;; 获取code block的语言
      (setq lang (nth 0 (org-babel-get-src-block-info)))
      ;; 获取:tangle
      (setq tangle-file (cdr (assoc :tangle (nth 2 (org-babel-get-src-block-info))))) 
      ;; 如果:tangle等于no 则文件名为index.
      (if (string-equal tangle-file "no")
          (setq filename (concat "index."
                                 (or (cdr (assoc lang langs)) lang)))
        (setq filename tangle-file))
      (setq dir (concat (file-name-directory buffer-file-name) (nth 4 (org-heading-components))))
      (setq filename (expand-file-name filename dir))
      ;; 目录不存在时自动创建
      (unless (file-exists-p dir)
        (make-directory dir t))
      (org-babel-tangle '(4) filename)
      (when (string-equal lang "html")
        (wing/open-url-in-chrome (concat "file://" (url-encode-url filename)))
        )
      ;; (shell-command (concat "open -a \"Google Chrome\" " dir tangle-file))
      )
    )



;; copy from https://github.com/zilongshanren/spacemacs-private/blob/develop/layers%2Fzilongshanren%2Ffuncs.el
(defvar wc-regexp-chinese-char-and-punc
  (rx (category chinese)))
(defvar wc-regexp-chinese-punc
  "[。，！？；：「」『』（）、【】《》〈〉※—]")
(defvar wc-regexp-english-word
  "[a-zA-Z0-9-]+")

(defun wing/word-count-for-chinese ()
  "「較精確地」統計中 / 日 / 英文字數。
- 文章中的註解不算在字數內。
- 平假名與片假名亦包含在「中日文字數」內，每個平 / 片假名都算單獨一個字（但片假
  名不含連音「ー」）。
- 英文只計算「單字數」，不含標點。
- 韓文不包含在內。
※計算標準太多種了，例如英文標點是否算入、以及可能有不太常用的標點符號沒算入等
。且中日文標點的計算標準要看 Emacs 如何定義特殊標點符號如ヴァランタン・アルカン
中間的點也被 Emacs 算為一個字而不是標點符號。"
  (interactive)
  (let* ((v-buffer-string
          (progn
            (if (eq major-mode 'org-mode) ; 去掉 org 文件的 OPTIONS（以 #+ 開頭）
                (setq v-buffer-string (replace-regexp-in-string "^#\\+.+" ""
                                                                (buffer-substring-no-properties (point-min) (point-max))))
              (setq v-buffer-string (buffer-substring-no-properties (point-min) (point-max))))
            (replace-regexp-in-string (format "^ *%s *.+" comment-start) "" v-buffer-string)))
                                        ; 把註解行刪掉（不把註解算進字數）。
         (chinese-char-and-punc 0)
         (chinese-punc 0)
         (english-word 0)
         (chinese-char 0))
    (with-temp-buffer
      (insert v-buffer-string)
      (goto-char (point-min))
      ;; 中文（含標點、片假名）
      (while (re-search-forward wc-regexp-chinese-char-and-punc nil :no-error)
        (setq chinese-char-and-punc (1+ chinese-char-and-punc)))
      ;; 中文標點符號
      (goto-char (point-min))
      (while (re-search-forward wc-regexp-chinese-punc nil :no-error)
        (setq chinese-punc (1+ chinese-punc)))
      ;; 英文字數（不含標點）
      (goto-char (point-min))
      (while (re-search-forward wc-regexp-english-word nil :no-error)
        (setq english-word (1+ english-word))))
    (setq chinese-char (- chinese-char-and-punc chinese-punc))
    (message
     (format " 中日文字數（不含標點）：%s
 中日文字數（包含標點）：%s
 英文字數（不含標點）：%s
=======================
 中英文合計（不含標點）：%s"
             chinese-char chinese-char-and-punc english-word
             (+ chinese-char english-word)))))


(defun wing/hotspots ()
  "helm interface to my hotspots, which includes my locations,
org-files and bookmarks"
  (interactive)
  (helm :buffer "*helm: utities*"
        :sources `(,(wing//hotspots-sources))))

(defun wing//hotspots-sources ()
  "Construct the helm sources for my hotspots"
  `((name . "wing's center")
    (candidates . (("Blog" . (lambda() (blog-admin-start)))
                   ("Preview Blog" . (lambda() (browse-url "http://0.0.0.0:4000")))
                   ("Open Github" . (lambda() (browse-url "https://github.com/wing-ho")))
                   ("Open Blog" . (lambda() (browse-url "http://wing-ho.github.io")))
                   ("Org-Capture" . (lambda () (org-capture)))
                   ("ERC" . (lambda () (wing/start-erc)))
                   ("Agenda" . (lambda () (org-agenda "" "a")))
                   ("Agenda Next TODO" . (lambda () (org-agenda "" "t")))
                   ("Agenda Menu" . (lambda () (org-agenda)))
                   ))
    (candidate-number-limit)
    (action . (("Open" . (lambda (x) (funcall x)))))))

(defun wing/start-erc ()
  "Connect to IRC"
  (interactive)
  (when (y-or-n-p "Do you want to start IRC? ")
    (erc :server "irc.freenode.net" :port 6666 :nick erc-nick)))


(defun wing//notify (title message)
  (let ((terminal-notifier-command (executable-find "terminal-notifier")))
    (start-process "terminal-notifier"
                   "*terminal-notifier*"
                   terminal-notifier-command
                   "-title" title
                   "-message" message
                   "-activate" "org.gnu.Emacs"
                   "-sender" "org.gnu.Emacs")))
