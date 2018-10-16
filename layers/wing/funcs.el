(require'cl)

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
(defun wing/run-tangle()
  (interactive)
  (save-excursion
    (org-babel-tangle-file buffer-file-name)
    (let (dir tangle-file)
      (setq dir (file-name-directory buffer-file-name))
      (setq tangle-file (cdr (assq :tangle  (nth 2 (org-babel-get-src-block-info)))))
      ;; foreground激活firefox窗口
      (shell-command (concat "firefox -foreground --new-tab " dir tangle-file  ))
      )
    ;; (shell-command (concat "google-chrome " dir tangle-file))
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
