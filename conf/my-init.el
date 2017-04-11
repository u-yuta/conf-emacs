
(let ( (default-directory
         (file-name-as-directory (concat user-emacs-directory "site-lisp")))
       )
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path)
  )

(setq package-archives
      '(("gnu"         . "http://elpa.gnu.org/packages/")
        ("org"         . "http://orgmode.org/elpa/")
        ("melpa"       . "http://melpa.org/packages/")
        ("marmalade"   . "http://marmalade-repo.org/packages/")))
(package-initialize)
(require 'use-package)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
; パッケージが無ければ自動インストール
(setq use-package-always-ensure t)
(require 'use-package)

; 何もしないマクロ定義
(unless (require 'use-package nil t)
  (defmacro use-package (&rest args)))

(delete-selection-mode t)

(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)

;; TABの表示幅。初期値は8
(setq-default tab-width 4)

(cua-mode t) ; cua-modeをオン
(setq cua-enable-cua-keys nil) ; CUAキーバインドを無効にする

;; redo+
(use-package redo+
  :bind ("C-?" . redo))
;; undo-hist
(use-package undohist
  :config (undohist-initialize))
;; undo-tree
(use-package undo-tree
  :config
  (global-undo-tree-mode))
;; point-undo
(use-package point-undo
  :bind (("M-[" . point-undo)
         ("M-]" . point-redo)))

;; デフォルトの文字コード
(set-default-coding-systems 'utf-8-dos)

;; テキストファイル／新規バッファの文字コード
(prefer-coding-system 'utf-8-dos)

;; ファイル名の文字コード
(set-file-name-coding-system 'utf-8-unix)

;; キーボード入力の文字コード
(set-keyboard-coding-system 'utf-8-unix)

;; サブプロセスのデフォルト文字コード
(setq default-process-coding-system '(undecided-dos . utf-8-unix))

;; 環境依存文字 文字化け対応
(set-charset-priority 'ascii 'japanese-jisx0208 'latin-jisx0201
                      'katakana-jisx0201 'iso-8859-1 'cp1252 'unicode)
(set-coding-system-priority 'utf-8 'euc-jp 'iso-2022-jp 'cp932)

;; (set-language-environment "UTF-8") ;; UTF-8 でも問題ないので適宜コメントアウトしてください
(setq default-input-method "W32-IME")
(setq-default w32-ime-mode-line-state-indicator "[--]")
(setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
(w32-ime-initialize)

(add-hook 'w32-ime-on-hook '(lambda () (set-cursor-color "coral4")))
(add-hook 'w32-ime-off-hook '(lambda () (set-cursor-color "black")))

;; ミニバッファに移動した際は最初に日本語入力が無効な状態にする
(add-hook 'minibuffer-setup-hook 'deactivate-input-method)
 
;; isearch に移行した際に日本語入力を無効にする
(add-hook 'isearch-mode-hook '(lambda ()
                                (deactivate-input-method)
                                (setq w32-ime-composition-window (minibuffer-window))))
(add-hook 'isearch-mode-end-hook '(lambda () (setq w32-ime-composition-window nil)))
 
;; helm 使用中に日本語入力を無効にする
(advice-add 'helm :around '(lambda (orig-fun &rest args)
                             (let ((select-window-functions nil)
                                   (w32-ime-composition-window (minibuffer-window)))
                               (deactivate-input-method)
                               (apply orig-fun args))))

; USキーボードで日本語入力のON/OFFを切り替えるのに"Alt-`"
; を使うとメッセージが出るのを抑止する。
(global-set-key [M-kanji] 'ignore)

;; デフォルト フォント
;; (set-face-attribute 'default nil :family "Migu 1M" :height 110)
;(set-face-font 'default "Migu 1M-11:antialias=standard")
(set-face-font 'default "Myrica M-11:antialias=natural")

;; プロポーショナル フォント
;; (set-face-attribute 'variable-pitch nil :family "Migu 1M" :height 110)
;(set-face-font 'variable-pitch "Migu 1M-11:antialias=standard")
(set-face-font 'variable-pitch "Myrica M-11:antialias=natural")

;; 等幅フォント
;; (set-face-attribute 'fixed-pitch nil :family "Migu 1M" :height 110)
;(set-face-font 'fixed-pitch "Migu 1M-11:antialias=standard")
(set-face-font 'fixed-pitch "Myrica M-11:antialias=natural")

;; ツールチップ表示フォント
;; (set-face-attribute 'tooltip nil :family "Migu 1M" :height 90)
;(set-face-font 'tooltip "Migu 1M-9:antialias=standard")
(set-face-font 'tooltip "Myrica M-9:antialias=natural")

;; フォントサイズ調整
(global-set-key (kbd "C-<wheel-up>")   '(lambda() (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C-=")            '(lambda() (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C-<wheel-down>") '(lambda() (interactive) (text-scale-decrease 1)))
(global-set-key (kbd "C--")            '(lambda() (interactive) (text-scale-decrease 1)))

;; フォントサイズ リセット
(global-set-key (kbd "M-0") '(lambda() (interactive) (text-scale-set 0)))

(setq default-frame-alist
      (append '((width                . 95)  ; フレーム幅
                (height               . 55 ) ; フレーム高
                (left                 . 200 ) ; 配置左位置
                (top                  . 20 ) ; 配置上位置
                (line-spacing         . 0  ) ; 文字間隔
                (left-fringe          . 10 ) ; 左フリンジ幅
                (right-fringe         . 11 ) ; 右フリンジ幅
                (menu-bar-lines       . 1  ) ; メニューバー
                (tool-bar-lines       . 1  ) ; ツールバー
                (vertical-scroll-bars . 1  ) ; スクロールバー
                (scroll-bar-width     . 17 ) ; スクロールバー幅
                (cursor-type          . box) ; カーソル種別
                (alpha                . 100) ; 透明度
                ) default-frame-alist) )
(setq initial-frame-alist default-frame-alist)

;; フレーム タイトル
(setq frame-title-format
      (format "%%f - Emacs %s@%s" emacs-version system-name))


;; フルスクリーン化
(global-set-key (kbd "<M-return>") 'toggle-frame-fullscreen)

;; 初期画面の非表示（有効：t、無効：nil）
(setq inhibit-startup-message nil)
(setq inhibit-startup-screen t)

(tool-bar-mode -1)

(setq visible-bell t)

;; 行番号の表示（有効：t、無効：nil）
(line-number-mode t)
;; 列番号の表示（有効：t、無効：nil）
(column-number-mode t)
 
;; モードライン カスタマイズ
(setq-default
 mode-line-format
 `(
   ""
   w32-ime-mode-line-state-indicator
   " "
   mode-line-mule-info
   mode-line-modified
   mode-line-frame-identification
   mode-line-buffer-identification
   " "
   global-mode-string
   " %[("
   mode-name
   mode-line-process
   "%n"
   ")%] "
   (which-func-mode ("" which-func-format " "))
   (line-number-mode
    (:eval
     (format "L%%l/L%d " (count-lines (point-max) 1) )))
   (column-number-mode " C%c ")
   (-3 . "%p")
   )
 )
(setq mode-line-frame-identification " ")

;; cp932エンコードの表記変更
(coding-system-put 'cp932 :mnemonic ?P)
(coding-system-put 'cp932-dos :mnemonic ?P)
(coding-system-put 'cp932-unix :mnemonic ?P)
(coding-system-put 'cp932-mac :mnemonic ?P)
 
;; UTF-8エンコードの表記変更
(coding-system-put 'utf-8 :mnemonic ?U)
(coding-system-put 'utf-8-with-signature :mnemonic ?u)
 
;; 改行コードの表記追加
(setq eol-mnemonic-dos       ":Dos ")
(setq eol-mnemonic-mac       ":Mac ")
(setq eol-mnemonic-unix      ":Unx ")
(setq eol-mnemonic-undecided ":??? ")

;; 時刻の表示
(require 'time)
(setq display-time-24hr-format t)
(setq display-time-string-forms '(24-hours ":" minutes))
(display-time-mode t)

;; ウィンドウ縦分割時のバッファ画面外文字の切り詰め表示（有効：t、無効：nil）
(setq truncate-partial-width-windows t)

;; 同一バッファ名にディレクトリ付与
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

(require 'linum)

;; 行移動を契機に描画
(defvar linum-line-number 0)
(declare-function linum-update-current "linum" ())
(defadvice linum-update-current
    (around linum-update-current-around activate compile)
  (unless (= linum-line-number (line-number-at-pos))
    (setq linum-line-number (line-number-at-pos))
    ad-do-it
    ))

;; バッファ中の行番号表示の遅延設定
(defvar linum-delay nil)
(setq linum-delay t)
(defadvice linum-schedule (around linum-schedule-around () activate)
  (run-with-idle-timer 1.0 nil #'linum-update-current))

;; 行番号の書式
(defvar linum-format nil)
(setq linum-format "%5d")

;; バッファ中の行番号表示（有効：t、無効：nil）
(global-linum-mode t)

;; 文字サイズ
(set-face-attribute 'linum nil :height 0.75)

(load-theme 'hc-zenburn t)

;; paren-mode：対応する括弧を強調して表示する
(setq show-paren-delay 0.1) ; 表示までの秒数。初期値は0.125
(show-paren-mode t) ; 有効化

;; parenのスタイル: expressionは括弧内も強調表示
(setq show-paren-style 'mixed)
;; フェイスを変更する
(set-face-background 'show-paren-match-face nil)
(set-face-underline-p 'show-paren-match-face "blue")

;; スクロール時のカーソル位置を維持（有効：t、無効：nil）
(setq scroll-preserve-screen-position t)

;; スクロール開始の残り行数
(setq scroll-margin 0)

;; スクロール時の行数
(setq scroll-conservatively 10000)

;; スクロール時の行数（scroll-marginに影響せず）
(setq scroll-step 0)

;; 画面スクロール時の重複表示する行数
(setq next-screen-context-lines 1)

;; キー入力中の画面更新を抑止（有効：t、無効：nil）
(setq redisplay-dont-pause t)

;; recenter-top-bottomのポジション
(setq recenter-positions '(middle top bottom))

;; 横スクロール開始の残り列数
(setq hscroll-margin 1)

;; 横スクロール時の列数
(setq hscroll-step 1)

;; スクロールダウン
(global-set-key (kbd "C-z") 'scroll-down)

;; バッファの最後までスクロールダウン
(defadvice scroll-down (around scroll-down activate compile)
  (interactive)
  (let (
        (bgn-num (+ 1 (count-lines (point-min) (point))))
        )
    (if (< bgn-num (window-height))
        (goto-char (point-min))
      ad-do-it) ))

;; バッファの先頭までスクロールアップ
(defadvice scroll-up (around scroll-up activate compile)
  (interactive)
  (let (
        (bgn-num (+ 1 (count-lines (point-min) (point))))
        (end-num nil)
        )
    (save-excursion
      (goto-char (point-max))
      (setq end-num (+ 1 (count-lines (point-min) (point))))
      )
    (if (< (- (- end-num bgn-num) (window-height)) 0)
        (goto-char (point-max))
      ad-do-it) ))

(require 'saveplace)
(save-place-mode 1) ;; Changed for Emacs 25

(global-set-key "\C-x\C-b" 'buffer-menu)

;; diredを2つのウィンドウで開いている時に、デフォルトの移動orコピー先をもう一方のdiredで開いているディレクトリにする
(setq dired-dwim-target t)
;; ディレクトリを再帰的にコピーする
(setq dired-recursive-copies 'always)
;; diredバッファでC-sした時にファイル名だけにマッチするように
(setq dired-isearch-filenames t)

;; ファイルオープン時のバックアップ（~）（有効：t、無効：nil）
(setq make-backup-files   t)  ;; 自動バックアップの実行有無
(setq version-control     t)  ;; バックアップファイルへの番号付与
(setq kept-new-versions   3)  ;; 最新バックアップファイルの保持数
(setq kept-old-versions   0)  ;; 最古バックアップファイルの保持数
(setq delete-old-versions t)  ;; バックアップファイル削除の実行有無

;; ファイルオープン時のバックアップ（~）の格納ディレクトリ
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "/tmp/emacsbk"))
            backup-directory-alist))

;; 編集中ファイルの自動バックアップ（有効：t、無効：nil）
(setq backup-inhibited nil)

;; 終了時に自動バックアップファイルを削除（有効：t、無効：nil）
(setq delete-auto-save-files nil)

;; 編集中ファイルのバックアップ（有効：t、無効：nil）
(setq auto-save-list-file-name nil)
(setq auto-save-list-file-prefix nil)

;; 編集中ファイルのバックアップ間隔（秒）
(setq auto-save-timeout 3)

;; 編集中ファイルのバックアップ間隔（打鍵）
(setq auto-save-interval 100)

;; 編集中ファイル（##）の格納ディレクトリ
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "/tmp/emacsbk") t)))

(setq create-lockfiles nil)

;; 大文字・小文字を区別しないでサーチ（有効：t、無効：nil）
(setq-default case-fold-search t)

;; インクリメント検索時に縦スクロールを有効化（有効：t、無効：nil）
(setq isearch-allow-scroll nil)

;; C-dで検索文字列を一文字削除
(define-key isearch-mode-map (kbd "C-d") 'isearch-delete-char)

;; C-yで検索文字列にヤンク貼り付け
(define-key isearch-mode-map (kbd "C-y") 'isearch-yank-kill)

;; C-eで検索文字列を編集
(define-key isearch-mode-map (kbd "C-e") 'isearch-edit-string)

;; Tabで検索文字列を補完
(define-key isearch-mode-map (kbd "TAB") 'isearch-yank-word)

;; C-gで検索を終了
(define-key isearch-mode-map (kbd "C-g")
  '(lambda() (interactive) (isearch-done)))

;; 日本語の検索文字列をミニバッファに表示
(define-key isearch-mode-map (kbd "<compend>")
  '(lambda() (interactive) (isearch-update)))
(define-key isearch-mode-map (kbd "<kanji>")
  'isearch-toggle-input-method)
(add-hook
 'isearch-mode-hook
 '(lambda() (setq w32-ime-composition-window (minibuffer-window)))
 )
(add-hook
 'isearch-mode-end-hook
 '(lambda() (setq w32-ime-composition-window nil))
 )

(add-to-list 'load-path "~/.emacs.d/elisp/lisp")
(add-to-list 'load-path "~/.emacs.d/elisp/contrib/lisp" t)

;; fontify code in code blocks
(setq org-src-fontify-natively t)
 
(setq org-src-tab-acts-natively t)

(setq org-startup-indented t)

(setq org-startup-folded (quote content))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(setq org-directory "~/git/org")
(setq org-default-notes-file "~/git/org/refile.org")
(setq org-agenda-files (quote ("~/git/org")))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

(setq org-use-speed-commands t)

(use-package tabbar
  :config
  ;; tabbar有効化（有効：t、無効：nil）
  (call-interactively 'tabbar-mode t)

  ;; ボタン非表示
  (dolist (btn '(tabbar-buffer-home-button
                 tabbar-scroll-left-button
                 tabbar-scroll-right-button))
    (set btn (cons (cons "" nil) (cons "" nil)))
    )

  ;; タブ切替にマウスホイールを使用（有効：0、無効：-1）
  (call-interactively 'tabbar-mwheel-mode -1)
  (remove-hook 'tabbar-mode-hook      'tabbar-mwheel-follow)
  (remove-hook 'mouse-wheel-mode-hook 'tabbar-mwheel-follow)

  ;; タブグループを使用（有効：t、無効：nil）
  (defvar tabbar-buffer-groups-function nil)
  (setq tabbar-buffer-groups-function nil)

  ;; タブの表示間隔
  (defvar tabbar-separator nil)
  (setq tabbar-separator '(1.0))

  ;; タブ切り替え
  (global-set-key (kbd "<C-tab>") 'tabbar-forward-tab)
  (global-set-key (kbd "C-q")     'tabbar-backward-tab))

;; open recent files
(use-package recentf
  :config
  (setq recentf-max-menu-items 400)
  (setq recentf-exclude '(".recentf"))
  (setq recentf-auto-cleanup 10)
  (setq recentf-auto-save-timer
        (run-with-idle-timer 30 t 'recentf-save-list))
  (defun recentf-ido-find-file ()
    "Find a recent file using Ido."
    (interactive)
    (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
      (when file
        (find-file file))))
  (recentf-mode 1)
  :bind
  ("C-x C-r" . recentf-ido-find-file))

(use-package ido
  :init
  (ido-mode t)
  :config
  (setq ido-enable-flex-matching t)
  (when (fboundp 'ido-vertical-mode)
  (ido-vertical-mode 1))
  ; ido-vertical にて C-n, C-p, ↑, ↓で選択できるようにする
  (setq ido-vertical-define-keys 'C-n-C-p-up-and-down))

(use-package smex
  :ensure t
  :config
  (smex-initialize)
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands)))

(use-package migemo
  :config
  (setq exec-path (append exec-path '("C:\\app\\cmigemo-default-win64")))
  (setq migemo-dictionary "C:/app/cmigemo-default-win64/dict/utf-8/migemo-dict")
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (migemo-init))

(use-package volatile-highlights
  :config
  (volatile-highlights-mode t))

(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  (setq magit-git-executable "C:/Program Files/Git/bin/git.exe")
  )

;; markdown-mode
(use-package markdown-mode
   :mode (("\\.md\\'" . markdown-mode))
   )


