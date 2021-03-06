;-------------------------------------------
; バージョン判定用変数
;-------------------------------------------
(defvar oldemacs-p (<= emacs-major-version 22))
(defvar emacs23-p  (<= emacs-major-version 23))
(defvar emacs24-p  (>= emacs-major-version 24))

;-------------------------------------------
; Compile-Log の非表示
;-------------------------------------------
(let ((win (get-buffer-window "*Compile-Log*")))
  (when win (delete-window win)))

;-------------------------------------------
; 日本語用の設定 (UTF-8)
;-------------------------------------------
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

;-------------------------------------------
; 全体のキー入力関連の設定
;-------------------------------------------

;;; C-h は delete だと決まってるだろ!!
(global-set-key "\C-h" 'delete-backward-char)

;;; 拡大, 縮小は1ステップで行いたい
; \C-+ で拡大
(global-set-key [(control ?;)] (lambda () (interactive) (text-scale-increase 1)))
(global-set-key [(control ?+)] (lambda () (interactive) (text-scale-increase 1)))
; \C-- で縮小
(global-set-key [(control ?=)] (lambda () (interactive) (text-scale-decrease 1)))
(global-set-key [(control ?-)] (lambda () (interactive) (text-scale-decrease 1)))
; \C-0 でデフォルトに戻す
(global-set-key [(control ?0)] (lambda () (interactive) (text-scale-increase 0)))

;-------------------------------------------
; フォーマットに関する設定
;------------------------------------------
(setq default-tab-width 2)
(setq c-basic-offset 2)

;- ここから言語固有の設定

;-------------------------------------------
; 保存に関する設定
;-------------------------------------------

;;; バックアップファイルはウザい!!!
(setq make-backup-files nil)
(setq auto-save-dexfault nil)

;-------------------------------------------
; エディタの表示に関する設定
;-------------------------------------------

;;; welcome メッセージは封印
(setq inhibit-startup-message t)

;;; 時間を表示
(display-time)
;;; 現在行を目立たせる
(global-hl-line-mode)
(set-face-background 'hl-line "bisque")
; (set-face-foreground 'highlight nil)
;;; カーソルの位置が何文字目かを表示する
(column-number-mode t)
;;; カーソルの位置が何行目かを表示する
(line-number-mode t)
;;; カーソルの場所を保存する
(require 'saveplace)
(setq-default save-place t)
;;; 対応する括弧を光らせる。
(show-paren-mode t)
(setq show-paren-delay 0)
;;; ウィンドウ内に収まらないときだけ括弧内も光らせる。
(setq show-paren-style 'mixed)
;(set-face-background 'show-paren-match "DarkSlateBlue")
;(set-face-foreground 'show-paren-match "black")
;;; 選択範囲を光らせる
(transient-mark-mode 1)
;;; C-x bでミニバッファにバッファ候補を表示
(iswitchb-mode t)
(setq read-buffer-function 'iswitchb-read-buffer)
(defvar iswitchb-regexp nil)
(defvar iswitch-b-prompt-newbuffer nil)

;-------------------------------------------
; カラーテーマ
;-------------------------------------------
(if emacs23-p (progn (require 'color-theme)
										 (color-theme-initialize)
										 (color-theme-deep-blue)
										 )
	(load-theme 'whiteboard t))

;-------------------------------------------
; フォント
;-------------------------------------------
(add-to-list 'default-frame-alist '(font . "ricty-13.5"))

;-------------------------------------------
; 標準のパッケージ管理に関する設定
;-------------------------------------------

;;; emacs 24 以降なら標準でパッケージ管理できる
(require 'package)
;; 公式のリストは対応パッケージが少ない. 有志のも使う
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(package-initialize)

;;; 標準で管理するパッケージを定義
(defvar installed_packages
  '(
    ;; markdown
    markdown-mode
		
    ;; 行番号を左に表示
    linum
    ;; 突然の死で強調表示
    sudden-death
    ;; 括弧の強調表示
    rainbow-delimiters
    ;; 検索
    anzu
   )
  "A list of packages to install from package at launch.")
;; 無いものに関してはインストールするように.
; 未インストールなものが無いかチェック

(if (not(require 'cl-lib nil 'noerror))
		(progn
			(package-refresh-contents)
			(package-install 'cl-lib)))
(require 'cl-lib)	
(setq already-installed-packages-p
  (cl-reduce (lambda (ret package) (and ret (if (eq 't package) 't (package-installed-p package)))) (append '(t t) installed_packages)))

; 未インストールのものがあったらリフレッシュしてインスコ
(if (not already-installed-packages-p)
  (progn
    (package-refresh-contents)
    (dolist (package installed_packages)
      (when (or (not (package-installed-p package)))
        (package-install package)))))

;-------------------------------------------
; インスコしたものに対する設定
;-------------------------------------------

;;markdown
(require 'markdown-mode)
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Mrkdown files" t)
(add-to-list 'auto-mode-alist '("\\.md$"       . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdc$"      . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;; linum
(require 'linum)
(global-linum-mode 1)
(setq linum-format "%d ")

;; rainbow-delimiters
(require 'rainbow-delimiters)
(require 'color)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(defun rainbow-delimiters-using-stronger-colors() 
	(interactive)
	(cl-loop
	 for index from 1 to rainbow-delimiters-max-face-count
	 do
	 (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
		 (cl-callf color-saturate-name (face-foreground face) 30))))
(add-hook 'emacs-startup-hook 'rainbow-delimiters-using-stronger-colors)

;; sudden-death
(require 'sudden-death)

;; anzu
(global-anzu-mode +1)
(custom-set-variables
  '(anzu-mode-lighter "")
  '(anzu-deactivate-region t)
  '(anzu-search-threshold 1000))

;-------------------------------
; 起動までの時間計測
;-------------------------------
(defun message-startup-time ()
  (message
   "Emacs loaded in %dms"
   (/ (- (+ (cl-third after-init-time) (* 1000000 (cl-second after-init-time)))
	 (+ (cl-third before-init-time) (* 1000000 (cl-second before-init-time))))
      1000)))
(add-hook 'after-init-hook 'message-startup-time)
