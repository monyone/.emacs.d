;-------------------------------------------
; バージョン判定用変数
;-------------------------------------------
(defvar oldemacs-p (<= emacs-major-version 22))
(defvar emacs23-p (<= emacs-major-version 23))
(defvar emacs24-p (>= emacs-major-version 24))

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

;;; 

;-------------------------------------------
; フォーマットに関する設定
;------------------------------------------
(setq default-tab-width 2)

;-------------------------------------------
; 保存に関する設定
;-------------------------------------------

;;; バックアップファイルはウザい!!!
(setq make-backup-files nil)
(setq auto-save-default nil)

;-------------------------------------------
; エディタの表示に関する設定
;-------------------------------------------

;;; welcome メッセージは封印
(setq inhibit-startup-message t)

;;; 時間を表示
(display-time)
;;; 現在行を目立たせる
(global-hl-line-mode)
(set-face-background 'hl-line "DarkSlateBlue")
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
(load-theme 'deeper-blue t)

;-------------------------------------------
; 標準のパッケージ管理に関する設定
;-------------------------------------------

;;; emacs 24 以降なら標準でパッケージ管理できる
(require 'package)
;; 公式のリストは対応パッケージが少ない. 有志のも使う
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;; 標準で管理するパッケージを定義
(defvar installed_packages
  '(
    ;; markdown
    markdown-mode
    markdown-mode+
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
;;(require 'markdown-mode+)
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Mrkdown files" t)
(add-to-list 'auto-mode-alist '("\\.md$"       . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;; linum
(require 'linum)
(global-linum-mode 1)
(setq linum-format "%d ")

;; rainbow-delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(custom-set-faces '(rainbow-delimiters-depth-1-face ((t (:foreground "#7f8c8d")))))

;; sudden-death
(require 'sudden-death)

;; anzu
(global-anzu-mode +1)
(custom-set-variables
  '(anzu-mode-lighter "")
  '(anzu-deactivate-region t)
  '(anzu-search-threshold 1000))


(defun message-startup-time ()
  (message
   "Emacs loaded in %dms"
   (/ (- (+ (cl-third after-init-time) (* 1000000 (cl-second after-init-time)))
	 (+ (cl-third before-init-time) (* 1000000 (cl-second before-init-time))))
      1000)))
(add-hook 'after-init-hook 'message-startup-time)
