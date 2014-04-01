;-------------------------------------------
; 全体のキー入力関連の設定
;-------------------------------------------

;;; 
(global-set-key "\C-h" 'delete-backward-char)

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
;;; カーソルの位置が何文字目かを表示する
(column-number-mode t)
;;; カーソルの位置が何行目かを表示する
(line-number-mode t)
;;; カーソルの場所を保存する
(require 'saveplace)
(setq-default save-place t)
;;; 対応する括弧を光らせる。
(show-paren-mode 1)
;;; ウィンドウ内に収まらないときだけ括弧内も光らせる。
(setq show-paren-style 'mixed)
;;; 選択範囲を光らせる
(transient-mark-mode 1)

;;; C-x bでミニバッファにバッファ候補を表示
(iswitchb-mode t)
(setq read-buffer-function 'iswitchb-read-buffer)
(defvar iswitchb-regexp nil)
(defvar iswitch-b-prompt-newbuffer nil)

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
   )
  "A list of packages to install from package at launch.")
;; 無いものに関してはインストールするように.
; 未インストールなものが無いかチェック
(require 'cl)
(setq already-installed-packages-p
  (reduce (lambda (ret package) (and ret (if (eq 't package) 't (package-installed-p package)))) (append '(t t) installed_packages)))

; 未インストールのものがあったらリフレッシュしてインスコ
(if (not already-installed-packages-p)
  (progn
    (package-refresh-contents)
    (dolist (package installed_packages)
      (when (or (not (package-installed-p package)))
        (package-install package)))))

;-------------------------------------------
; el-get 用の設定
;-------------------------------------------
(add-to-list 'load-path "~/.emacs.d/el-get-elisp/el-get")
(setq el-get-dir "~/.emacs.d/el-get-elisp")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(require 'el-get)

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-elisp/el-git-user-recipes")

(el-get 'sync)

;; インストしたものをsyncする
(defvar el-get-packages
  '(
    ;; 括弧の強調表示
    rainbow-delimiters
    ;; 検索
    anzu
    ;;
   )
  "A list of package to install from el-get al alunch.")
(el-get 'sync el-get-packages)

;-------------------------------------------
; インスコしたものに対する設定
;-------------------------------------------

;;markdown
(require 'markdown-mode)
(require 'markdown-mode+)

;; linum
(require 'linum)
(global-linum-mode 1)
(setq linum-format "%d ")

;; rainbow-delimiters
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode t)
(custom-set-faces '(rainbow-delimiters-depth-1-face ((t (:foreground "#7f8c8d")))))

;; sudden-death
(require 'sudden-death)

;; anzu
(global-anzu-mode +1)
(custom-set-variables
  '(anzu-mode-lighter "")
  '(anzu-deactivate-region t)
  '(anzu-search-threshold 1000))

