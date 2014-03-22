;-------------------------------------------
; 保存に関する設定
;-------------------------------------------

;;; バックアップファイルはウザい!!!
(setq make-backup-files nil)
(setq auto-save-default nil)

;-------------------------------------------
; エディタの表示に関する設定
;-------------------------------------------

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

;;; C-x bでミニバッファにバッファ候補を表示
(iswitchb-mode t)
;(iswitchb-default-keybindings)

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
    markdown-mode  ;;; markdown用
    markdown-mode+ ;;;
    tabbar         ;;; tab化
   )
  "A list of packages to install from package at launch.")
;; 無いものに関してはインストールするように.
(dolist (package installed_packages)
  (when (or (not (package-installed-p package)))
    (package-install package)))

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

(el-get 'sync)

;; インストしたものをsyncする
(defvar el-get-packages
  '()
  "A list of package to install from el-get al alunch.")
(el-get 'sync el-get-packages)

;;; インスコしたものに対する設定
;;markdown
(require 'markdown-mode)
(require 'markdown-mode+)
;;tabbar
(require 'tabbar)
;; scratch buffer以外をまとめてタブに表示する
(setq tabbar-buffer-groups-function
      (lambda (b) (list "All Buffers")))
(setq tabbar-buffer-list-function
      (lambda ()
        (remove-if
         (lambda(buffer)
           (unless (string-match (buffer-name buffer)
                                 "\\(*scratch*\\|*Apropos*\\|*shell*\\|*eshell*\\|*Customize*\\)")
             (find (aref (buffer-name buffer) 0) " *"))
           )
         (buffer-list))))
;; tabbarを有効にする
(tabbar-mode)
;; ボタンをシンプルにする
(setq tabbar-home-button-enabled "")
(setq tabbar-scroll-right-button-enabled "")
(setq tabbar-scroll-left-button-enabled "")
(setq tabbar-scroll-right-button-disabled "")
(setq tabbar-scroll-left-button-disabled "")
;; Ctrl-Tab, Ctrl-Shift-Tab でタブを切り替える
(dolist (func '(tabbar-mode tabbar-forward-tab tabbar-forward-group tabbar-backward-tab tabbar-backward-group))
  (autoload func "tabbar" "Tabs at the top of buffers and easy control-tab navigation"))
(defmacro defun-prefix-alt (name on-no-prefix on-prefix &optional do-always)
  `(defun ,name (arg)
     (interactive "P")
     ,do-always
     (if (equal nil arg)
         ,on-no-prefix
       ,on-prefix)))
(defun-prefix-alt shk-tabbar-next (tabbar-forward-tab) (tabbar-forward-group) (tabbar-mode 1))
(defun-prefix-alt shk-tabbar-prev (tabbar-backward-tab) (tabbar-backward-group) (tabbar-mode 1))
(global-set-key [(control tab)] 'shk-tabbar-next)
(global-set-key [(control shift tab)] 'shk-tabbar-prev)
;; GUIで直接ファイルを開いた場合フレームを作成しない
(add-hook 'before-make-frame-hook
          (lambda ()
            (when (eq tabbar-mode t)
              (switch-to-buffer (buffer-name))
              (delete-this-frame))))
