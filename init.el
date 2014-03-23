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
    markdown-mode  ;;; markdown用
    markdown-mode+ ;;;
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
    ;;括弧の強調表示
    rainbow-delimiters
    ;;helm関連
    helm
    helm-descbinds  ;;キーバインドをhelmで表示
   )
  "A list of package to install from el-get al alunch.")
(el-get 'sync el-get-packages)

;-------------------------------------------
; インスコしたものに対する設定
;-------------------------------------------

;;markdown
(require 'markdown-mode)
(require 'markdown-mode+)

;; rainbow-delimiters
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode t)
(custom-set-faces '(rainbow-delimiters-depth-1-face ((t (:foreground "#7f8c8d")))))

;;helm
(require 'helm-config)
(require 'helm-descbinds)

(helm-mode 1)

(helm-descbinds-mode)


(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-^") 'helm-c-apropos)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "C-c y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c o") 'helm-occur)
(global-set-key (kbd "C-c s") 'helm-do-grep)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-c g") 'helm-resume)
(global-set-key (kbd "C-c b") 'helm-descbinds)

(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

;; Emulate `kill-line' in helm minibuffer
(setq helm-delete-minibuffer-contents-from-point t)
(defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
  "Emulate `kill-line' in helm minibuffer"
  (kill-new (buffer-substring (point) (field-end))))

; helm-find-files で helm-execute-presistent-action した場合, ファイルがなければキャンセルする.
(defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
  "Execute command only if CANDIDATE exists"
  (when (file-exists-p candidate)
    ad-do-it))

;; Disable helm in some functions
(add-to-list 'helm-completing-read-handlers-alist '(find-alternate-file . nil))

(defadvice helm-ff-transform-fname-for-completion (around my-transform activate)
  "Transform the pattern to reflect my intention"
  (let* ((pattern (ad-get-arg 0))
         (input-pattern (file-name-nondirectory pattern))
         (dirname (file-name-directory pattern)))
    (setq input-pattern (replace-regexp-in-string "\\." "\\\\." input-pattern))
    (setq ad-return-value
          (concat dirname
                  (if (string-match "^\\^" input-pattern)
                      ;; '^' is a pattern for basename
                      ;; and not required because the directory name is prepended
                      (substring input-pattern 1)
                    (concat ".*" input-pattern))))))

(defun helm-buffers-list-pattern-transformer (pattern)
  (if (equal pattern "")
      pattern
    ;; Escape '.' to match '.' instead of an arbitrary character
    (setq pattern (replace-regexp-in-string "\\." "\\\\." pattern))
    (let ((first-char (substring pattern 0 1)))
      (cond ((equal first-char "*")
             (concat " " pattern))
            ((equal first-char "=")
             (concat "*" (substring pattern 1)))
            (t
             pattern)))))

(add-to-list 'helm-source-buffers-list
             '(pattern-transformer helm-buffers-list-pattern-transformer))
