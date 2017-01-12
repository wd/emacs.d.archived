(if (window-system)
    (set-frame-size
     (selected-frame) 120 50))

(setq exec-path (append exec-path '("~/bin/") ))

;; set python-mode use ipython global
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
(setq python-shell-interpreter "/Users/wd/.pyenv/shims/ipython"
      python-shell-interpreter-args "-i")


;;
;; default browser
;; 
;; (setq browse-url-browser-function 'browse-url-generic
;;       browse-url-generic-program "google-chrome")
;; (if (eq system-type 'darwin)
;;     (setq browse-url-browser-function 'browse-url-default-macosx-browser)
;;   (setq browse-url-browser-function 'browse-url-firefox
;;         browse-url-new-window-flag  t
;;         browse-url-firefox-new-window-is-tab t)
;;   )

;;
;; misc settings
;; 
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(mouse-wheel-mode 1)
;; 不弹出图形界面的确认窗口
(setq use-dialog-box nil)

(global-auto-revert-mode 1)

;;
;; for mac only
;; 
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  (menu-bar-mode 1)
  )


;; 和x公用剪贴板
(setq x-select-enable-clipboard t)
;; (setq x-select-enable-primary t)

;;'y' for 'yes', 'n' for 'no'
(fset 'yes-or-no-p 'y-or-n-p)

;;禁用启动信息
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
;; 显示列号
(setq column-number-mode t) 

;; 防止页面滚动时跳动， scroll-margin 3 可以在靠近屏幕边沿3行时就开始滚动，可以很好的看到上下文。
;; (setq scroll-margin 3
;;       scroll-conservatively 2)

;;关闭烦人的出错时的提示声
;;(setq visible-bell t)

;;把title设置为“文件名@LC's Emacs"
(setq frame-title-format
        '("GNU Emacs - [ "(buffer-file-name "%f \]"
                (dired-directory dired-directory "%b \]"))))

;; 语法高亮
(global-font-lock-mode t)

;; 个人信息
(setq user-full-name "Wang Dong")
(setq user-mail-address "wd@wdicc.com")

;;光标靠近鼠标的时候，让鼠标自动让开，别挡住视线
(mouse-avoidance-mode 'animate)

;; 翻页后再回来的时候，光标到原来的位置
(setq scroll-preserve-screen-position t)

;;下面的这个设置可以让光标指到某个括号的时候显示与它匹配的括号
(show-paren-mode t)
;; (setq show-paren-style 'parentheses)
(setq show-paren-style 'expression)

;;设置缺省模式是text，而不是基本模式
(setq default-major-mode 'text-mode)
;; (setq fill-column 80)
;; (setq-default fill-column 80)
;; (setq longlines-show-hard-newlines t)
;; (setq longlines-auto-wrap t)
;; (add-hook 'text-mode-hook 'longlines-mode)
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)

;; 所有的备份文件转移到~/backups目录下
(setq auto-save-default nil)
(setq make-backup-files t)
(setq backup-by-copying t)
(setq version-control t)
(setq kept-old-versions 2)
(setq kept-new-versions 5)
(setq delete-old-versions t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
;; Emacs 中，改变文件时，默认都会产生备份文件(以 ~ 结尾的文件)。可以完全去掉
;; (并不可取)，也可以制定备份的方式。这里采用的是，把所有的文件备份都放在一
;; 个固定的地方("~/var/tmp")。对于每个备份文件，保留最原始的两个版本和最新的
;; 五个版本。并且备份的时候，备份文件是复本，而不是原件。

;;不产生备份文件
;(setq make-backup-files nil)

;;设置kill-ring-max(我不知道怎么翻译这个词：)为200，以防不测：）
(setq kill-ring-max 200)

;; 设置mark， C-x <SPC>
(global-set-key (kbd "C-t") 'set-mark-command)

;; Make Emacs UTF-8 compatible for both display and editing:
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; 打开 quick-calc
;; (global-set-key (kbd "M-#") 'quick-calc)

;; 高量当前行 // 会造成滚动的时候抖动，不是很爽。。
;; (require 'hl-line)
;; (global-hl-line-mode 1)

;; 查找打开当前光标所在的文件
(global-set-key (kbd "C-x f") 'find-file-at-point)

;;
;; indent
;;
;; 不用 TAB 字符来indent
(setq-default indent-tabs-mode nil)
;;设置tab为4个空格的宽度，而不是原来的2
(setq-default tab-width 4)
(setq tab-width 4)
(setq tab-stop-list ())
(cl-loop for x downfrom 40 to 1 do
           (setq tab-stop-list (cons (* x 4) tab-stop-list)))
(add-hook 'html-mode-hook
              (lambda ()
                (setq indent-line-function 'indent-relative)))
(add-hook 'php-mode-hook
              (lambda ()
                (setq php-mode-force-pear 1)
                (setq c-basic-offset 4)
            ))
;; auto indent
;; (setq indent-line-function 'indent-relative-maybe)
;; (global-set-key (kbd "RET") 'align-newline-and-indent)

;;
;; lisp
;;
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;; 
;; narrowing
;; 
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;;
;; register key binding
;;
(define-prefix-command 'ctl-x-r-map-alias)
(global-set-key (kbd "<f6>") 'ctl-x-r-map-alias)
;; (define-key ctl-x-r-map-alias "\C-@" 'point-to-register)
;; (define-key ctl-x-r-map-alias [?\C-\ ] 'point-to-register)
;; (define-key ctl-x-r-map-alias " " 'point-to-register)
(define-key ctl-x-r-map-alias "j" 'jump-to-register)
;; (define-key ctl-x-r-map-alias "s" 'copy-to-register)
;; (define-key ctl-x-r-map-alias "x" 'copy-to-register)
;; (define-key ctl-x-r-map-alias "i" 'insert-register)
;; (define-key ctl-x-r-map-alias "g" 'insert-register)
;; (define-key ctl-x-r-map-alias "r" 'copy-rectangle-to-register)
;; (define-key ctl-x-r-map-alias "n" 'number-to-register)
;; (define-key ctl-x-r-map-alias "+" 'increment-register)
(define-key ctl-x-r-map-alias "w" 'window-configuration-to-register)
;; (define-key ctl-x-r-map-alias "f" 'frame-configuration-to-register)


(defun back-to-indentation-or-beginning (arg)
  "combine two function into one call."
  (interactive "^p")
  (if (bolp)
      (back-to-indentation)
    (move-beginning-of-line arg)))
(define-key global-map (kbd "C-a") 'back-to-indentation-or-beginning)

;; proxy
 (setq url-proxy-services
       '(("no_proxy" . "^\\(localhost\\|10.*\\)")
         ("http" . "127.0.0.1:6152")
         ("https" . "127.0.0.1:6152")))

(provide 'wd-misc)
