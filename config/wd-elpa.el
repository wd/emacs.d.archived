(require 'use-package)

;; 让 shell  变量起作用
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))
  )


(use-package ample-theme
  :ensure t
  :config
  (load-theme 'ample t t)
  )

(use-package zenburn-theme
  :disabled
  :ensure t
  :config
  (load-theme 'zenburn t)
  )

;; fonts
(use-package chinese-fonts-setup
  :ensure t
  :config
  (chinese-fonts-setup-enable)
  )

(use-package ido
  :config
  (ido-mode t)
  (setq ido-max-directory-size 100000)
  (setq ido-auto-merge-delay-time 2)
  :bind (("C-x C-f" . ido-find-file)
         ("C-x C-d" . ido-dired))
  )

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward)
  (setq uniquify-separator ":")
)

(use-package whitespace
  :ensure t
  :config
  (setq whitespace-style '(trailing tabs))
  (global-whitespace-mode)
)


(use-package windmove
  :ensure t
  :config
  (windmove-default-keybindings 'meta)
  )

;;
;; snippet
;;
(use-package yasnippet
  :ensure t
  :config
  (add-to-list 'yas/snippet-dirs "~/.emacs.d/snippets" 'append)
  (yas-global-mode 1)
)

;; 
;; highlight-symbol
;; 

(use-package highlight-symbol
  :ensure t
  :bind (("C-c h h" . highlight-symbol-at-point)
         ("C-c h H" . highlight-symbol-remove-all))
  )


;; 
;; web-mode
;;
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)

  (setq web-mode-engines-alist
        '(("angular" . "task.*/.*\\.html\\'"))
        )

  (setq web-mode-content-types-alist
        '(("jsx" . "testProj/.*\\.js\\'"))
        )

  (add-hook 'web-mode-hook
            #'(lambda ()
                                        ;(yas-activate-extra-mode 'nxml-mode)
                (add-to-list 'web-mode-engine-attr-regexps '("angular" . "ng-"))
                (set-face-attribute 'web-mode-html-attr-engine-face nil :foreground "steel blue")
                ))

  (defun my-web-mode-hook ()
    (setq web-mode-enable-auto-pairing nil))

  (add-hook 'web-mode-hook  'my-web-mode-hook)
)

(use-package markdown-mode
  :ensure t
  :config
  (set-face-attribute 'markdown-list-face nil
                      :foreground "gold"
                      )
)

;; ace-isearch
;; (use-package  ace-isearch
;;   :config
;;   (global-ace-isearch-mode +1)
;;   (define-key isearch-mode-map (kbd "C-f") 'isearch-forward-symbol-at-point)
;;   )

;; projectile
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  )

;; helm
(use-package helm
  :ensure helm-swoop
  :ensure helm-projectile
  :config
  ; (helm-autoresize-mode t)
  (helm-projectile-on)
  (setq helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-bookmarks
                                    helm-source-recentf
                                    helm-source-buffer-not-found))

  ;; (set-face-attribute 'helm-selection nil
  ;;                     :background "dark cyan"
  ;;                     :foreground "brightwhite"
  ;;                     )

  :bind (("M-X" . helm-mini)
         ("M-x" . helm-M-x)
         ("C-s" . helm-swoop-without-pre-input))
)

;; tramp
(use-package tramp
  :config (setq tramp-ssh-controlmaster-options
              "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no"))

;; magit
(use-package magit
  :ensure t
  ;; :bind* ([tab] . magit-section-toggle)
  :config
  ;; (setq magit-auto-revert-mode nil)
  (setq magit-last-seen-setup-instructions "1.4.0")
  )

;; clipboard

(use-package osx-clipboard
  :ensure t
  :config
  (osx-clipboard-mode +1)
  )


;; lua-mode

(use-package lua-mode
  :ensure t
  :config
  (setq lua-indent-level 4)
  )

;; smartparens

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (defun sp-web-mode-is-code-context (id action context)
  (and (eq action 'insert)
       (not (or (get-text-property (point) 'part-side)
                (get-text-property (point) 'block-side)))))

  (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))
  (smartparens-global-mode)
)


;; avy & ace-window
;; (use-package avy
;;   :config
;;   ;; (define-key isearch-mode-map (kbd "M-s") 'avy-isearch)
;;   ;; (define-key isearch-mode-map (kbd "M-f") 'helm-swoop-from-isearch)
;;   ;; M-i already do this
;;   )


;; (use-package ace-window
;;   :ensure t
;;   :config
;;   (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
;; ;;  (setq aw-ignore-current t)
;;   (global-set-key (kbd "C-x o") #'ace-window)
;;   ;; (custom-set-faces
;;   ;;  '(aw-leading-char-face
;;   ;;    ((t (:height 20.0 )))
;;   ;;    ))
;;   )


(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill)
  ;; (global-set-key [remap mark-sexp] 'easy-mark)
  )

(use-package expand-region
  :ensure t
  :bind ("M-m" . er/expand-region)
  )

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

;; (defun do-yas-expand ()
;;   (let ((yas/fallback-behavior 'return-nil))
;;     (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    ;; (if (or (not yas/minor-mode)
    ;;         (null (do-yas-expand)))
    (if (check-expansion)
        (company-complete-common)
      (indent-for-tab-command))))
;)

;; company-mode
(use-package company
  :ensure t
  :config
  ;; (company-quickhelp-mode)
  (add-hook 'after-init-hook 'global-company-mode)
  ;; (let* (
  ;;        ;;(background (if window-system "#263238" nil)))
  ;;       (background (if window-system "dark slate gray" "dark slate gray")))
  ;;   (custom-set-faces
  ;;    `(company-tooltip ((t (:inherit default :foreground "yellow" :background ,background))))
  ;;    `(company-tooltip-common ((t (:inherit default :weight bold :background ,background))))
  ;;    `(company-tooltip-selection ((t (:inherit default :background "#90A4AE"))))
  ;;    ;; `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
  ;;    ;; `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
  ;;    `(company-preview-common ((t (:inherit default :foreground "yellow" :background ,background))))
  ;;    `(company-tooltip-annotation ((t (:inherit default :foreground "red" :background ,background))))
  ;;    `(company-tooltip-annotation-selection ((t (:inherit company-tooltip-selection))))
  ;;    )
  ;;   )

  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  ;; (global-set-key [tab] 'tab-indent-or-complete)

  ;; Add yasnippet support for all company backends
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  (defvar company-mode/enable-yas t "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend)    (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  )

;; Flx-ido

;; (use-package flx-ido
;;   :config
;;   (ido-mode 1)
;;   (ido-everywhere 1)
;;   (flx-ido-mode 1)
;;   ;; disable ido faces to see flx highlights.
;;   (setq ido-enable-flex-matching t)
;;   (setq ido-use-faces nil)
;;   )


;; ;; ivy swiper
;; (defun wd-swiper-at-point ()
;;   "Pull next word from buffer into search string."
;;   (interactive)
;;   (let (query)
;;     (with-ivy-window
;;       (let ((tmp (symbol-at-point)))
;;         (setq query tmp)))
;;     (when query
;;       (insert (format "%s" query))
;;       )))

;; (use-package ivy
;;   :config
;;   (ivy-mode 1)
;;   (setq ivy-use-virtual-buffers t)
;;   (set-variable 'ivy-on-del-error-function '(lambda()))
;;   ;; (global-set-key "\C-s" 'wd-swiper-at-point)
;;   )

;; (use-package swiper
;;   :config
;;   (global-set-key "\C-s" 'swiper)
;;   (define-key swiper-map (kbd "C-w") 'wd-swiper-at-point)
;;   (define-key swiper-map (kbd "C-f") 'swiper-avy)
;;   )

;; "M-q" swiper-query-replace
;; "C-l" swiper-recenter-top-bottom
;; "C-'" swiper-avy
;; "C-7" swiper-mc
;; "C-c C-f" swiper-toggle-face-matching

;; avy
(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-char-timer)
  )

;;
;; perl-mode
;;

(defalias 'perl-mode 'cperl-mode)

(use-package cperl-mode
  :mode "\\.[pP][Llm]\\'"
  :interpreter (("perl" . cperl-mode)
                ("perl5" . cperl-mode))
  :config
  (setq cperl-electric-keywords nil)
  (setq cperl-electric-parens nil)
  (define-key cperl-mode-map "{" 'nil)

  ;; (global-set-key (kbd "C-;") 'comment-dwim)
  (add-hook 'cperl-mode-hook (lambda () (abbrev-mode -1)))

  (setq cperl-indent-level 4
        cperl-close-paren-offset -4
        cperl-continued-statement-offset 4
        cperl-indent-parens-as-block t
        cperl-tab-always-indent t
        cperl-highlight-variables-indiscriminately t
        )
  )

;; (setq cperl-highlight-variables-indiscriminately t)
;; (add-hook 'cperl-mode-hook 'sl-highlight-todo)

(use-package guide-key
  :ensure t
  :config
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
  (setq guide-key/recursive-key-sequence-flag t)
  (setq guide-key/popup-window-position :bottom)
  (guide-key-mode 1)
  )

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-switch (:post (progn
                                   (message
                                    "Thank you, come again.")))
    "switch window"
    ("l" windmove-right)
    ("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("q" nil "quit")
    )
  :bind ("C-c o" . hydra-switch/body)
  )

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'lisp-mode #'rainbow-delimiters-mode)
  )

(use-package ox-reveal
  :ensure t
  )

(use-package python-mode
  :mode "\\.py\\'"
  :interpreter (("python" . python-mode)
                ("python3" . python-mode)
                 )
  :config
  (add-hook 'python-mode-hook
       (lambda ()
         (set (make-variable-buffer-local 'beginning-of-defun-function)
               'py-beginning-of-def-or-class)
         (setq outline-regexp "def\\|class ")))
  )


(use-package python-magic
  :bind ("M-o" . outline-cycle)
  )

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode t)
  (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc))
)

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(use-package jedi
  :ensure t
  :init
  (add-hook 'python-mode-hook 'my/python-mode-hook)
  )

;;; pythgon elpy
(use-package elpy
  :ensure
  :init
  (setq elpy-rpc-backend "jedi")
  (elpy-enable)
  (elpy-use-ipython)
  :bind (("M-*" . pop-tag-mark))
  )

;;; hightlight-tail
(use-package highlight-tail
  :ensure t
  :config
  ;; (setq highlight-tail-colors 
  ;;       '(("balck" . 0)
  ;;         ("white" . 25)
  ;;         ))

  (setq highlight-tail-colors '(("black" . 0)
                              ("#bc2525" . 25)
                              ("black" . 100)))
  (highlight-tail-mode)
  )

;;; indent-tools
(use-package indent-tools
  :ensure t
  :init
  (add-hook 'python-mode-hook
            (lambda () (define-key python-mode-map (kbd "C-c >") 'indent-tools-hydra/body))
            )
  )


(use-package ibuffer
  :ensure ibuffer-vc
  :config
  (add-hook 'ibuffer-hook
    (lambda ()
      (ibuffer-vc-set-filter-groups-by-vc-root)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic))))

  (setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (vc-status 16 16 :left)
              " "
              filename-and-process)))
  :bind ("C-x C-b" . ibuffer))

(use-package golden-ratio
  :ensure t
  :config
  (setq golden-ratio-auto-scale 1)
  (golden-ratio-mode 1)
  )

(use-package osx-dictionary
  :ensure t
  :bind ("C-c d" . osx-dictionary-search-pointer)
  )

(use-package helm-ag
  ;; require brew install the_silver_searcher
  ;; run helm-do-ag command
  :ensure t
  )

(provide 'wd-elpa)
