(require 'use-package)

;; 让 shell  变量起作用
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))
  )

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-eighties t)
  )

(use-package cnfonts
  :ensure t
  :config
  (cnfonts-enable)
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

;;
;; org-mode
;;

(use-package org
  :bind (("C-c t" . org-capture)
         ("C-c a" . org-agenda))
  :config

  ;; support org-protocol to capture in browser
  (server-start)
  (require 'org-protocol)

  ;; (setq org-archive-location "::* Archived Tasks")
  (use-package alert
    :config
    (setq alert-default-style 'fringe)
    (setq alert-fade-time 20)
    )

  (use-package org-alert
    ;; :ensure t
    :config
    ;; (setq alert-default-style 'notifier)
    (setq org-alert-alert-before 30)
    (org-alert-enable)
  )
  
  (setq org-archive-location "archive.org::* From %s")
  (setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/inbox.org" "Tasks")
         "* TODO %?\n%U")
        ("l" "Todo with link" entry (file+headline "~/org/inbox.org" "Links")
         "* TODO %?[[%:link][%:description]] %U\n" :prepend t)
        ("L" "Capture a link from browser" entry (file+headline "~/org/inbox.org" "Links")
         "* TODO [[%:link][%:description]]\n%u\n\n%:initial"
         :empty-lines 1)
        ))
  (setq org-agenda-files '("~/org"))
  (setq org-log-done 'note)

  ;; open inbox.org when start emacs
  (find-file "~/org/inbox.org")
  )

;;
;; snippet
;;
(use-package yasnippet
  :ensure react-snippets
  :config
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
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-enable-auto-indentation nil)

  (setq web-mode-engines-alist
        '(("angular" . "task.*/.*\\.html\\'"))
        )

  (setq web-mode-content-types-alist
        '(("jsx" . "GeneralAviationForUser/.*/.*\\.js\\'")
          ("jsx" . "GeneralAviationForManager/.*/.*\\.js\\'")
          ("jsx" . "Daphne/.*\\.js\\'")
          ("jsx" . "Coco/.*\\.js\\'"))
        )

  (add-hook 'web-mode-hook
            #'(lambda ()
                                        ;(yas-activate-extra-mode 'nxml-mode)
                (add-to-list 'web-mode-engine-attr-regexps '("angular" . "ng-"))
                (set-face-attribute 'web-mode-html-attr-engine-face nil :foreground "steel blue")
                ))

  (defun my-web-mode-hook ()
    (setq web-mode-enable-auto-pairing nil)
    )

  (add-hook 'web-mode-hook  'my-web-mode-hook)
)

(use-package markdown-mode
  :ensure t
  :config
  (set-face-attribute 'markdown-list-face nil
                      :foreground "gold"
                      )
)

;; projectile
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  )

(use-package counsel-projectile
  :ensure t
  )

;; tramp
(use-package tramp
  :config
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "10.10.8.1") "remote-shell" "sh"))
  )

;; magit
(use-package magit
  :ensure t
  :bind* ("C-x g" . magit-status)
  :config
  (setq magit-auto-revert-mode nil)
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
  ;;(show-smartparens-global-mode +1)
)


(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  ;; (setq aw-ignore-current t)
  (global-set-key (kbd "C-M-h") #'ace-window)
  ;; (custom-set-faces
  ;;  '(aw-leading-char-face
  ;;    ((t
  ;;      (:height 10.0 :foreground "gold")
  ;;      ))
  ;;    ))
  )


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

;; company-mode
(use-package company
  :ensure t
  :init
  (setq company-minimum-prefix-length 2)
  ;; (setq company-dabbrev-ignore-case t)
  ;; (setq company-idle-delay t)
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-s") #'company-filter-candidates)

  (require 'company-my-backend)

  (add-hook 'web-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   '(
                      company-tide
                      company-react
                      ;; company-dabbrev-code
                      ;; company-keywords
                      company-files
                      company-yasnippet))))

  ;; (add-to-list 'company-backends '(company-yasnippet
  ;;                                   company-files
  ;;                                   company-dabbrev-code
  ;;                                   company-keywords
  ;;                                   ))

  (global-set-key (kbd "C-c y") 'company-yasnippet)

  ;; (add-hook 'python-mode-hook
  ;;           (lambda ()
  ;;             (add-to-list 'company-backends 'company-yasnippet)
  ;;             ))
  )

;; ivy swiper
(defun my-ivy-yank-word ()
  (interactive)
  (let (amend)
    (with-selected-window (ivy-state-window ivy-last)
      (goto-char swiper--opoint)
      (setq amend (thing-at-point 'symbol)))
    (when amend (insert amend))))

(use-package counsel
  :ensure t
  :bind (("C-c i" . counsel-projectile-ag)
         ("M-x" . counsel-M-x)
         ("C-c f" . counsel-projectile-find-file)
         ("M-X" . ivy-switch-buffer)
         ("C-c v" . counsel-imenu)
         )
  :init
  (setq recentf-max-saved-items 200)
  ;; (setq ivy-virtual-abbreviate 'full)
  :config
  (use-package wgrep :ensure t)
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t ; treat recentf, bookmarks as virtual buffers.
        ivy-height 10
        ivy-display-style 'fancy
        ivy-count-format "(%d/%d) "
        ivy-initial-inputs-alist nil ; remove initial ^ input.
        ivy-extra-directories nil ; remove . and .. directory.
        ivy-wrap nil
        )
  
  (set-variable 'ivy-on-del-error-function '(lambda()))

  ;; (ivy-add-actions
  ;;  'counsel-find-file
  ;;  '(("g" counsel-find-file-ag-action "grep")))
  )

(use-package ivy-rich
  :ensure t
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  (ivy-rich-mode 1)
)

(use-package swiper
  :after ivy
  :ensure t
  :bind (("C-s" . swiper-isearch)
         :map swiper-map
         ("M-q" . swiper-query-replace)
         ("C-w" . my-ivy-yank-word)
         ("C-'" . swiper-avy)
         )
  )

;; "M-q" swiper-query-replace
;; "C-l" swiper-recenter-top-bottom
;; "C-'" swiper-avy
;; "C-7" swiper-mc
;; "C-c C-f" swiper-toggle-face-matching

;; avy
(use-package avy
  :ensure t
  :bind (("M-s" . avy-goto-char-timer)
         ("C-." . avy-pop-mark))
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

(use-package guide-key
  :ensure t
  :config
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c" "<SPC>"))
  (setq guide-key/recursive-key-sequence-flag t)
  (setq guide-key/popup-window-position :bottom)
  (guide-key-mode 1)
  )

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  )

(use-package ox-reveal
  :ensure t
  )

(use-package python-mode
  :mode "\\.py\\'"
  :interpreter (("python" . python-mode)
                ("python3" . python-mode)
                )
  )

(use-package add-node-modules-path
  :ensure t
)

(use-package flycheck
  :ensure t
  :init
  (eval-after-load 'web-mode
    '(add-hook 'web-mode-hook #'add-node-modules-path))
  :config
  (global-flycheck-mode t)
  (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc tsx-tide))

  (flycheck-add-mode 'javascript-eslint 'web-mode)
)


(use-package highlight-indentation
  :ensure t
  :config
  (add-hook 'python-mode-hook 'highlight-indentation-mode)
  )


;;; hightlight-tail
;; (use-package highlight-tail
;;   :ensure t
;;   :config
;;   ;; (setq highlight-tail-colors 
;;   ;;       '(("balck" . 0)
;;   ;;         ("white" . 25)
;;   ;;         ))

;;   (setq highlight-tail-colors '(("black" . 0)
;;                               ("#bc2525" . 25)
;;                               ("black" . 100)))
;;   (highlight-tail-mode)
;;   )

;;; indent-tools
(use-package indent-tools
  :ensure t
  ;; :init
  ;; (add-hook 'python-mode-hook
  ;;           (lambda () (define-key python-mode-map (kbd "C-c i") 'indent-tools-hydra/body))
  ;;           )
  )


(use-package ibuffer
  :ensure ibuffer-vc
  :config
  (add-hook 'ibuffer-hook
    (lambda ()

      (face-remap-add-relative 'default 'font-lock-comment-face)
      (copy-face 'font-lock-keyword-face 'tempface )
      (setq ibuffer-filter-group-name-face 'tempface)
      (face-remap-add-relative ibuffer-filter-group-name-face font-lock-doc-face)
      (ibuffer-vc-set-filter-groups-by-vc-root)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic))))

  (defconst gcs-ibuffer-fontification-alist
    '((ruby-mode . font-lock-string-face)
      (sh-mode . font-lock-string-face)
      (objc-mode . font-lock-constant-face)
      (c-mode . font-lock-constant-face)
      (java-mode . font-lock-constant-face)
      (emacs-lisp-mode . font-lock-variable-name-face)
      (org-mode . font-lock-negation-char-face)
      (dired-mode . font-lock-function-name-face)
      (term-mode . font-lock-doc-string-face)
      (python-mode . font-lock-variable-name-face)))
  
  (setq ibuffer-fontification-alist
        `(,@(mapcar (lambda (b)
                      `(9999 (eq major-mode ',(car b)) ,(cdr b)))
                    gcs-ibuffer-fontification-alist)
          (90 (string-match "magit" (symbol-name major-mode))
              font-lock-function-name-face)
          (90 (or (string-match "^*" (buffer-name))
                  (memq major-mode ibuffer-help-buffer-modes))
              font-lock-comment-face)))

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
  ;; (define-key ibuffer-mode-map (kbd "C-g") 'quit-window)
  ;; (define-key ibuffer-mode-map (kbd "j") 'ibuffer-forward-line)
  ;; (define-key ibuffer-mode-map (kbd "k") 'ibuffer-backward-line)
  ;; (define-key ibuffer-mode-map (kbd "C-n") 'ibuffer-forward-filter-group)
  ;; (define-key ibuffer-mode-map (kbd "C-p") 'ibuffer-backward-filter-group)
  :bind ("C-x C-b" . ibuffer))

;; (use-package golden-ratio
;;   :ensure t
;;   :config
;;   (setq golden-ratio-auto-scale 1)
;;   (golden-ratio-mode 1)
;;   )

(use-package osx-dictionary
  :ensure t
  :bind ("C-c d" . osx-dictionary-search-pointer)
  )

(use-package bing-dict
  :ensure t
  :bind ("C-c e" . bing-dict-brief)
  )

;; (use-package outline
;;   :config
;;   (add-hook 'emacs-lisp-mode-hook
;;             (lambda ()
;;               (setq outline-regexp ";;; \\|(use-package ")
;;               (outline-minor-mode)
;;               (outline-hide-body)
;;               ))
;;   :bind ("M-o" . outline-cycle)
;;   ;; C-c @ C-a expand all
;;   )

(use-package fic-mode
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'fic-mode)
  (add-hook 'python-mode-hook 'fic-mode)
  )

(use-package yaml-mode
  :ensure t
  :mode "\\.yml$"
  )

(use-package lsp-python-ms
  :ensure t
  :demand nil
  :config

  ;;(setq lsp-python-ms-extra-paths "")
  (setq lsp-python-ms-executable
        (string-trim (shell-command-to-string
                      "find ~/.vscode/extensions/ -name 'Microsoft.Python.LanguageServer' | sort | tail -1")))
  ;; for dev build of language server
  (setq lsp-python-ms-dir
        (file-name-directory lsp-python-ms-executable)))


(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook  (python-mode . (lambda ()
                          (pipenv-activate)
                          (require 'lsp-python-ms)
                          (lsp)))
  :config

  (setq lsp-auto-configure nil)

  (use-package pipenv
    :ensure t
    :init
    (setq
     pipenv-projectile-after-switch-function
     #'pipenv-projectile-after-switch-extended))

  (use-package company-lsp
    :ensure t
    :config
    (add-to-list 'company-backends 'company-lsp)
    )

  ;; (use-package lsp-ui
  ;;   :ensure t
  ;;   :config
  ;;   (setq lsp-prefer-flymake nil)


  ;;   (require 'lsp-ui-flycheck)
  ;;   (with-eval-after-load 'lsp-mode
  ;;     (add-hook 'lsp-after-open-hook
  ;;               (lambda ()
  ;;                 (setq-local flycheck-checker 'python-flake8)
  ;;                 (flycheck-add-next-checker 'python-flake8 'lsp-ui)
  ;;                 (lsp-ui-flycheck-add-mode major-mode)
  ;;                 (add-to-list 'flycheck-checkers 'lsp-ui)
  ;;                 (add-hook 'lsp-after-diagnostics-hook 'lsp-ui-flycheck--report nil t)
  ;;                 )
  ;;               ))

  ;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  ;;   )



;; (use-package lsp-javascript-typescript
;;   :ensure t
;;   :config
;;   (defun my-company-transformer (candidates)
;;     (let ((completion-ignore-case t))
;;       (all-completions (company-grab-symbol) candidates)))

;;   (defun my-js-hook nil
;;     (make-local-variable 'company-transformers)
;;     (push 'my-company-transformer company-transformers))

;;   (add-hook 'web-mode-hook 'my-js-hook)
;;   (add-hook 'web-mode-hook #'lsp-javascript-typescript-enable)
;;   )

  )

(use-package easy-hugo
  :init
  (setq easy-hugo-basedir "~/blog/")
  (setq easy-hugo-url "https://wdicc.com")
  (setq easy-hugo-postdir "content/post")
  (setq easy-hugo-previewtime "300")
  (setq easy-hugo-default-ext ".org")
  :bind ("C-c C-e" . easy-hugo)
  )

(use-package ivy-yasnippet
  :ensure t
  )

;;; wd-elpa ends
(provide 'wd-elpa)
