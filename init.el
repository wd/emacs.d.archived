(package-initialize)

(defun load-config()
  "Load the actual configuration in literate 'org-mode' elisp."
  (interactive)
  (org-babel-load-file "~/.emacs.d/config.org"))

(load-config)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cfs--current-profile "profile1" t)
 '(cfs--profiles-steps '(("profile1" . 4)) t)
 '(custom-enabled-themes '(sanityinc-tomorrow-eighties))
 '(ivy-virtual-abbreviate 'full)
 '(package-selected-packages
   '(lsp-ui company-terraform terraform-mode forge conda ivy-yasnippet yaml-mode fic-mode company-lsp pipenv lsp-python-ms bing-dict osx-dictionary ibuffer-vc indent-tools highlight-indentation flycheck add-node-modules-path rainbow-delimiters guide-key ivy-rich wgrep company expand-region easy-kill ace-window smartparens lua-mode magit counsel-projectile projectile markdown-mode web-mode highlight-symbol react-snippets alert osx-clipboard cnfonts color-theme-sanityinc-tomorrow exec-path-from-shell use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:height 10.0 :foreground "gold")))))
