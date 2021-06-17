;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Alex Drake"
      user-mail-address "adrake@attentivemobile.com")

;; Load my custom elisp directories
(add-to-list 'load-path "~/.doom.d/lisp/")

;; Load my custom lisp files
(require 'nerd-icons-treemacs-theme)

;; DOOM
(use-package! doom-themes
  :config
  (setq doom-molokai-brighter-comments t)
  (load-theme 'doom-molokai t)

  ;; corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; DOOM modeline
(setq doom-modeline-unicode-fallback t
      ;;doom-modeline-icon t ;; Still broken
      doom-modeline-vcs-max-length 20)

;; Display absolute line numbers
(setq display-line-numbers-type t)

;; Hilight selected line
(global-hl-line-mode 1)
(custom-set-faces!
  '(hl-line :background "gray13"))

;; Hilight line numbers a bit better
(custom-set-faces!
  '(line-number :background "gray6")
  '(line-number-current-line :background "gray16"))

;; Only ask y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Show Paren Mode config
(show-paren-mode 1)
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(custom-set-faces!
  '(show-paren-match :foreground "white" :background "red" :weight extra-bold))

;; Allows moving buffers with Ctrl-c <Arrow>
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; Allows resizing windows with Shift-Ctrl-<Arrow>
(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)

;; Kill with C-k instead of clear
(setq kill-whole-line t)

;; Enable smartscan mode
(global-smartscan-mode t)

;; Magit config
(use-package! magit
  :init (setq magit-git-executable "/usr/bin/git")
  :bind (("C-c v b" . magit-branch-and-checkout)))

;; Enable backup files and send them to a better directory
(setq auto-save-default t
      make-backup-files t)

(let ((backup-dir "~/.doom.d/backups/")
      (auto-saves-dir "~/.doom.d/auto-saves/"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))

(setq backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t  ; Clean up the backups
      version-control t      ; Use version numbers on backups,
      kept-new-versions 5    ; keep some new versions
      kept-old-versions 2)   ; and some old ones, too

;; No word wrapping
(set-default 'truncate-lines nil)
(setq truncate-partial-width-windows t)
(global-visual-line-mode 0)

;; Fix font caching
(setq inhibit-compacting-font-caches t)

;; Whitespace cleanup on save
(add-hook! 'after-save-hook #'delete-trailing-whitespace)

;; Whitespace config
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default whitespace-style (delete 'lines-tail whitespace-style))

;; Disable exit confirmation
(setq confirm-kill-emacs nil)

;; ibuffer instead of list-buffer
(global-set-key [remap list-buffers] 'ibuffer)

;; Ivy Rich Icons
(use-package! all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))
(setq all-the-icons-ivy-rich-color-icon t)


;; Treemacs config
(use-package! treemacs
  :init (setq treemacs-no-png-images t
              treemacs-space-between-root-nodes nil
              treemacs-window-background-color (cons "color-233" "color-19")
              treemacs-collapse-dirs 5)
  :config
  ;; Custom nerd icons theme
  (treemacs-load-theme "nerd-icons")

  ;; Allow pageup/pagedown in treemacs buffer
  (unbind-key "<next>" treemacs-mode-map)
  (unbind-key "<prior>" treemacs-mode-map)

  ;; Key bindings
  (global-set-key [f8] 'treemacs)

  ;; Set git colors
  (set-face-foreground 'treemacs-git-modified-face "red")
  (set-face-foreground 'treemacs-git-added-face "color-70")
  (set-face-foreground 'treemacs-git-conflict-face "color-226"))

;; Projectile config
(use-package! projectile
  :init (setq projectile-auto-discover nil
              projectile-indexing-method 'alien
              projectile-sort-order 'recentf
              projectile-globally-ignored-files '(".DS_Store" "Icon" "TAGS")
              projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o" ".class")
              projectile-require-project-root nil))

;; Org Mode config
(use-package! org
  :init (setq org-directory "~/Dropbox/org"
              org-agenda-files '("~/Dropbox/org/work/attentive")
              org-hide-emphasis-markers t)
  :hook (org-mode . (lambda () (electric-indent-local-mode -1)))
  :hook (org-mode . (lambda () (visual-line-mode 0))))
(use-package! org-fancy-priorities
  :config (setq org-fancy-priorities-list '("" " " "  ")))
(after! org
  (setq org-startup-indented nil))
(map! "C-c [" #'org-insert-structure-template) ;; C-, is funky with terminals

;; Associate yaml-mode with yaml files and enable whitespace
(use-package! yaml-mode
  :hook (yaml-mode . (lambda () "Show whitespace in yaml mode" (whitespace-mode 1)))
  :hook (yaml-mode . (lambda () "Disable word wrap" (visual-line-mode 0)))
  :mode "\\.yml\\'"
  :mode "\\.yaml\\'")

;; Associate dockerfile-mode with Dockerfile
(use-package! dockerfile-mode
  :mode "Dockerfile\\'")

;; Associate jenkins files with jenkinsfile-mode
(use-package! jenkinsfile-mode
  :mode "Jenkinsfile\\'"
  :mode "jenkins.pipeline\\'")

;; Associate csv-mode with csv's
(use-package! csv-mode
  :hook (csv-mode . (lambda () "Disable word wrap" (visual-line-mode 0)))
  :mode "\\.csv\\'")

;; Configure markdown-mode based on common md files and render with pandoc
(use-package! markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "/usr/local/bin/pandoc"))

;; Expand Region config
(use-package! expand-region
  :bind (("M-=" . er/expand-region)
         ("M-+" . er/contract-region)))

;; Company mode config
(setq company-minimum-prefix-length 2
      company-idle-delay 0.2)

;; Java Config
(setq-hook! 'java-mode-hook c-basic-offset 4)
(setq-hook! 'java-mode-hook tab-width 4)
(setq-hook! 'java-mode-hook indent-tabs-mode nil)

;; Json Mode Indentation
(setq-hook! 'json-mode-hook js-indent-level 2)

;; LSP Mode config
(use-package! lsp-mode
  :init (setq lsp-idle-delay 0.500
              lsp-log-io nil
              lsp-auto-guess-root nil
              lsp-enable-file-watchers nil
              lsp-keymap-prefix "C-c C-l"
              read-process-output-max (* 1024 1024)))

;; Fix LSP and Molokai
(custom-set-faces!
 '(lsp-face-highlight-textual :background "color-18"))

;; Replace all-the-icons functions with their nerd-icons counterparts for terminal support
(with-eval-after-load 'all-the-icons
  (require 'nerd-icons)

  (fset #'all-the-icons-insert #'nerd-icons-insert)
  (fset #'all-the-icons-insert-faicon #'nerd-icons-insert-faicon)
  (fset #'all-the-icons-insert-fileicon #'nerd-icons-insert-fileicon)
  (fset #'all-the-icons-insert-material #'nerd-icons-insert-material)
  (fset #'all-the-icons-insert-octicon #'nerd-icons-insert-octicon)
  (fset #'all-the-icons-insert-wicon #'nerd-icons-insert-wicon)

  (fset #'all-the-icons-icon-for-dir #'nerd-icons-icon-for-dir)
  (fset #'all-the-icons-icon-for-file #'nerd-icons-icon-for-file)
  (fset #'all-the-icons-icon-for-mode #'nerd-icons-icon-for-mode)
  (fset #'all-the-icons-icon-for-url #'nerd-icons-icon-for-url)
  (fset #'all-the-icons-icon-for-buffer #'nerd-icons-icon-for-buffer)

  (fset #'all-the-icons-icon-family #'nerd-icons-icon-family)
  (fset #'all-the-icons-icon-family-for-buffer #'nerd-icons-icon-family-for-buffer)
  (fset #'all-the-icons-icon-family-for-file #'nerd-icons-icon-family-for-file)
  (fset #'all-the-icons-icon-family-for-mode #'nerd-icons-icon-family-for-mode)

  (fset #'all-the-icons-faicon #'nerd-icons-faicon)
  (fset #'all-the-icons-octicon #'nerd-icons-octicon)
  (fset #'all-the-icons-fileicon #'nerd-icons-fileicon)
  (fset #'all-the-icons-material #'nerd-icons-material)
  (fset #'all-the-icons-alltheicon #'nerd-icons-material)
  (fset #'all-the-icons-wicon #'nerd-icons-wicon))

;; LSP config for Java 8 Coretto
(setq lsp-java-jdt-download-url "https://download.eclipse.org/jdtls/milestones/0.57.0/jdt-language-server-0.57.0-202006172108.tar.gz"
      lsp-java-configuration-runtimes '[(:name "JavaCorretto-8"
                                         :path "/Library/Java/JavaVirtualMachines/amazon-corretto-8.jdk/Contents/Home"
                                         :default t)]
      lsp-java-java-path "/Library/Java/JavaVirtualMachines/amazon-corretto-8.jdk/Contents/Home/bin/java")
