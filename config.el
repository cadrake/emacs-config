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
(setq doom-modeline-icon t
      doom-modeline-major-mode-icon t
      doom-modeline-unicode-fallback t
      doom-modeline-vcs-max-length 20)

;; Display absolute line numbers
(setq display-line-numbers-type t)

;; Hilight selected line
(global-hl-line-mode 1)
(set-face-background hl-line-face "gray13")

;; Hilight line numbers a bit better
(set-face-background 'line-number "gray6")
(set-face-background 'line-number-current-line "gray16")

;; Only ask y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Show Paren Mode config
(show-paren-mode 1)
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#def")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)

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

;; Whitespace cleanup
(add-hook! 'after-save-hook #'delete-trailing-whitespace)

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

;; Whitespace Config
(setq-default indent-tabs-mode nil)
(setq whitespace-style (delete 'lines-tail whitespace-style))
(setq tab-width 4)

;; Disable exit confirmation
(setq confirm-kill-emacs nil)

;; ibuffer instead of list-buffer
(global-set-key [remap list-buffers] 'ibuffer)

;; Treemacs config
(use-package! treemacs
  :init (setq treemacs-no-png-images t
              treemacs-space-between-root-nodes nil
              treemacs-collapse-dirs 5)
  :bind (("C-c C-n" . treemacs-create-file)
         ("C-c C-d" . treemacs-delete))
  :config
  ;; Custom nerd icons theme
  (treemacs-load-theme "nerd-icons")

  ;; Allow pageup/pagedown in treemacs buffer
  (unbind-key "<next>" treemacs-mode-map)
  (unbind-key "<prior>" treemacs-mode-map)

  ;; Key bindings
  (global-set-key [f8] 'treemacs)

  ;; Set git colors
  (set-face-foreground 'treemacs-git-modified-face "red1")
  (set-face-foreground 'treemacs-git-added-face "green2")
  (set-face-foreground 'treemacs-git-conflict-face "yellow1"))

;; Projectile config
(use-package! projectile
  :init (setq projectile-auto-discover nil
              projectile-indexing-method 'alien
              projectile-sort-order 'recentf
              projectile-require-project-root t))

;; Org Mode config
(use-package! org
  :init (setq org-directory "~/Documents/org"
              org-agenda-files '("~/Documents/org/work")
              org-hide-emphasis-markers t
              truncate-string-ellipsis ""))
(add-hook! org-mode (electric-indent-local-mode -1))
(add-hook! org-mode :append #'visual-line-mode #'variable-pitch-mode)

;; Associate yaml-mode with yaml files and enable whitespace
(use-package! yaml-mode
  :hook (yaml-mode . (lambda () "Show whitespace in yaml mode" (whitespace-mode 1)))
  :mode "\\.yml\\'"
  :mode "\\.yaml\\'")

;; Associate dockerfile-mode with Dockerfile
(use-package! dockerfile-mode
  :mode "Dockerfile\\'")

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

;; LSP Mode config
(use-package! lsp-mode
  :init (setq lsp-idle-delay 0.500
              lsp-log-io nil
              lsp-auto-guess-root t
              lsp-java-enable-file-watch nil
              lsp-keymap-prefix "C-c C-l"
              read-process-output-max (* 1024 1024)))

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

  (fset #'all-the-icons-icon-family #'nerd-icons-icon-family)
  (fset #'all-the-icons-icon-family-for-buffer #'nerd-icons-icon-family-for-buffer)
  (fset #'all-the-icons-icon-family-for-file #'nerd-icons-icon-family-for-file)
  (fset #'all-the-icons-icon-family-for-mode #'nerd-icons-icon-family-for-mode)
  (fset #'all-the-icons-icon-for-buffer #'nerd-icons-icon-for-buffer)

  (fset #'all-the-icons-faicon #'nerd-icons-faicon)
  (fset #'all-the-icons-octicon #'nerd-icons-octicon)
  (fset #'all-the-icons-fileicon #'nerd-icons-fileicon)
  (fset #'all-the-icons-material #'nerd-icons-material)
  (fset #'all-the-icons-wicon #'nerd-icons-wicon))

;; LSP config for Java 8 Coretto
(setq lsp-java-jdt-download-url "https://download.eclipse.org/jdtls/milestones/0.57.0/jdt-language-server-0.57.0-202006172108.tar.gz"
      lsp-java-configuration-runtimes '[(:name "JavaCorretto-8"
                                         :path "/Library/Java/JavaVirtualMachines/amazon-corretto-8.jdk/Contents/Home"
                                         :default t)]
      lsp-java-java-path "/Library/Java/JavaVirtualMachines/amazon-corretto-8.jdk/Contents/Home/bin/java")
