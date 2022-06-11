;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Alex Drake"
      user-mail-address "adrake@attentivemobile.com")

;; Special constant for WSL
(defconst IS-WINDOWS-WSL (and (eq system-type 'gnu/linux) (getenv "WSL_DISTRO_NAME")))

;; Load additional elisp directories
(add-to-list 'load-path "~/.doom.d/custom/")
(add-to-list 'load-path "~/.doom.d/extensions")

;; Load extension scripts
(require 'subr+)

;; Load my custom lisp files
(require 'nerd-icons-treemacs-theme)
(require 'org-tagging-support)

;; DOOM themes
(use-package! doom-themes
  :config
  (setq doom-molokai-brighter-comments t
        doom-themes-enable-bold t
        doom-themes-enable-italic t)
  ;; Enable molokai theme
  (load-theme 'doom-molokai t)

  ;; corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; DOOM modeline
(setq doom-modeline-icon t
      doom-modeline-buffer-file-name-style 'truncate-upto-project
      doom-modeline-major-mode-icon t
      doom-modeline-major-mode-color-icon t
      doom-modeline-unicode-fallback t
      doom-modeline-vcs-max-length 50)

;; Display absolute line numbers
(setq display-line-numbers-type t)

;; Terminal capabilities (Avoids phanton self inserts)
(setq xterm-extra-capabilities nil)

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

;; Swiper ISearch
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-r") 'swiper-isearch-C-r)

;; Magit config
(use-package! magit
  :init (setq magit-git-executable "/usr/bin/git")
  :bind
  (("C-c v b" . magit-branch-and-checkout))
  :config
  (set-face-foreground 'magit-diff-added "color-22")
  (set-face-foreground 'magit-diff-added-highlight "color-22"))

;; Enable backup files and send them to a better directory
(setq auto-save-default t
      make-backup-files t)

;; Fix bug in xclip-mode where xclip-program is unset when xclip-method is powershell
(if IS-WINDOWS-WSL (setq xclip-method 'powershell
                         xclip-program "powershell.exe"))

;; Better backups
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

;; Smartparens config
(use-package! smartparens
  :config
  (sp-pair "'" nil :actions :rem)
  (sp-pair "\"" nil :actions :rem))

;; Disable exit confirmation
(setq confirm-kill-emacs nil)

;; ibuffer instead of list-buffer
(global-set-key [remap list-buffers] 'ibuffer)

;; Ivy Rich Icons
(use-package! all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))
(setq all-the-icons-ivy-rich-color-icon t)

;; Dired Icons
(add-hook! 'dired-mode-hook 'all-the-icons-dired-mode)

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

  ;; Other config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode nil)

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
              org-agenda-files (directory-files-recursively "~/Dropbox/org/work/attentive" "\\.org")
              org-auto-align-tags t
              org-pretty-entities t
              org-hide-emphasis-markers t
              org-agenda-block-separator nil
              org-agenda-tags-column (- 8 (window-body-width))
              org-agenda-compact-blocks t)
  :hook (org-mode . (lambda () (electric-indent-local-mode -1)))
  :hook (org-mode . (lambda () (visual-line-mode 0))))
(after! org
  (setq org-startup-indented nil))
(map! "C-c [" #'org-insert-structure-template) ;; C-, is funky with terminals

;; Org Mode additional config
(setq org-agenda-prefix-format
      (quote
       ((agenda . " %i %-26:c%?-12t% s")
        (timeline . "  % s")
        (todo . " %i %-26c %-40(concat (org-format-outline-path (org-get-outline-path)))")
        (tags . " %i %-26:c")
        (search . " %i %-26:c"))))
(use-package! org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :config (setq org-fancy-priorities-list '("" " " "  " "   ")))
(use-package! org-super-agenda
  :init (setq org-super-agenda-header-prefix " "
              org-super-agenda-groups
              '((:name "Prioritized" :priority>= "C" :order 1 :face (:background "color-241"))
                (:name "Jira Tickets" :order 2 :face (:background "color-234") :and (:property "assignee" :property "type-id"))
                (:auto-category t :auto-parent t :priority< "C" :order 3))))
(custom-set-faces!
  '(org-super-agenda-header :weight ultra-bold :background "blue" :underline "white"))
(org-super-agenda-mode)

;; Org Jira (Enabled if environment variables exist)
(if (> (length (getenv "JIRALIB_URL")) 0)
    (progn
      (setq jiralib-url (getenv "JIRALIB_URL")
            org-jira-done-states '("Closed" "Resolved" "Done" "Won't Fix") ;; Attentive Jira
            org-agenda-files (append (directory-files-recursively "~/.org-jira" "\\.org") org-agenda-files))
      (if (not (file-directory-p "~/.org-jira")) (make-directory "~/.org-jira"))))

;; Associate yaml-mode with yaml files and enable whitespace
(use-package! yaml-mode
  :hook (yaml-mode . (lambda () "Show whitespace in yaml mode" (whitespace-mode 1)))
  :hook (yaml-mode . (lambda () "Disable word wrap" (visual-line-mode 0)))
  :mode "\\.yml\\'"
  :mode "\\.yaml\\'")

;; Json Mode configuration
(setq-hook! 'json-mode-hook js-indent-level 2)

;; Graphviz Mode Config
(use-package! graphviz-dot-mode
  :config
  (setq graphviz-dot-indent-width 4)
  :mode "\\.gv\\'")
(use-package! company-graphviz-dot)

;; Associate dockerfile-mode with Dockerfile
(use-package! dockerfile-mode
  :mode "Dockerfile\\'")

;; Associate jenkins files with jenkinsfile-mode
(use-package! jenkinsfile-mode
  :mode "Jenkinsfile\\'"
  :mode "jenkins.pipeline\\'"
  :init (add-to-list 'company-backends 'company-keywords))

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

;; Associate protobuf-mode with proto files
(use-package! protobuf-mode
  :mode "\\.proto\\'")

;; Expand Region config
(use-package! expand-region
  :bind (("M-=" . er/expand-region)
         ("M-+" . er/contract-region)))

;; Company mode config
(setq company-minimum-prefix-length 2
      company-idle-delay 0.2
      company-shell-dont-fetch-meta t)

;; Java Config
(setq-hook! 'java-mode-hook c-basic-offset 4)
(setq-hook! 'java-mode-hook tab-width 4)
(setq-hook! 'java-mode-hook indent-tabs-mode nil)

;; Go Config
(setq-hook! 'go-mode-hook gofmt-command "goimports")
(setq-hook! 'go-mode-hook before-save-hook 'gofmt-before-save)

;; LSP Mode config
(use-package! lsp-mode
  :init (setq lsp-idle-delay 0.500
              lsp-log-io nil
              lsp-auto-guess-root nil
              lsp-enable-file-watchers nil
              lsp-keymap-prefix "C-c C-l"
              lsp-terraform-server '("terraform-ls" "serve")
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

;; Debugging
(setq debug-on-error t)
;;(debug-on-entry 'self-insert-command)

;; TODO: Can I get this from JAVA_HOME or some shit
;;
;; lsp-java Configurations
;;

;; Java 8 Coretto w/Gradle
;; (use-package! lsp-java :after lsp
;;               :init (setq lsp-java-java-path "/Library/Java/JavaVirtualMachines/amazon-corretto-8.jdk/Contents/Home/bin/java"
;;                           lsp-java-import-gradle-java-home "/Library/Java/JavaVirtualMachines/amazon-corretto-8.jdk/Contents/Home/bin/java"
;;                           lsp-java-jdt-download-url "https://download.eclipse.org/jdtls/milestones/0.57.0/jdt-language-server-0.57.0-202006172108.tar.gz"
;;                           lsp-java-configuration-runtimes '[(:name "JavaCorretto-8"
;;                                                              :path "/Library/Java/JavaVirtualMachines/amazon-corretto-8.jdk/Contents/Home"
;;                                                              :default t)]
;;                           lsp-java-vmargs (list "-noverify" "--enable-preview")))

;; Java 11 Coretto w/Gradle
(use-package! lsp-java :after lsp
              :init (setq lsp-java-java-path "/Library/Java/JavaVirtualMachines/amazon-corretto-11.jdk/Contents/Home/bin/java"
                          lsp-java-import-gradle-java-home "/Library/Java/JavaVirtualMachines/amazon-corretto-11.jdk/Contents/Home/bin/java"
                          lsp-java-configuration-runtimes '[(:name "JavaCoretto-11"
                                                             :path "/Library/Java/JavaVirtualMachines/amazon-corretto-11.jdk/Contents/Home"
                                                             :default t)]
                          lsp-java-vmargs (list "-noverify" "--enable-preview")))
