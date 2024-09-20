;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Alex Drake"
      user-mail-address "alexd@cypress.io")

;; Special constant for WSL
(defconst IS-WINDOWS-WSL (and (eq system-type 'gnu/linux) (getenv "WSL_DISTRO_NAME")))

;; Load additional elisp directories
(add-to-list 'load-path (concat (getenv "DOOMDIR") "/custom"))
(add-to-list 'load-path (concat (getenv "DOOMDIR") "/extensions"))

;; Load extension scripts
(require 'auth-source)
(require 'subr+)

;; Enable disabled commands
(put 'downcase-region 'disabled nil)

;; Load my custom lisp files
;;(require 'nerd-icons-treemacs-theme)
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

;; Configure auth-source
(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))

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

;; Enable semantic mode and sticky functions
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(semantic-mode 1)

;; Consult ISearch
(global-set-key (kbd "C-s") 'consult-line)

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
(let ((backup-dir (concat (getenv "DOOMDIR") "/backups"))
      (auto-saves-dir (concat (getenv "DOOMDIR") "/auto-saves")))
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

;; Dired Icons
(add-hook! 'dired-mode-hook 'nerd-icons-dired-mode)

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
              org-agenda-files (directory-files-recursively "~/Documents/org" "\\.org")
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
(if (and (getenv "JIRALIB_HOST") (getenv "JIRALIB_USERNAME"))
   (progn
     (if (not (file-directory-p "~/.org-jira")) (make-directory "~/.org-jira"))
     (setq jiralib-url (format "https://%s" (getenv "JIRALIB_HOST"))
           jiralib-token (cons "Authorization" (format "Basic %s" (base64-encode-string (concat (getenv "JIRALIB_USERNAME") ":" (auth-source-pick-first-password :host (getenv "JIRALIB_HOST"))) t)))
           org-jira-done-states '("Canceled" "Done")
           org-agenda-files (append (directory-files-recursively "~/.org-jira" "\\.org") org-agenda-files))
     )
)

;; Associate yaml-mode with yaml files and enable whitespace
(use-package! yaml-mode
  :hook (yaml-mode . (lambda () "Show whitespace in yaml mode" (whitespace-mode 1)))
  :hook (yaml-mode . (lambda () "Disable word wrap" (visual-line-mode 0)))
  :mode "\\.yml\\'"
  :mode "\\.yaml\\'")

;; Json Mode configuration
(add-hook! 'json-mode-hook
  (lambda ()
    (make-local-variable 'js-indent-level)
    (setq js-indent-level 2)))
(setq-hook! 'json-mode-hook +format-with-lsp nil) ;; Json LSP overrides spacing

;; Typescript Mode config
(setq-hook! 'typescript-mode-hook typescript-indent-level 2)

;; Graphviz Mode Config
(use-package! graphviz-dot-mode
  :config
  (setq graphviz-dot-indent-width 4)
  :hook (graphviz-dot-mode . company-mode)
  :mode "\\.gv\\'")

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

;; Configure format-all-mode and hook into various editor modes
(use-package! format-all
  :commands format-all-mode
  :hook (rsjx-mode-hook . format-all-mode)
  :hook (python-mode-hook . format-all-mode)
  :hook (java-mode-hook . format-all-mode)
  :hook (json-mode-hook . format-all-mode))

;; Fix LSP and Molokai
(custom-set-faces!
  '(lsp-face-highlight-textual :background "color-18"))

;; Replace all-the-icons functions with their nerd-icons counterparts for terminal support
(with-eval-after-load 'all-the-icons
  (require 'nerd-icons)

  (defalias 'all-the-icons-insert 'nerd-icons-insert)
  (defalias 'all-the-icons-insert-faicon 'nerd-icons-insert-faicon)
  (defalias 'all-the-icons-insert-fileicon 'nerd-icons-insert-fileicon)
  (defalias 'all-the-icons-insert-material 'nerd-icons-insert-material)
  (defalias 'all-the-icons-insert-octicon 'nerd-icons-insert-octicon)
  (defalias 'all-the-icons-insert-wicon 'nerd-icons-insert-wicon)

  (defalias 'all-the-icons-icon-for-dir 'nerd-icons-icon-for-dir)
  (defalias 'all-the-icons-icon-for-file 'nerd-icons-icon-for-file)
  (defalias 'all-the-icons-icon-for-mode 'nerd-icons-icon-for-mode)
  (defalias 'all-the-icons-icon-for-url 'nerd-icons-icon-for-url)
  (defalias 'all-the-icons-icon-for-buffer 'nerd-icons-icon-for-buffer)
  (defalias 'all-the-icons-icon-family 'nerd-icons-icon-family)
  (defalias 'all-the-icons-icon-family-for-buffer 'nerd-icons-icon-family-for-buffer)
  (defalias 'all-the-icons-icon-family-for-file 'nerd-icons-icon-family-for-file)
  (defalias 'all-the-icons-icon-family-for-mode 'nerd-icons-icon-family-for-mode)

  (defalias 'all-the-icons-faicon (lambda (name &rest args) (ati-nerd-icons-adapter "faicon" name args)))
  (defalias 'all-the-icons-octicon (lambda (name &rest args) (ati-nerd-icons-adapter "octicon" name args)))
  (defalias 'all-the-icons-fileicon (lambda (name &rest args) (ati-nerd-icons-adapter "fileicon" name args)))
  (defalias 'all-the-icons-material (lambda (name &rest args) (ati-nerd-icons-adapter "material" name args)))
  (defalias 'all-the-icons-alltheicon (lambda (name &rest args) (ati-nerd-icons-adapter "alltheicon" name args)))
  (defalias 'all-the-icons-wicon (lambda (name &rest args) (ati-nerd-icons-adapter "wicon" name args))))

(defun ati-nerd-icons-adapter (icon-set icon-name &rest args)
  "Takes an all-the-icons icon set and name and returns the nerd-icons equivalent"
  (pcase icon-set
    ("faicon" (nerd-icons-faicon (format "nf-fa-%s" (to-nerd-name icon-name)) args))
    ("octicon" (nerd-icons-octicon (format "nf-oct-%s" (to-nerd-name icon-name)) args))
    ("fileicon" (nerd-icons-faicon (format "nf-fa-%s" (to-nerd-name icon-name)) args))
    ("material" (nerd-icons-mdicon (format "nf-md-%s" (to-nerd-name icon-name)) args))
    ("alltheicon" (nerd-icons-mdicon (format "nf-md-%s" (to-nerd-name icon-name))) args)
    ("wicon" (nerd-icons-wicon (format "nf-weather-%s" (to-nerd-name icon-name))) args)))

(defun to-nerd-name (name)
  "Transforms some all-the-icons named icons into nerd-icon equivalents"
  (if (string= name "light-bulb")
        (eval "light_bulb")
        (eval name)))

;; Debugging
(setq debug-on-error t)
;;(debug-on-entry 'all-the-icons-octicon)

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
;; (use-package! lsp-java :after lsp
;;               :init (setq       lsp-java-java-path "/Library/Java/JavaVirtualMachines/amazon-corretto-11.jdk/Contents/Home/bin/java"
;;                           lsp-java-import-gradle-java-home "/Library/Java/JavaVirtualMachines/amazon-corretto-11.jdk/Contents/Home/bin/java"
;;                           lsp-java-configuration-runtimes '[(:name "JavaCoretto-11"
;;                                                              :path "/Library/Java/JavaVirtualMachines/amazon-corretto-11.jdk/Contents/Home"
;;                                                              :default t)]
;;                           lsp-java-vmargs (list "-noverify" "--enable-preview")))

;; Java 17 w/Gradle
(use-package! lsp-java :after lsp
              :init (setq lsp-java-java-path "/Library/Java/JavaVirtualMachines/openjdk.jdk/Contents/Home/bin/java"
                          lsp-java-import-gradle-java-home "/Library/Java/JavaVirtualMachines/openjdk.jdk/Contents/Home/bin/java"
                          lsp-java-configuration-runtimes '[(:name "JavaSE-17"
                                                             :path "/Library/Java/JavaVirtualMachines/openjdk.jdk/Contents/Home"
                                                             :default t)]
                          lsp-java-vmargs (list "-noverify" "--enable-preview")))
