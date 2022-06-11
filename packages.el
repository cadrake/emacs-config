;; Extra Modes
(package! dockerfile-mode)
(package! yaml-mode)
(package! terraform-mode)
(package! markdown-mode)
(package! web-mode)
(package! groovy-mode)
(package! jenkinsfile-mode)
(package! csv-mode)
(package! json-mode)
(package! protobuf-mode)
(package! graphviz-dot-mode)

;; Helpers
(package! smartscan)
(package! expand-region)
(package! vdiff)

;; UI Improvements
(package! all-the-icons)
(package! all-the-icons-ivy-rich)

;; Extensions
(package! org-super-agenda)
(package! org-jira)

;; My Stuff
;; Local nerd-icons for development
;; (package! nerd-icons
;;   :recipe (:local-repo "/Users/adrake/Code/elisp/nerd-icons.el"
;;            :build (:not compile)))
;; Github nerd-icons for production
(package! nerd-icons
  :recipe (:host github :repo "cadrake/nerd-icons.el"))
