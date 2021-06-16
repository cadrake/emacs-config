(package! dockerfile-mode)
(package! yaml-mode)
(package! terraform-mode)
(package! markdown-mode)
(package! web-mode)
(package! smartscan)
(package! expand-region)
(package! groovy-mode)
(package! jenkinsfile-mode)
(package! csv-mode)
(package! vdiff)
(package! json-mode)

(package! all-the-icons)
;; Local nerd-icons for development
;; (package! nerd-icons
;;   :recipe (:local-repo "/Users/adrake/Code/elisp/nerd-icons.el"
;;            :build (:not compile)))
;; Github nerd-icons for production
(package! nerd-icons
  :recipe (:host github :repo "cadrake/nerd-icons.el"))
;; (package! icons-in-terminal
;;   :recipe (:host github :repo "seagle0128/icons-in-terminal.el"))
