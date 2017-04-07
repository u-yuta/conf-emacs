(setq load-path
      (append '(
                "~/.emacs.d/conf"
                ) load-path))

;(load "my-init")
(org-babel-load-file "~/.emacs.d/conf/my-init.org")
