(package "sideline-flycheck"
         "0.1.0"
         "Show flycheck errors with sideline")

(website-url "https://github.com/emacs-sideline/sideline-flycheck")
(keywords "convenience" "flycheck")

(package-file "sideline-flycheck.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source "gnu")
(source "melpa")
(source "jcs-elpa")

(source-priority "melpa"    5)
(source-priority "jcs-elpa" 10)

(depends-on "emacs" "28.1")
(depends-on "sideline")
(depends-on "flycheck")
(depends-on "ht")

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
