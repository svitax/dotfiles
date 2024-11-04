(define-module (config home home-config)
	#:use-module (guix gexp)
	#:use-module (gnu)
	#:use-module (gnu packages emacs)
	#:use-module (gnu packages version-control)
	#:use-module (gnu home)
	#:use-module (gnu home services shells)
	#:use-module (gnu home services dotfiles)
	#:use-module (config home services emacs-config))

(home-environment
	(packages (list git))
	(services (list
		   (service home-emacs-config-service-type)
		   (service home-dotfiles-service-type
			    (home-dotfiles-configuration
			     (directories '("../../files"))))
		   (service home-bash-service-type
			    (home-bash-configuration
			     (environment-variables '(("EDITOR" . "emacs"))))))))
