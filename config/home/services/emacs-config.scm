(define-module (config home services emacs-config)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (config packages emacs)
  #:export (home-emacs-config-service-type))

(define (home-emacs-config-profile-service config)
  (list emacs
	emacs-modus-themes
	emacs-mowie
	emacs-vertico))

(define home-emacs-config-service-type
  (service-type (name 'home-emacs-config)
		(description "A service for configuring Emacs.")
		(extensions
		 (list (service-extension
			home-profile-service-type
			home-emacs-config-profile-service)))
		(default-value #f)))
