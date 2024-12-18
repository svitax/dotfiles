(define-module (config home services emacs-config)
  #:use-module (config packages emacs)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:export (home-emacs-config-service-type))

(define (home-emacs-config-profile-service config)
  (list emacs
	emacs-guix
	+emacs-bind
	emacs-which-key
	emacs-fontaine
	emacs-modus-themes ; @4.5.0
	emacs-pulsar
	+emacs-cursory ; @1.1.0
    emacs-envrc
    emacs-inheritenv
	emacs-marginalia
	emacs-orderless
	emacs-vertico
	emacs-consult
	emacs-corfu
    emacs-cape
    ;; +emacs-projection
	emacs-popper
	emacs-avy
	+emacs-better-jumper
	+emacs-mowie ; @0.1.1
	emacs-move-text
	emacs-expand-region
	+emacs-multiple-cursors ; @1.4.0 | commit c870c18
    ;; +emacs-compile-multi
    ;; +emacs-consult-compile-multi
    ;; +emacs-projection-multi
	emacs-magit
	emacs-git-gutter
	emacs-git-gutter-fringe
	emacs-biblio
	+emacs-biblio-openlibrary
	+emacs-biblio-gbooks
	emacs-citar
	emacs-denote
	emacs-consult-denote
	emacs-citar-denote
	+emacs-org-remark ;; HACK get a commit with the user option
                      ;; `org-remark-report-no-highlights' until a new release
                      ;; is tagged
	emacs-nov-el
	))

(define home-emacs-config-service-type
  (service-type (name 'home-emacs-config)
		(description "A service for configuring Emacs.")
		(extensions
		 (list (service-extension
			home-profile-service-type
			home-emacs-config-profile-service)))
		(default-value #f)))
