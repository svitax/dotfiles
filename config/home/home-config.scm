(define-module (config home home-config)
  #:use-module (guix gexp)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages librewolf)
  #:use-module (gnu packages chromium)
  #:use-module (gnu packages version-control)
  #:use-module (gnu home)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services dotfiles)
  #:use-module (config packages sync)
  #:use-module (config packages node-xyz)
  #:use-module (config home services emacs-config))

;; TODO font-iosevka 2.1.0
(home-environment
 (packages (list git
                 font-iosevka
                 librewolf
                 ungoogled-chromium
                 ;; NOTE make a home-onedrive-service-type
                 ;; or use owncloud/nextcloud
                 +onedrive
                 ;; NOTE make a home-blocky-service-type
                 isc-bind ;; for nslookup
                 ))
 (services (list
            (service home-emacs-config-service-type)
            (service home-dotfiles-service-type
                     (home-dotfiles-configuration
                      (directories '("../../files"))))
            (service home-bash-service-type
                     (home-bash-configuration
                      (environment-variables '(("EDITOR" . "emacs"))))))))
