(define-module (config packages sync)
  #:use-module (guix gexp)
  #:use-module (guix utils) ; cc-for-target
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages dlang)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages pkg-config)
  #:export (+onedrive))

(define-public +onedrive
  (package
    (name "onedrive")
    (version "2.5.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/abraunegg/onedrive")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0307qa3nncarn6r5837nn9z5nv8j60ycykq6pfn93qriabk65qlx"))))
    (build-system gnu-build-system)
    (arguments
     (list
       #:configure-flags
       #~(list "--enable-completions"
               "--enable-notifications"
               (string-append "--with-zsh-completion-dir="
                              #$output "/share/zsh/site-functions")
               (string-append "--with-fish-completion-dir="
                              #$output "/share/fish/vendor_completions.d"))
       #:make-flags
       #~(list (string-append "CC=" #$(cc-for-target)))
       #:phases
       #~(modify-phases %standard-phases
         (add-after 'configure 'adjust-makefile
           (lambda _
             (substitute* "Makefile"
               (("-O ") "-O2 "))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "./onedrive" "--version")))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list bash-minimal
           curl
           ldc
           libnotify
           sqlite))
    (home-page "https://abraunegg.github.io")
    (synopsis "Client for OneDrive")
    (description "OneDrive Client which supports OneDrive Personal, OneDrive for
Business, OneDrive for Office365 and SharePoint and fully supports Azure
National Cloud Deployments.  It supports one-way and two-way sync capabilities
and securely connects to Microsoft OneDrive services.")
    (license gpl3)))
