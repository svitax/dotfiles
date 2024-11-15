(define-module (config packages emacs)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (guix licenses)
  #:export (+emacs-super-save
	    +emacs-mowie
	    +emacs-cursory
	    +emacs-bind
	    +emacs-better-jumper))

;; TODO delete emacs-super-save because its already in guix/emacs-xyz
(define-public +emacs-super-save
  (let ((commit "886b5518c8a8b4e1f5e59c332d5d80d95b61201d")
	(revision "0"))
    (package
     (name "emacs-super-save")
     (version (git-version "0.3.0" revision commit))
     (source
      (origin
       (uri (git-reference
	     (url "https://github.com/bbatsov/super-save")
	     (commit commit)))
       (method git-fetch)
       (sha256
	(base32 "1w62sd1vcn164y70rgwgys6a8q8mwzplkiwqiib8vjzqn87w0lqv"))
       (file-name (git-file-name name version))))
     (build-system emacs-build-system)
     (home-page "https://github.com/bbatsov/super-save")
     (synopsis "Emacs package for automatic saving of buffers")
     (description
      "super-save auto-saves your buffers, when certain events happen: you
switch between buffers, an Emacs frame loses focus, etc. You can think of
it as an enhanced `auto-save-mode'")
     (license gpl3+))))

(define-public +emacs-mowie
  (let ((commit "5236a231c172ffe3a831bb649031f4a1aaec5b15")
	(revision "0"))
    (package
     (name "emacs-mowie")
     (version (git-version "0.1.1" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://codeberg.org/mekeor/mowie")
	     (commit commit)))
       (sha256
	(base32 "0kz0av456mzp3cblvkdwr6l6xwi9ibw7jhd7wjfypjajz5bzb9d1"))
       (file-name (git-file-name name version))))
     (build-system emacs-build-system)
     (home-page "https://codeberg.org/mekeor/mowie")
     (synopsis "Emacs package for cycling through point-moving commands")
     (description "mowie lets you define smart commands that cycle through the
result of point-moving commands by consecutive repetitions. The package also
offers a few point-moving commands")
     (license gpl3+))))

(define-public +emacs-cursory
  (let ((commit "b596eb53c8df051112d232daaebb47f52167fbe5")
	(revision "0"))
    (package
     (name "emacs-cursory")
     (version (git-version "1.1.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/protesilaos/cursory")
	     (commit commit)))
       (sha256
	(base32 "1jmhlppx9cihqjvq3vdln8ymgmayv04jiixm4jwwf1f9280fpv3z"))
       (file-name (git-file-name name version))))
     (native-inputs (list texinfo))
     (build-system emacs-build-system)
     (arguments
      (list
       #:phases
       #~(modify-phases %standard-phases
			(add-after 'install 'makeinfo
				   (lambda _
				     (invoke "emacs"
					     "--batch"
					     "--eval=(require 'ox-texinfo)"
					     "--eval=(find-file \"README.org\")"
					     "--eval=(org-texinfo-export-to-info)")
				     (install-file "cursory.info" (string-append #$output "/share/info")))))))
     (home-page "https://protesilaos.com/emacs/cursory")
     (synopsis "Manage Emacs cursor styles using presets")
     (description "Cursory provides a thin wrapper around built-in variables that affect
the style of the Emacs cursor on graphical terminals. The intent is to
allow the user to define preset configurations such as 'block with
slow blinking' or 'bar with fast blinking' and set them on demand. The
use-case for such presets is to adapt to evolving interface
requirements and concomitant levels of expected comfort, such as in
the difference between writing and reading.")
     (license (list gpl3+
		    fdl1.3+))))) ; GFDLv1.3+ for the manual

(define-public +emacs-multiple-cursors
  (let ((commit "c870c18462461df19382ecd2f9374c8b902cd804")
	(revision "0"))
    (package
     (name "emacs-multiple-cursors")
     (version (git-version "1.4.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/magnars/multiple-cursors.el")
	     (commit commit)))
       (sha256
	(base32 "1703ca0k0mlvjh7l0jv2nzgzah8ixb3n9av725cs2c07cih6vhsa"))
       (file-name (git-file-name name version))))
     (build-system emacs-build-system)
     (home-page "https://github.com/magnars/multiple-cursors.el")
     (synopsis "Multiple cursors for Emacs")
     (description "This package adds support to Emacs for editing text with multiple
simultaneous cursors")
     (license gpl3+))))

(define-public +emacs-bind
  (let ((commit "4c1698a7c1c9f3d45559c3be871d87d76a1cbe00")
	(revision "0"))
    (package
     (name "emacs-bind")
     (version (git-version "0.9.1" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/repelliuss/bind")
	     (commit commit)))
       (sha256
	(base32 "0dv2jgis2z4cbss346wxxnka6ig6w88rf7pnrd3v9xf8r2x4zqj6"))
       (file-name (git-file-name name version))))
     (build-system emacs-build-system)
     (home-page "https://github.com/repelliuss/bind")
     (synopsis "Emacs package to bind many commands to keys in many keymaps, multiple times.")
     (description "`bind' many commands to keys in many keymaps, multiple times and
support prefix, autoload, and repeat-mode.")
     (license gpl3+))))

(define-public +emacs-better-jumper
  (package
   (name "emacs-better-jumper")
   (version "1.0.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
	   "https://elpa.nongnu.org/nongnu/better-jumper-"
	   version
	   ".tar"))
     (sha256
      (base32 "1jdmbp1jjip8vmmc66z2wgx95lzp1b92m66p160mdm4g3skl64c2"))))
   (build-system emacs-build-system)
   (home-page "https://elpa.nongnu.org/nongnu/better-jumper.html")
   (synopsis "Emacs package for a configurable jump list.")
   (description "A configurable jump list implementation for Emacs that can be used to
easily jump back to previous locations.")
   (license gpl3+)))

(define-public +emacs-biblio-openlibrary
  (let ((commit "c3e349c5521e75c308a45216437850e528bdceed")
	(revision "0"))
    (package
     (name "emacs-biblio-openlibrary")
     (version (git-version "0.0.1" revision commit))
     (source
      (origin
       (uri (git-reference
	     (url "https://github.com/fabcontigiani/biblio-openlibrary")
	     (commit commit)))
       (method git-fetch)
       (sha256
	(base32 "0y626gj76p51sfyv2xq1kmj3k56by0yhkk5nxj1dfzd9jngfb80a"))
       (file-name (git-file-name name version))))
     (build-system emacs-build-system)
     (propagated-inputs (list emacs-biblio))
     (home-page "https://github.com/fabcontigiani/biblio-openlibrary")
     (synopsis "Emacs package for looking up and importing bibliographic entries from OpenLibrary.")
     (description
      "`biblio-openlibrary' provides a backend for `biblio.el', which allows you to easily search and retrieve bibliographic entries by ISBN using OpenLibrary's Read API.")
     (license gpl3+))))

(define-public +emacs-biblio-gbooks
  (let ((commit "c7bdaba4dde8fca8b8e923f3c004d050a32c06c2")
	(revision "0"))
    (package
     (name "emacs-biblio-gbooks")
     (version (git-version "1.0.0" revision commit))
     (source
      (origin
       (uri (git-reference
	     (url "https://github.com/jrasband/biblio-gbooks")
	     (commit commit)))
       (method git-fetch)
       (sha256
	(base32 "18fg3anm09bigv8zlb2hd3mf83kghf8261xjpklpxy77d80j7gv7"))
       (file-name (git-file-name name version))))
     (build-system emacs-build-system)
     (propagated-inputs (list emacs-biblio emacs-compat))
     (home-page "https://github.com/jrasband/biblio-gbooks")
     (synopsis "Emacs package for looking up and importing bibliographic entries from Google Books.")
     (description
      "`biblio-openlibrary' provides a backend for `biblio.el', which allows you to easily search and retrieve bibliographic entries using Google's Books API.")
     (license gpl3+))))


