(define-module (config packages emacs)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (guix licenses)
  #:export (emacs-mowie emacs-super-save))

;; TODO delete emacs-super-save because its already in guix/emacs-xyz
(define-public emacs-super-save
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

(define-public emacs-mowie
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
