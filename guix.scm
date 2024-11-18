(use-modules (guix gexp)
             (guix packages)
             (guix build-system trivial)
             (gnu packages base)
             (gnu packages shellutils))

(package
  (name "dotfiles")
  (version "0.0.1")
  (source (local-file "."))
  (build-system trivial-build-system)
  (native-inputs
   `(("make" ,gnu-make)
     ("direnv" ,direnv)))
  (home-page "")
  (synopsis "")
  (description "")
  (license #f))
