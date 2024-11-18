;;; init.el --- This is my init. -*- lexical-binding: t; -*-

;;; Commentary:

;; This is where my Emacs config starts.

;;; Code:

;;;;;;;;;;;;;;;;;
;;;; startup ;;;;

;; NOTE document startup
(use-package startup
  :no-require
  :init
  (setopt inhibit-startup-screen t
          inhibit-startup-echo-area-message user-login-name))

;;;;;;;;;;;;;;
;;;; lisp ;;;;

;; Additional load paths
(eval-when-compile
  ;; "plugins/" contains downloaded packages or plugins I've written.
  (add-to-list 'load-path (concat user-emacs-directory "plugins")))

;;;;;;;;;;;;;;;;;;;;;
;;;; use-package ;;;;

(use-package use-package
  :init
  (setopt
   ;; `use-package-enable-imenu-support' must be set before requiring
   ;; use-package
   use-package-enable-imenu-support t
   ;; If you know your config works, this will make the byte-compiled file as
   ;; minimal as possible in exchange for harder use-package debugging sessions
   use-package-expand-minimally t))

;;;;;;;;;;;;;;;;;;;
;;;; bindings ;;;;;

;; NOTE document repeat
(use-package repeat
  :config
  (repeat-mode)
  (setopt repeat-exit-key "C-g"
          repeat-exit-timeout 10)
  ;; `repeat-mode' is great for many things, but `other-window' keeps
  ;; causing me trouble. I keep trying to switch to another window and
  ;; typing a word that begins with o or O. Let's disable repeat-mode
  ;; for other-window.
  (put 'other-window 'repeat-map nil)
  ;; `dired-jump' doesn't need a repeat map either.
  (put 'dired-jump 'repeat-map nil)
  ;; I always forget I'm in repeat-mode because I'm usually looking at point and
  ;; not at the echo area. This gives me a clue by updating the cursor color if
  ;; repeat-mode is active.
  (add-function :after repeat-echo-function
                (let ((default-cursor-color (face-background 'cursor)))
                  (lambda (map)
                    "Color the cursor while repeat-map is active"
                    (let ((cursor-indicator-color (face-foreground 'error))
                          (cursor-current-color (face-background 'cursor)))
                      (unless (equal cursor-current-color cursor-indicator-color)
                        (setq default-cursor-color cursor-current-color))
                      (set-cursor-color (if map
                                            cursor-indicator-color
                                          default-cursor-color)))))
                '((name . "colorful-cursor-while-repeating"))))

(use-package prefix
  :no-require
  :config
  ;; Define a set of keymaps with commonly used commands and put them behind the
  ;; mode-specific-map (`C-c') or the ctl-x-map (`C-x'). The idea is to hit a
  ;; series of keys to get the desired command. Keymaps are organised
  ;; thematically and rely on strong mnemonics, such as `b' for buffers, `w' for
  ;; windows, and so on.
  (defvar-keymap +bib-prefix-map
    :doc "Prefix keymap for bibliography."
    :prefix '+bib-prefix-map)
  (defvar-keymap +buffer-prefix-map
    :doc "Prefix keymap for buffers."
    :prefix '+buffer-prefix-map)
  (defvar-keymap +dap-prefix-map
    :doc "Prefix keymap for debugging."
    :prefix '+dap-prefix-map)
  (defvar-keymap +eval-prefix-map
    :doc "Prefix keymap for evaluating."
    :prefix '+eval-prefix-map)
  (defvar-keymap +file-prefix-map
    :doc "Prefix keymap for files."
    :prefix '+file-prefix-map)
  (defvar-keymap +guix-prefix-map
    :doc "Prefix keymap for Guix commands."
    :prefix '+guix-prefix-map)
  (defvar-keymap +mail-prefix-map
    :doc "Prefix keymap for mail."
    :prefix '+mail-prefix-map)
  (defvar-keymap +notes-prefix-map
    :doc "Prefix keymap for notes commands."
    :prefix '+notes-prefix-map)
  (defvar-keymap +project-prefix-map
    :doc "Prefix map for project."
    :prefix '+project-prefix-map)
  (defvar-keymap +registers-prefix-map
    :doc "Prefix map for registers."
    :prefix '+registers-prefix-map)
  (defvar-keymap +tab-prefix-map
    :doc "Prefix map for tabs."
    :prefix '+tab-prefix-map)
  (defvar-keymap +vc-prefix-map
    :doc "Prefix map for version control."
    :prefix '+vc-prefix-map)
  (defvar-keymap +window-prefix-map
    :doc "Prefix map for windows."
    :prefix '+window-prefix-map)
  (defvar-keymap +toggle-prefix-map
    :doc "Prefix map for minor mode toggles."
    :prefix '+toggle-prefix-map
    "f" #'flymake-mode
    "h" #'hl-line-mode
    ;; "k" #'keycast-mode-line-mode
    ;; "l" #'logos-focus-map
    "n" #'display-line-numbers-mode
    ;; "s" #'spacious-padding-mode
    ;; "r" #'rainbow-mode
    )

  (bind-keys
   :map ctl-x-map
   ;; ("C-a" . )
   ("b" . +buffer-prefix-map)
   ("C-b" . ibuffer) ; list-buffers
   ;; ("c" . org-capture)
   ("d" . dired-jump)
   ("C-d" . dired)
   ("e" . +eval-prefix-map)
   ("f" . +file-prefix-map)
   ("g" . +guix-prefix-map)
   ("h" . mark-whole-buffer)
   ;; ("i" . ) ; insert-file
   ("j" . +dap-prefix-map) ; i don't like dap/debug on j
   ;; ("C-j" . ) ; dired
   ("k" . kill-buffer)
   ("C-k" . kmacro-keymap)
   ("l" . +bib-prefix-map) ; "lib" mnemonic
   ;; ("C-l" . ) ; downcase-region
   ("m" . +mail-prefix-map)
   ("n" . +notes-prefix-map)
   ;; ("C-n" . ) ; set-goal-column
   ;; ("C-o" . delete-blank-lines)
   ("p" . +project-prefix-map)
   ;; "C-p" . mark-page
   ;; "q" ; kbd-macro-query
   ;; "C-q" . read-only-mode
   ("r" . +registers-prefix-map)
   ;; "C-r" . find-file-read-only
   ;; "s" ; save-some-buffers
   ;; ("C-s" . save-buffers)
   ("t" . +tab-prefix-map)
   ;; "C-t" . transpose-lines
   ;; "u" undo? undo-prefix?
   ;; ("C-u" . upcase-region) ; don't need
   ("v" . +vc-prefix-map)
   ;; "C-v" . find-alternate-file
   ("w" . +window-prefix-map)
   ;; "C-w" . write-file
   ("x" . +toggle-prefix-map)))

;; TODO replace with embark
(use-package which-key)

;;;;;;;;;;;;;;
;;;; guix ;;;;

(use-package guix
  :config
  (bind-keys
   :map +guix-prefix-map
   ("g" . guix)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; native-compilation ;;;;

(use-package native-comp
  :no-require
  :config
  ;; The default setting for reporting native compilation errors is set to a
  ;; verbose value which is confusing: it produces warnings for compilation
  ;; issues that only the developer of the given package needs to deal
  ;; with. These include innocuous facts like docstrings being wider than a
  ;; certain character count. To make things even worse, the buffer that shows
  ;; these warnings uses the stop sign character, resulting in a long list of
  ;; lines with red spots everywhere, as if we have totally broken Emacs.
  (when (native-comp-available-p)
    (setq native-comp-async-report-warnings-errors 'silent)
    (setq native-compile-prune-cache t)))

;;;;;;;;;;;;;;;
;;;; faces ;;;;

(use-package fontaine
  :if (display-graphic-p)
  :config
  ;; Define detailed font configurations and set them on command.
  (bind-keys
   :map +toggle-prefix-map
   ("f" . fontaine-set-preset))

  (setopt x-underline-at-descent-line nil
          text-scale-remap-header-line t
          fontaine-presets '((regular)
                             (presentation
                              :default-height 260)
                             (t
                              :default-family "Iosevka Comfy"
                              ;; font height is 1/10pt.
                              :default-height 150
                              :variable-family "Iosevka Comfy Motion Duo")))
  ;; Themes re-apply face definitions when they are loaded. This is necessary to
  ;; render the theme. For certain faces, such as `bold' and `italic', it means
  ;; that their font family may be reset (depending on the particularities of
  ;; the theme.)
  ;;
  ;; To avoid such a potential problem, we can arrange to restore the current
  ;; font preset which was applied by `fontaine-set-preset'. Fontaine provides
  ;; the command `fontaine-apply-current-preset'. It can be called interactively
  ;; after loading a theme or be assigned to a hook that is ran at the post
  ;; `load-theme' phase.
  ;;
  ;; `fontaine-mode' does this automatically, persisting the latest font preset
  ;; when closing/starting Emacs and while switching between themes.
  (fontaine-mode 1)

  ;; Set the last preset or fall back to desired style from `fontaine-presets'
  ;; (the `regular' in this case).
  (fontaine-set-preset 'regular))

(use-package variable-pitch
  :no-require
  :config
  ;; The built-in `variable-pitch-mode' makes the current buffer use a
  ;; proportionately spaced font. I want to activate it in all buffers where I
  ;; normally focus on prose. Exceptions to these major modes that I do not
  ;; consider related to prose (and which in my opinion should not be derived
  ;; from text-mode): these are excluded in the function
  ;; `+enable-variable-pitch'.
  (defun +enable-variable-pitch ()
    (unless (derived-mode-p 'mhtml-mode 'nxml-mode 'yaml-mode)
      (variable-pitch-mode 1)))
  (add-hook 'text-mode-hook #'+enable-variable-pitch)
  (add-hook 'notmuch-show-mode-hook #'+enable-variable-pitch)
  (add-hook 'elfeed-show-mode-hook #'+enable-variable-pitch)

  (bind-keys
   :map +toggle-prefix-map
       ("v" . variable-pitch-mode)))

(use-package modus-themes
  :config
  (setopt modus-themes-common-palette-overrides
          `((bg-region bg-sage)
            ;; With `modus-themes-preset-overrides-faint' the grays are toned
            ;; down, gray backgrounds are removed from some contexts, and almost
            ;; all accent colors are desaturated. Is makes the themes less
            ;; attention-grabbing.
            ,@modus-themes-preset-overrides-faint))

  ;; We use the `enable-theme-functions' hook to ensure that these values are
  ;; updated after we switch themes. This special hook available in Emacs 29+
  ;; works with anything that uses the basic `enable-theme' function.
  (defun +customize-theme-faces (&rest _)
    (modus-themes-with-colors
      (custom-set-faces
       ;; By default, the background of the `region' face extends from the end
       ;; of the line to the edge of the window. To limit it to the end of the
       ;; line, we need to override the face's `:extend' attribute.
       '(region ((t :extend nil)))
       ;; The `git-gutter' and `git-gutter-fr' packages default to drawing
       ;; bitmaps for the indicators they display (e.g. bitmap of a plus sign
       ;; for added lines). I replace these bitmaps with contiguous lines which
       ;; look nicer, but require a change to the foreground of the relevant
       ;; faces to yield the desired color combinations.
       `(git-gutter-fr:added ((,c :foreground ,bg-added-fringe :background ,fringe)))
       `(git-gutter-fr:deleted ((,c :foreground ,bg-removed-fringe :background ,fringe)))
       `(git-gutter-fr:modified ((,c :foreground ,bg-changed-fringe :background ,fringe))))))
  (add-hook 'enable-theme-functions #'+customize-theme-faces)

  (modus-themes-select 'modus-vivendi))

(use-package pulsar
  :config
  ;; Temporarily highlight the current line after a given function is invoked.
  ;; The affected functions are defined in the user option
  ;; `pulsar-pulse-functions'. What Pulsar does is set up an advice so that
  ;; those functions run a hook after they are called. The pulse effect is added
  ;; there (`pulsar-after-function-hook').
  (dolist (func '(beginning-of-buffer
                  end-of-buffer))
    (add-to-list 'pulsar-pulse-functions func))
  ;; There are convenience functions/commands which pulse the line using a
  ;; specific color: `pulsar-pulse-line-green' is one of them.
  (add-hook 'minibuffer-setup-hook #'pulsar-pulse-line-blue)
  (pulsar-global-mode 1))

(use-package cursory
  :if (display-graphic-p)
  :config
  ;; Cursory provides a thin wrapper around built-in variables that affect the
  ;; style of the Emacs cursor on graphical terminals. The intent is to allow the
  ;; user to define preset configurations such as 'block with slow blinking' or
  ;; 'bar with fast blinking' and set them on demand. The use-case for such
  ;; presets is to adapt to evolving interface requirements and concomitant levels
  ;; of expected comfort, such as in the difference between writing and reading.
  (cursory-set-preset (or (cursory-restore-latest-preset) 'box))
  (cursory-mode 1))

;;;;;;;;;;;;;;;;;;
;;;; modeline ;;;;

;;;;;;;;;;;;;;;;
;;;; frames ;;;;

(use-package frame
  :config
  (setopt frame-title-format '("%b")))

(use-package scratch
  :no-require
  :config
  (setopt initial-buffer-choice t
          initial-major-mode 'lisp-interaction-mode))

;;;;;;;;;;;;;;;
;;;; files ;;;;

;; NOTE document files
(use-package files
  :config
  (setopt y-or-n-p-use-read-key t
          use-short-answers t
          confirm-kill-processes nil
          confirm-kill-emacs 'yes-or-no-p)

  (bind-keys
   :map +file-prefix-map
   ("f" . find-file)
   ;; TODO add mark to xref before navigating to library
   ("b" . find-library)
   ("m" . man)))

(use-package backup
  :no-require
  :config
  ;; By default, Emacs tries to keep backups (i.e. some-file.el~). I do not need
  ;; this feature because all the files I care about are either under version
  ;; control or backed up to a flash drive.
  (setopt backup-inhibited t
          make-backup-files nil))

(use-package lockfiles
  :no-require
  :config
  ;; By default, Emacs tries to lock down files so that they are not modified by
  ;; other programs (i.e. .#some-file.el). I do not need this feature because if
  ;; I am ever modifying my files externally, then I know what I am doing
  (setopt create-lockfiles nil))

(use-package autorevert
  :config
  ;; The "auto-revert" facility makes Emacs update the contents of a saved
  ;; buffer when its underlying file is changed externally. This can happen, for
  ;; example, when a "git pull" modifies the file we are already displaying in a
  ;; buffer. Emacs thus automatically reverts the buffer to reflect the new file
  ;; contents.
  (global-auto-revert-mode))

;; TODO savehist

;;;;;;;;;;;;;;;;;;;;
;;;; minibuffer ;;;;

(use-package minibuffer
  :config
  ;; This makes it so that the minibuffer prompt is not accessible with regular
  ;; motions to avoid mistakes.
  (setopt minibuffer-prompt-properties
          '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Make it so prompts where more than one candidate can be provided using
  ;; completion show an indicator about this fact. Each candidate is separated
  ;; by the `crm-separator'. We display [`completing-read-multiple': <separator>],
  ;; e.g., [`completing-read-multiple': ,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[`completing-read-multiple': %s]  %s"
                  (propertize
                   (replace-regexp-in-string
                    "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                    crm-separator)
                   'face 'error)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator))

(use-package mb-depth
  :config
  ;; The need to have multiple (i.e. "recursive") minibuffers arises when you
  ;; initiate a command, such as M-x followed by some incomplete command where
  ;; remember that you forgot to perform another command before confirming the
  ;; first one. An example is the combination of M-x (execute-extended-command)
  ;; and M-: (eval-expression).
  (setopt enable-recursive-minibuffers t)
  ;; Shows a number next to the minibuffer prompt, indicating the level of depth
  ;; in the recursion, starting with 2
  (minibuffer-depth-indicate-mode))

(use-package minibuf-eldef
  :config
  ;; Minibuffer prompts often have a default value. This is used when the user
  ;; types `RET' without inputing anything. The out-of-the-box behaviour of
  ;; Emacs is to append informative text to the prompt like
  ;; `\(default some-default-value\)'. With this tweak to
  ;; `minibuffer-default-prompt-format' we get a more compact style of
  ;; `\[some-default-value\]', which looks better to me.
  (setopt minibuffer-default-prompt-format " [%s]")
  ;; Display the default value next to the prompt only if `RET' will actually
  ;; use the default in that situation. This means that while you start typing
  ;; in the minibuffer, the `[some-default-value]' indicator disappears, since
  ;; it is no longer applicable. Without this mode, the indicator stays there at
  ;; all times, which can be annoying or distracting.
  (minibuffer-electric-default-mode))

(use-package rfn-eshadow
  :config
  ;; `file-name-shadow-mode' is a neat little feature to dim or remove the
  ;; "shadowed" part of a file prompt while using something like find-file. File
  ;; name shadowing happens when we invoke find-file and instead of first
  ;; deleting the contents of the minibuffer, we start typing out the file
  ;; system path we wish to visit. For example, I am in `~/Git/Projects' and
  ;; type directly after it something like `~/.local/share/fonts/', so Emacs
  ;; displays `~/Git/Projects/~/.local/share/fonts/'.  With
  ;; `file-name-shadow-mode' the original "shadowed" part will be greyed out.
  ;; We can remove it altogether by applying the invisible property. This is
  ;; especially nice with the completion style called `partial-completion'.
  (setopt file-name-shadow-properties
          '(face file-name-shadow field shadow invisible t intangible t))
  (file-name-shadow-mode 1))

;;;;;;;;;;;;;;;;;;;;
;;;; marginalia ;;;;

;; NOTE document marginalia
(use-package marginalia
  :config
  (setopt marginalia-max-relative-age 0 ; absolute time
          marginalia-align 'right)
  (bind-keys :map minibuffer-local-map
             ("M-]" . marginalia-cycle))
  (marginalia-mode))

;;;;;;;;;;;;;;;;;;;
;;;; orderless ;;;;

(use-package orderless
  :init
  ;; NOTE these dispatchers aren't particularly useful but I leave this here as
  ;; an example of how to write and use them.
  ;; (setf (alist-get ?` orderless-affix-dispatch-alist) #'orderless-flex)
  ;; (setf (alist-get ?~ orderless-affix-dispatch-alist) #'+orderless-beg-or-end)
  ;; (setf (alist-get ?. orderless-affix-dispatch-alist) #'+orderless-file-ext)
  (defun +orderless-beg-or-end (component)
    "Match COMPONENT as a prefix or suffix string."
    (orderless-regexp (format "\\(^%s\\|%s$\\)" component component)))
  (defun +orderless-file-ext (component)
    "Match COMPONENT to a file suffix when completing file names."
    (when minibuffer-completing-file-name
      (orderless-regexp (format "\\.%s\\'" component))))
  :config
  ;; `basic' only matches candidates that have the same beginning as the text in
  ;; the minibuffer. It is required for /ssh: completion to work for TRAMP.
  ;;
  ;; `partial-completion' divides the minibuffer text into words separated by
  ;; hyphens or spaces, and completes each word separately. This is wonderful
  ;; for files because it can expand ~/.l/s/fo to ~/.local/share/fonts. It also
  ;; expands em-l-m to emacs-lisp-mode. Bear in mind we do not need to have
  ;; partial-completion first as basic will never match something like this.
  ;;
  ;; `orderless', which is the most powerful/flexible is placed last. I do this
  ;; because Emacs tries the styles in the given order from left to right,
  ;; moving until it finds a match. As such, I usually want to start with tight
  ;; matches (e.g. li-fa-di for list-faces-display) and only widen the scope of
  ;; the search as I need to. This is easy to do because none of the built-in
  ;; completion styles parses the empty space (the default
  ;; orderless-component-separator), so as soon as I type a space after some
  ;; characters I am using orderless.
  (setopt completion-styles '(basic partial-completion orderless))
  ;; While we can override only the categories we care about, the presence of
  ;; those `completion-category-defaults' will surprise us in some cases because
  ;; we will not be using what was specified in the `completion-styles'. As
  ;; such, I set `completion-category-defaults' to nil, to always fall back to
  ;; my preferred `completion-styles' and then I further configure overrides
  ;; where those make sense to me.
  (setopt completion-category-defaults nil)
  ;; We can opt for per-category styles by configuring the user option
  ;; `completion-category-overrides'.
  (setq completion-category-overrides
        ;; In order to narrow our Citar searches not only using citation keys
        ;; (i.e. using authors, titles, etc.), we need a completion style that
        ;; is order independent.
        '((citar-candidate (styles . (orderless basic))))))

;;;;;;;;;;;;;;;;;
;;;; vertico ;;;;

(use-package vertico
  :config
  (vertico-mode)
  (vertico-multiform-mode)
  ;; I don't like it when the minibuffer shows up eagerly. I find it disorienting
  ;; and motion sickness inducing when there are many things jumping around on my
  ;; screen at once, such as when the minibuffer constantly resizes itself or
  ;; pushes my windows around.
  ;;
  ;; The "multiform" mechanism of the `vertico' package allows us to change the
  ;; layout on a per-command or per-category basis. We can use this mechanism to
  ;; make the minibuffer not show up eagerly.
  (defvar +vertico-multiform-minimal
    '(unobtrusive
      (vertico-flat-format . ( :multiple ""
                               :single ""
                               :prompt ""
                               :separator ""
                               :ellipsis ""
                               :no-match "")))
    "List of configurations for minimal Vertico multiform.
The minimal view is intended to be less eager or less revealing
for general usage.

Toggle the vertical view with the `vertico-multiform-vertical'
command or use the commands `+vertico-minimal-next' and
`+vertico-minimal-previous', which toggle the vertical view
automatically.")
  (defvar +vertico-multiform-maximal
    '((vertico-count . 10))
    "List of configurations for maximal Vertico multiform.")
  (defun +vertico--match-directory (str)
    "Match directory delimeter in STR."
    (string-suffix-p "/" str))
  (defun +vertico-sort-directories-first (files)
    "Sort directories before FILES."
    (setq files (vertico-sort-alpha files))
    (nconc (seq-filter #'+vertico--match-directory files)
           (seq-remove #'+vertico--match-directory files)))
  (defun +vertico-minimal-next ()
    "Like `vertico-next' but toggle vertical view if needed.
This is done to accomodate `+vertico-multiform-minimal'."
    (interactive)
    (if vertico-unobtrusive-mode
        (let ((vertico--index 0))
          (vertico-multiform-vertical)
          (vertico-next 1))
      (vertico-next 1)))
  (defun +vertico-minimal-previous ()
    "Like `vertico-previous' but toggle vertical view if needed.
This is done to accomodate `+vertico-multiform-minimal'."
    (interactive)
    (if vertico-unobtrusive-mode
        (let ((vertico--index 0))
          (vertico-multiform-vertical)
          (vertico-previous 1))
      (vertico-previous 1)))
  (defun +vertico-minimal-complete ()
    "Expand contents and show remaining candidates, if needed.
This is dote to accomodate `+vertico-multiform-minimal'."
    (interactive)
    (if (and vertico-unobtrusive-mode
             (> vertico--total 1))
        (progn
          (minibuffer-complete)
          (vertico-multiform-vertical))
      (vertico-insert)))

  (setopt vertico-multiform-categories `(;; Maximal
                                         (embark-keybinding ,@+vertico-multiform-maximal)
                                         (imenu ,@+vertico-multiform-maximal)
                                         ;; Minimal
                                         (file ,@+vertico-multiform-minimal
                                               (vertico-preselect . prompt)
                                               (vertico-sort-function . +vertico-sort-directories-first))
                                         (t ,@+vertico-multiform-minimal))
          vertico-cycle t
          vertico-count 5)

  (with-eval-after-load 'rfn-eshadow
    ;; This works with `file-name-shadow-mode' enabled. When you are in
    ;; a sub-directory and use, say, find-file to go to your home '~/'
    ;; or root '/' directory, Vertico will also clear the old path to
    ;; keep only your current input.
    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

  (bind-keys
   :map vertico-map
   ("TAB" . +vertico-minimal-complete)
   ("DEL" . vertico-directory-delete-char)
   ("M-DEL" . vertico-directory-delete-word)
   :map vertico-multiform-map
   ("C-n" . +vertico-minimal-next)
   ("C-p" . +vertico-minimal-previous)
   ("<down>" . +vertico-minimal-next)
   ("<up>" . +vertico-minimal-previous)
   ("C-l" . vertico-multiform-vertical)))

;;;;;;;;;;;;;;;;;
;;;; consult ;;;;

(use-package consult
  :config
  ;; Provides a number of commands that turbocharge the minibuffer with advanced
  ;; capabilities for filtering, asynchronous input, and previewing of the
  ;; current candidate's context.
  ;;
  ;; A case where filtering is in use is the `consult-buffer' command. Anything
  ;; that defines a source for this interface can be filtered by typing the
  ;; appropriate narrow key and space in the empty minibuffer e.g. `b SPC' the
  ;; filter specific to buffers. Delete back to remove the `[Buffer]' filter and
  ;; insert another filter. Every multi-source command from `consult' relies on
  ;; this paradigm.
  ;;
  ;; Asynchronous input pertains to the intersection between Emacs and external
  ;; search programs. A case in point is `consult-grep', which calls the
  ;; system's `grep' program. The prompt distinguishes between what is sent to
  ;; the external program and what is only shown to Emacs by wrapping the former
  ;; inside of `consult-async-split-style' (`#' be default). So the input
  ;; `#my-#completion' will send `my-' to the `grep' program and then use
  ;; `completion' inside of the minibuffer to perform the subsequent
  ;; pattern-matching (e.g. with help from `orderless'. The part that is sent to
  ;; the external program does not block Emacs. It is handled asynchronously, so
  ;; everything stays responsive.
  ;;
  ;; As for previewing, `consult' commands show the context of the current match
  ;; and update the window as we move between completion candidates in the
  ;; minibuffer. For example, the `consult-line' command performs an in-buffer
  ;; search and lets us move between matches in the minibuffer while seeing in
  ;; the window above what the surrounding text looks like. This is an excellent
  ;; feature when we are trying to find something and do not quite remember all
  ;; the search terms to narrow down to it simply by typing at the minibuffer
  ;; prompt.  Unfortunately, the eager previewing can be disorienting when
  ;; moving quickly between candidates. Because of that I prefer to activate
  ;; preview with a key (`M-.') for certain sources.
  (consult-customize
   consult-bookmark consult--source-buffer consult-recent-file
   consult--source-recent-file consult--source-project-recent-file
   consult--source-bookmark consult--source-project-buffer
   ;; consult-denote-buffer-source consult-denote-subdirectory-source
   ;; consult-denote-silo-source consult-info
   :preview-key "M-."
   consult-theme
   :preview-key (list :debounce 0.3 "M-."))
  ;;
  ;; Use `consult-find-args' to specify slow directories to skip, like .git/,
  ;; .cache/, and node-modules.
  (setopt consult-find-args (concat "find . -not ( "
                                    "-path */.git* -prune "
                                    "-or -path */.cache* -prune "
                                    ")"))

  ;; NOTE document pulsar and consult integraon
  (with-eval-after-load 'pulsar
    (setq consult-after-jump-hook nil)
    (dolist (fn '(pulsar-recenter-center pulsar-reveal-entry))
      (add-hook 'consult-after-jump-hook fn)))

  (bind-keys
   :map global-map
   ("M-y" . consult-yank-pop)
   :map goto-map
   ("g" . consult-goto-line)
   ("i")
   :map search-map
   ("b" . consult-buffer)
   ("f" . consult-find) ; fd
   ("g" . consult-grep) ; rg
   ("h" . consult-history)
   ("i" . consult-imenu)
   ("l" . consult-line)
   ("m" . consult-mark)
   ("s" . consult-outline)
   :map +buffer-prefix-map
   ("b" . consult-buffer)
   :map consult-narrow-map
   ;; Available filters are displayed with the `consult-narrow-help' command at
   ;; the prompt
   ("?" . consult-narrow-help)))

;;;;;;;;;;;;;;;
;;;; corfu ;;;;

(use-package corfu
  :config
  ;; `corfu' handles in-buffer text completion splendidly using Emacs'
  ;; underlying infrastructure for `completion-at-point-functions'.
  ;;
  ;; However, automatic in-buffer text completion distracts me. I don't want
  ;; things eagerly popping in and out of my view. I want manual completion. Pop
  ;; up only when I say so. So completion is triggered with the `TAB' key,
  ;; producing a popup where the cursor is. See my `tabs' configuration.
  ;;
  ;; On that note, I set `corfu-preview-current' to nil because I don't want the
  ;; selected candidates to insert themselves into my buffer without my
  ;; direction, once again with the `TAB' key (`corfu-complete') while a `corfu'
  ;; popup is active.
  ;;
  ;; Since I am doing manual completion, that lets me use `SPC' for separator
  ;; insertion while a corfu popup is active. This means I get all the benefits
  ;; of `orderless' with `corfu'.
  ;;
  ;; `corfu-popupinfo-mode' will show a secondary documentation popup if we move
  ;; over a candidate but do not to anything with it.

  (global-corfu-mode)
  (corfu-popupinfo-mode 1)

  (setopt corfu-cycle t
          corfu-preview-current nil
          corfu-min-width 20
          corfu-popupinfo-delay '(0.25 . 0.25))

  ;; Sort by input history
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history))

  (defun +corfu-quit-and-newline ()
    "Quit Corfu completion and insert newline."
    (interactive)
    (progn
      (corfu-quit)
      (electric-newline-and-maybe-indent)))

  (bind-keys
   :map corfu-map
   ("C-h" . corfu-info-documentation)
   ("C-v" . corfu-popupinfo-scroll-up)
   ("M-v" . corfu-popupinfo-scroll-down)
   ("M-." . corfu-info-location)
   ("SPC" . corfu-insert-separator)
   ("TAB" . corfu-complete)
   ("RET" . +corfu-quit-and-newline)))

;;;;;;;;;;;;;;;;;
;;;; buffers ;;;;

;; NOTE document buffer
(use-package buffers
  :no-require
  :config
  (bind-keys
   :map +buffer-prefix-map
   ("c" . clone-indirect-buffer-other-window)
   ("g" . revert-buffer-quick)
   ("k" . +kill-current-buffer)
   ;; ("m" . +buffers-major-mode) ; (prot) if i can filter in consult-buffer by major mode i don't need this
   ("r" . +rename-file-and-buffer)
   ;; ("v" . +buffers-vc-root) ; (prot) if i can filter in consult-buffer by vc root i don't need this
   )

  (defun +kill-current-buffer (&optional arg)
    "Kill current buffer.
With optional prefix ARG (\\[universal-argument]) delete the
buffer's window as well."
    (interactive "P")
    (if (and arg (not (one-window-p)))
        (kill-buffer-and-window)
      (kill-buffer)))
  (defun +rename-file-and-buffer (name)
    "Apply NAME to current file and rename its buffer."
    (interactive
     (list (read-file-name "Rename current file: " (buffer-file-name))))
    (let ((file (buffer-file-name)))
      (if (vc-registered file)
          (vc-rename-file file name)
        (rename-file file name))
      (set-visited-file-name name t t))))

;;;;;;;;;;;;;;;;;
;;;; windows ;;;;

(use-package window
  :no-require
  :config
  (bind-keys
   :map global-map
   ("M-o" . other-window)
   :map +window-prefix-map
   ("0" . delete-window)
   ("1" . delete-other-windows)
   ("2" . split-window-below)
   ("3" . split-window-right)
   ("o" . other-window)
   ("^" . tear-off-window)
   ;; ("f" . +toggle-window-split) ; toggle windows between horizontal and vertical (lambda emacs)
   ;; ("r" . +rotate-windows) ; (lambda emacs)
   ;; ("R" . +rotate-windows-backward) ; (lambda emacs)
   ("k" . delete-window)
   ("K" . delete-other-windows)
   ("s" . split-window-below)
   ("S" . +split-window-below-and-focus) ; (lambda emacs)
   ("v" . split-window-right)
   ;; ("V" . +split-window-right-and-focus) ; (lambda emacs)
   ;; ("w" . ace-window)
   ("y" . windmove-left)
   ("h" . windmove-down)
   ("a" . windmove-up)
   ("e" . windmove-right)
   ("Y" . windmove-swap-states-left)
   ("H" . windmove-swap-states-down)
   ("A" . windmove-swap-states-up)
   ("E" . windmove-swap-states-right)))

;; NOTE document popper
(use-package popper
  :config
  (defvar +help-modes-list '(helpful-mode
                             help-mode
                             "\\*Help\\*"
                             "^\\*eldoc"
                             apropos-mode)
    "List of major modes used in documentation buffers.")
  (defvar +man-modes-list '(Man-mode woman-mode)
    "List of major modes used in Man-type buffers.")
  (defvar +message-modes-list '(compilation-mode
                                messages-buffer-mode
                                edebug-eval-mode)
    "List of major modes used in message buffers.")
  (defvar +notes-names-list '("\\*marginal notes\\*")
    "List of buffer names used in Org-Remark marginal notes buffers.")
  (defvar +occur-grep-modes-list '(occur-mode
                                   grep-mode
                                   xref--xref-buffer-mode
                                   locate-mode
                                   flymake-diagnostics-buffer-mode
                                   rg-mode)
    "List of major modes used in occur-type buffers.")
  ;; This does not work at buffer creation since the major mode for REPLs is not
  ;; yet set when `display-buffer' is called, but it is useful afterwards.
  (defvar +repl-modes-list '(matlab-shell-mode
                             geiser-repl-mode
                             inferior-python-mode
                             cider-repl-mode
                             fennel-repl-mode
                             jupyter-repl-mode
                             inferior-ess-julia-mode
                             eshell-mode
                             shell-mode
                             eat-mode
                             vterm-mode)
    "List of major modes used in REPL buffers.")
  (defvar +repl-names-list '("^\\*\\(?:.*?-\\)\\{0,1\\}e*shell[^z-a]*\\(?:\\*\\|<[[:digit:]]+>\\)$"
                             "\\*.*REPL.*\\*"
                             "\\*MATLAB\\*"
                             "\\*Python\\*"
                             "^\\*jupyter-repl.*?\\(\\*\\|<[[:digit:]]>\\)$"
                             "\\*Inferior .*\\*$"
                             "^\\*julia.*\\*$"
                             "^\\*cider-repl.*\\*$"
                             "^\\*vterm.*\\*$"
                             "\\*ielm\\*"
                             "\\*edebug\\*")
    "List of buffer names used in REPL buffers.")
  (defvar +shell-command-names-list '("\\*Shell Command Output\\*"
                                      "\\*Async Shell Command\\*"
                                      "\\*Detached Shell Command\\*" )
    "List of buffer names used in Shell Command buffers.")

  (setopt popper-reference-buffers (append +help-modes-list
                                           +man-modes-list
                                           +message-modes-list
                                           +notes-names-list
                                           +occur-grep-modes-list
                                           +repl-modes-list
                                           +repl-names-list
                                           +shell-command-names-list
                                           '(("^\\*Warnings\\*$" . hide)
                                             "[Oo]utput\\*$"
                                             "\\*Completions\\*")))

  ;; Set popper height to 30% of frame height
  (setopt popper-window-height (lambda (win)
                                 (fit-window-to-buffer
                                  win
                                  (floor (frame-height) 3)
                                  (floor (frame-height) 3))))
  (popper-mode)
  (bind-keys
   :map global-map
   ("C-`" . popper-toggle-latest)
   ("M-`" . popper-cycle)
   ("C-M-`" . popper-toggle-type)))

;; NOTE document golden-ratio-scroll
(use-package golden-ratio-scroll
  :no-require
  :config
  (bind-keys
   :map global-map
   ("C-v" . +golden-ratio-scroll-screen-down)
   ("M-v" . +golden-ratio-scroll-screen-up))

  (setq-default scroll-preserve-screen-position t
                scroll-conservatively 1
                scroll-margin 0
                next-screen-context-lines 0)

  (defcustom +golden-ratio-scroll-recenter nil
    "Recenter or not after scroll"
    :type 'boolean)
  (defcustom +golden-ratio-scroll-screen-ratio 1.618
    "Forward or backward (window-text-height)/<this-value> lines"
    :type 'number)
  (defvar +golden-ratio-scroll-screen-previous-point (point-marker))
  (defun +golden-ratio-scroll-screen-down ()
    "Scroll half screen down."
    (interactive)
    (let ((old-marker +golden-ratio-scroll-screen-previous-point)
          (scroll-line-count (round (/ (window-text-height)
                                       +golden-ratio-scroll-screen-ratio))))
      (setq +golden-ratio-scroll-screen-previous-point (point-marker))
      (if (and (not (and (equal (current-buffer) (marker-buffer old-marker))
                         (equal (marker-position old-marker) (point))))
               (equal last-command '+golden-ratio-scroll-screen-up))
          (goto-char (marker-position old-marker))
        (forward-visible-line scroll-line-count))
      (when (and (member major-mode '(dired-mode wdired-mode))
                 (equal (point-max) (point)))
        (dired-previous-line 1))
      (when +golden-ratio-scroll-recenter
        (recenter (+ scroll-line-count (/ (- (window-text-height) scroll-line-count) 2))))))
  (defun +golden-ratio-scroll-screen-up ()
    "Scroll half screen up."
    (interactive)
    (let ((old-marker +golden-ratio-scroll-screen-previous-point)
          (scroll-line-count (round (/ (window-text-height)
                                       +golden-ratio-scroll-screen-ratio))))
      (setq +golden-ratio-scroll-screen-previous-point (point-marker))
      (if (and (not (and (equal (current-buffer) (marker-buffer old-marker))
                         (equal (marker-position old-marker) (point))))
               (equal last-command '+golden-ratio-scroll-screen-down))
          (goto-char (marker-position old-marker))
        (forward-visible-line (- 0 scroll-line-count)))
      (when (and (member major-mode '(dired-mode wdired-mode))
                 (equal (point-min) (point)))
        (dired-next-line 2))
      (when +golden-ratio-scroll-recenter
        (recenter (/ (- (window-text-height) scroll-line-count) 2)))))

  (with-eval-after-load 'pulsar
    (dolist (func '(+golden-ratio-scroll-screen-up
                    +golden-ratio-scroll-screen-down))
      (add-to-list 'pulsar-pulse-functions func))))

(use-package display-line-numbers
  :config
  ;; I do not like to see line numbers by default and seldom use
  ;; `display-line-numbers-mode'. They do not help me navigate a buffer, nor are
  ;; they relevant in most cases. I enable the mode only when I need to compare
  ;; buffers or get a sense of how far apart two relevant sections are in a
  ;; file.
  ;;
  ;; Use absolute numbers in narrowed buffers.
  (setq-default display-line-numbers-widen t)

  (bind-keys
   :map +toggle-prefix-map
   ("n" . global-display-line-numbers-mode)))

(use-package whitespace
  :config
  ;; Emacs has very comprehensive whitespace rendering capabilities. I do not
  ;; render newline and space characters (see my tab configuration) because they
  ;; are easy to infer in most cases, but also because `whitespace-mode'
  ;; highlights each whitespace with a face which can cripple performance in
  ;; larger files. Since I only render trailing whitespace, empty lines, and tab
  ;; characters to draw attention to fix these mistakes, this ends up not
  ;; mattering as much.
  (setopt whitespace-style '(face tabs tab-mark trailing empty))

  ;; `whitespace-mode' provides the actions feature which allows us to
  ;; automatically run a series of actions after a buffer is written. I'm
  ;; interested in the cleanup actions which perform different operations based
  ;; on the defined whitespace style. For my defined whitespace style, it will
  ;; remove all empty lines at beginning and/or end of the buffer (`empty'), and
  ;; all trailing tabs and spaces (`trailing'). Lookup `whitespace-cleanup' for
  ;; all the available cleanup operations.
  (setq-default whitespace-action '(cleanup auto-cleanup))

  ;; We can enable whitespace mode globally by calling
  ;; `global-whitespace-mode'. The downside of this is that whitespace will be
  ;; rendered inside of every Emacs buffer and this is not really necessary. For
  ;; example, I don't need whitespace to be rendered in shell, occur, or ibuffer
  ;; windows. Luckily there's an option to control which modes should enable
  ;; whitespace mode when `global-whitespace-mode' is enabled. And it's aptly
  ;; named `whitespace-global-modes'. This option takes a list of major mode
  ;; symbol names, that when matched, will enable `whitespace-mode'. We can also
  ;; negate the list, by prefixing it with `not', causing global whitespace mode
  ;; to be disabled for the listed major mode symbols.
  (setq-default whitespace-global-modes '(prog-mode text-mode))
  (global-whitespace-mode))

;;;;;;;;;;;;;;;;
;;;; narrow ;;;;

(use-package narrow
  :no-require
  :config
  (bind-keys
   :map +buffer-prefix-map
   ("d" . narrow-to-defun)
   ;; ("g" . goto-line-relative) ; if narrowed, make "M-g g" do goto-line-relative instead
   ("n" . narrow-to-region)
   ("p" . narrow-to-page)
   ("w" . widen)))

;;;;;;;;;;;;;;;;;;;;
;;;; navigation ;;;;

(use-package avy
  :config
  (setopt avy-keys '(?n ?r ?t ?s ?h ?a ?e ?i) ; Graphite keyboard layout
          avy-timeout-seconds 0.27
          avy-single-candidate-jump nil)
  (bind-keys
   :map global-map
   ("C-j" . avy-goto-char-timer)))

(use-package dogears
  :load-path "plugins/dogears/"
  :config
  (dogears-mode)
  (setopt dogears-idle nil
          dogears-hooks '(imenu-after-jump-hook
                          consult-after-jump-hook))
  (bind-keys
   :map goto-map
   ("b" . dogears-back)
   ("f" . dogears-forward)
   ("d" . dogears-list)
   ("j" . dogears-go)
   :repeat-map dogears-repeat-map
   ("b" . dogears-back)
   ("f" . dogears-forward)))

;; FIXME either add dogears-list, dogears-go, and dogears-sidebar to
;; `better-jumper' or add better-jumper-add-jump-behavior 'replace to
;; `dogears'
;;
;; FIXME don't let `better-jumper-set-jump' set jump if current jump
;; is at same point/line
(use-package better-jumper
  :disabled t
  :config
  (better-jumper-mode +1)

  (setopt better-jumper-add-jump-behavior 'replace
          better-jumper-context 'buffer)

  (defun +better-jumper--position-is-newest-jump (orig-pos)
    (let* ((jump-struct (better-jumper-get-jumps))
           (ring (better-jumper-jump-list-struct-ring jump-struct))
           (jumps-vector (cddr ring))
           (newest-jump (aref jumps-vector 0))
           (filename (nth 0 newest-jump))
           (position (nth 1 newest-jump)))
      (and (string= (buffer-file-name) filename)
           (= orig-pos position))))

  (defun +better-jumper-advice (orig-fun &rest args)
    "Advice for ORIG-FUN to call `better-jumper-set-jump' if ORIG-FUN
moves the point more than one line."
    (let ((orig-pos (point)))
      (apply orig-fun args)
      (when (> (abs (- (line-number-at-pos orig-pos)
                       (line-number-at-pos (point))))
               1)
        (better-jumper-set-jump orig-pos))))

  (with-eval-after-load 'consult
    (advice-add 'consult-imenu :around #'+better-jumper-advice))

  (bind-keys
   :map goto-map
   ("b" . better-jumper-jump-backward)
   ("f" . better-jumper-jump-forward)
   :repeat-map better-jumper-repeat-map
   ("b" . better-jumper-jump-backward)
   ("f" . better-jumper-jump-forward)))

;;;;;;;;;;;;;;;;;
;;;; editing ;;;;

(use-package editing
  :no-require
  :config
  (defun +open-line-above ()
    "Insert a new line above current line."
    (interactive)
    (back-to-indentation)
    (split-line))
  (defun +open-line-below ()
    "Insert a new line below current line."
    (interactive)
    (end-of-line)
    (open-line 1)
    (next-line)
    (indent-according-to-mode))

  (defun +keyboard-quit-dwim ()
    "Do-What-I-Mean for a general `keyboard-quit'.
The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open. Whereas we want it to close the minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When a minibuffer is open, but not focused, close the minibuffer.
- In every other case use the regular 'keyboard-quit'."
    (interactive)
    (cond
     ((> (minibuffer-depth) 0)
      (abort-recursive-edit))
     (t
      (keyboard-quit))))

  (defun +duplicate--buffer-substring (boundaries)
    "Duplicate buffer substring between BOUNDARIES.
BOUNDARIES is a cons cell representing buffer positions."
    (unless (consp boundaries)
      (error "`%s' is not a cons cell" boundaries))
    (let ((beg (car boundaries))
          (end (cdr boundaries)))
      (goto-char end)
      (newline)
      (insert (buffer-substring-no-properties beg end))))
  (defun +duplicate-dwim ()
    "Duplicate the current line or active region."
    (interactive)
    (unless mark-ring ; needed when entering a new buffer
      (push-mark (point) t nil))
    (+duplicate--buffer-substring
     (if (region-active-p)
         (cons (region-beginning) (region-end))
       (cons (line-beginning-position) (line-end-position)))))

  (defun +join-line-below ()
    "Join the current line with the line beneath it."
    (interactive)
    (delete-indentation 1))

  (defun +comment-dwim (n)
    "Comment N lines, defaulting to the current one.
When the region is active, comment its lines instead."
    (interactive "p")
    (if (use-region-p)
        (comment-or-uncomment-region (region-beginning) (region-end))
      (comment-line n)))

  ;; TODO I would rather replace this with something like dogears.el
  ;; Make Emacs repeat the C-u C-SPC command (`set-mark-command') by
  ;; following it up with another C-SPC. It is faster to type C-u
  ;; C-SPC, C-SPC, C-SPC, than C-u C-SPC, C-u C-SPC, C-u C-SPC...
  (setopt set-mark-command-repeat-pop t)

  (bind-keys
   :map global-map
   ;; `+save-next-kill' causes the following command, if it kills, to save in
   ;; the kill ring instead. With prefix argument has same behavior as
   ;; `append-next-kill', which adds to previous kill.
   ;; ("C-M-w" . +save-next-kill)

   ;; `+keyboard-quit-dwim' closes and open but unfocused minibuffer.
   ("C-g" . +keyboard-quit-dwim)

   ;; `+duplicate-dwim' will duplicate the region if active, otherwise the
   ;; current line.
   ("C-M-y" . +duplicate-dwim)

   ;; The default `delete-char' doesn't respect the values of
   ;; `delete-active-region'. Make it so `C-d' deletes the region if active.
   ("C-d" . delete-forward-char)

   ;; Open new lines similar to Vim's o and O commands.
   ("C-o" . +open-line-below)
   ("C-M-o" . +open-line-above)

   ;; Join the current line with the line below it similar to Vim's J command.
   ("M-j" . +join-line-below)

   ;; The `+comment-dwim' command is like the built-in `comment-dwim', but
   ;; toggles linewise commenting instead of appending them by default.
   ("M-;" . +comment-dwim)))

(use-package fill-mode
  :no-require
  :config
  ;; `auto-fill-mode' automatically breaks long lines so that they wrap at the
  ;; `fill-column' length. This way, a paragraph is not a single long line, but
  ;; several shorter lines with newline characters between them. Often times
  ;; this is more pleasant to work with instead of having to rely on
  ;; `visual-line-mode' to visually wrap long lines. Relevant programs strip
  ;; away the newlines inside a paragraph, but there are some that do not. For
  ;; those I might rely upon `virtual-auto-fill-mode'.
  (add-hook 'text-mode-hook #'auto-fill-mode)
  (setopt fill-column 80))

;; NOTE document mowie
(use-package mowie
  :config
  (defun +beginning-of-line ()
    (interactive "^")
    (mowie
     #'beginning-of-line
     #'beginning-of-visual-line
     #'mowie-beginning-of-code
     #'mowie-beginning-of-comment
     #'mowie-beginning-of-comment-text))
  (defun +end-of-line ()
    (interactive "^")
    (mowie
     #'end-of-line
     #'end-of-visual-line
     #'mowie-end-of-code))
  (bind-keys ("C-a" . +beginning-of-line)
             ("C-e" . +end-of-line)))

;; NOTE document move-text
(use-package move-text
  :config
  ;; Re-indent the text in and around a text move.
  (defun +indent-region-advice (&rest ignored)
    (let ((deactivate deactivate-mark))
      (if (region-active-p)
          (indent-region (region-beginning) (region-end))
        (indent-region (line-beginning-position) (line-end-position)))
      (setq deactivate-mark deactivate)))
  (advice-add 'move-text-up :after '+indent-region-advice)
  (advice-add 'move-text-down :after '+indent-region-advice)

  (bind-keys
   :map global-map
   ("M-<up>" . move-text-up)
   ("M-<down>" . move-text-down)))

(use-package tabs
  :no-require
  :config
  ;; `TAB' in Emacs tries to be smart. Instead of inserting tabs, or spaces, it
  ;; tries to indent the current line to where it should be given the context
  ;; and depending on the major mode. This works best when we mark a region of
  ;; text and hit `TAB' there.
  ;;
  ;; If we need to forcefully indent, we can use `indent-rigidly' (`C-x\ C-i' by
  ;; default). This command allows us to shift a region left or right using the
  ;; arrow keys. A common use-case for me is to paste some text I want to
  ;; indent, and then do `C-u C-x C-i', which indents by four spaces the
  ;; implicit region.
  ;;
  ;; `tab-always-indent' makes the `TAB' key assume the dual role of indenting
  ;; text as well as triggering completion at point. (See my `corfu'
  ;; configuration). When it can perform indentation, it does that, otherwise it
  ;; starts a completion loop. The `tab-first-completion' determines when not to
  ;; complete. In my case complete unless the next character is part of a word.
  ;; Typing `TAB' a second time always results in completion.
  (setopt tab-always-indent 'complete
          tab-first-completion 'word)

  ;; `tab-width' and `indent-tabs-mode' are about the use of tabs. I never want
  ;; them, as I only use spaces.
  (setq-default tab-width 4
                indent-tabs-mode nil))

(use-package elec-pair
  :config
  ;; Emacs describes as "electric" any behaviour that tries to be smart about
  ;; how to handle a given action. The `electric-pair-mode', for example,
  ;; automatically inserts a closing parenthesis.
  (electric-pair-mode))

(use-package delsel
  :config
  ;; Every graphical application I have ever used will delete the selected text
  ;; upon the insertion of new text. Emacs does not do this by default. With
  ;; `delete-selection-mode' we get it.
  (delete-selection-mode +1))

;; NOTE document smart-hungry-delete
;; NOTE possibly create +smart-hungry-delete-forward/backward-char
;; with electric-pair-mode and delsel integration
(use-package smart-hungry-delete
  :disabled t
  :config
  (smart-hungry-delete-add-default-hooks)
  (bind-keys
   :map global-map
   ([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
   ([remap delete-backward-char] . smart-hungry-delete-backward-char)
   ([remap delete-char] . smart-hungry-delete-forward-char)
   ([remap delete-forward-char] . smart-hungry-delete-forward-char)))

(use-package expand-region
  :config
  ;; The `expand-region' package expands the region from smallest to the largest
  ;; syntactic unit in the give context. If Emacs is built with tree-sitter
  ;; support and we are running a major mode that is designed for tree-sitter,
  ;; it will use the tree-sitter framework to determine the syntactic units.
  ;;
  ;; I almost never use `er/contract-region'. I often find myself just
  ;; aborting and starting over again with `er/expand-region' in the
  ;; scenario where I widened the selection more than I should.
  (bind-keys
   :map global-map
   ("M-h" . er/expand-region) ; overrides mark-paragraph
   ("C-M-h" . er/contract-region) ; overrides mark-defun
   ("C-M-SPC" . er/expand-region) ; overrides mark-sexp
   ("C-M-S-SPC" . er/contract-region))

  (setopt expand-region-fast-keys-enabled nil))

(use-package multiple-cursors
  :config
  ;; The approach that `multiple-cursors' uses to create fake cursors doesn't
  ;; play nice with `cursory'. I suppose this might be a technical limitation of
  ;; Emacs, but the fake cursors don't inherit directly from my cursor type, and
  ;; instead place an overlay with either the "|" or " " characters.
  ;; Unfortunately if you set your cursor type to bar, it tries to render the
  ;; fake cursors as literal "|" characters with the mc/cursor-bar-face. This
  ;; approach distorts lines and just plainly doesn't work, so I disable it.
  (setopt mc/match-cursor-style t)

  (defvar-keymap mc-mark-map
    :doc "multiple-cursors mark map."
    :prefix 'mc-mark-map)
  (bind-keys
   :map global-map
   ("C-;" . mc-mark-map)
   :map mc-mark-map
   ("." . mc/mark-all-like-this-dwim)
   ("e" . mc/edit-ends-of-lines)
   ("a" . mc/edit-beginnings-of-lines)
   ("C-'" . mc-hide-unmatched-lines-mode)
   ("n" . mc/mark-next-like-this-symbol)
   ("p" . mc/mark-previous-like-this-symbol)
   ("C-n" . mc/mark-next-lines)
   ("C-p" . mc/mark-previous-lines)
   (">" . mc/skip-to-next-like-this)
   ("<" . mc/skip-to-previous-like-this)
   :repeat-map mc-mark-repeat-map
   ("n" . mc/mark-next-like-this-symbol)
   ("p" . mc/mark-previous-like-this-symbol)
   ("C-n" . mc/mark-next-lines)
   ("C-p" . mc/mark-previous-lines)
   (">" . mc/skip-to-next-like-this)
   ("<" . mc/skip-to-previous-like-this)))

;;;;;;;;;;;;;;;;;
;;;; compile ;;;;

;; NOTE document compile
(use-package compile
  :config
  ;; NOTE `compilation-filter-hook' is a set of filters to be applied to the
  ;; output of our compiler.

  ;; Automatically scroll build output
  (setopt compilation-scroll-output t)
  ;; Kill compilation process before starting another.
  (setopt compilation-always-kill t)
  ;; Translate ANSI escape sequences into faces
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

  (bind-keys
   :map +project-prefix-map
   ("," . project-compile)
   :map ctl-x-map
   ("," . compile)
   ("." . recompile)))

;;;;;;;;;;;;;;
;;;; prog ;;;;

(use-package xref
  :config
  ;; `xref' provides infrastructure to jump to and from a definition. For
  ;; example, with point over a function, call `xref-find-definitions' will jump
  ;; to the file where the function is defined or provide an option to pick one
  ;; among multiple definitions, where applicable.
  ;;
  ;; Use Consult to select xref locations with preview.
  (setopt xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref))

;;;;;;;;;;;;
;;;; vc ;;;;

;; NOTE document magit
(use-package magit
  :config
  (setopt magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  (bind-keys
   :map +vc-prefix-map
   ("v" . magit-status)))

;;;;;;;;;;;;;;
;;;; diff ;;;;

;; I would like to lean into native/built-in Emacs functionality where it's
;; equal or better than the third-party alternatives. `diff-hl' relies on the
;; built-in `vc.el' library instead of talking to git directly (thus expanding
;; support to whatever VCs vc.el supports, and not git alone), which also means
;; it can take advantage of its caching and other user configuration for
;; vc.el. Overall, it should be faster and lighter.
;;
;; However, everytime I have tried to use diff-hl, it has been buggy or slow to
;; refresh on changes. It still has issues with Magit altering the git state. It
;; is also easier to redefine fringe bitmaps for git-gutter than it is for
;; diff-hl.
;;
;; Doom Emacs has a lot of configuration code for diff-hl that I might look into
;; incorporating someday. In the meantime I'll keep using git-gutter.

(use-package git-gutter
  :config
  ;; `git-gutter' and `git-gutter-fringe' use the margins or fringes to
  ;; highlight changes in the current buffer. The indicators are colour-coded to
  ;; denote whether a change is an addition, removal, or change that includes a
  ;; bit of both.
  ;;
  ;; This package offers some more features, such as the ability to move between
  ;; diff hunks while editing the buffers. I still need to experiment with those
  ;; before customizing them to my liking.
  (add-hook 'prog-mode-hook #'git-gutter-mode)
  ;; The `git-gutter:update-interval' customizable variable was defined with
  ;; type 'integer, but I like it between 0.3 and 0.5 so I redefine it with type
  ;; 'number.
  (defcustom git-gutter:update-interval 0
  "Time interval in seconds for updating diff information."
  :type 'number
  :group 'git-gutter)
  (setopt git-gutter:update-interval 0.5))

(use-package git-gutter-fringe
  :config
  (setopt git-gutter-fr:side 'left-fringe)
  ;; Redefine fringe bitmaps to present the diff in the fringe as solid bars
  ;; (with no border) taking up less horizontal space in the fringe. However
  ;; this will look bad with themes that invert the foreground/background of
  ;; git-gutter-fr's faces (like `modus-themes' does.)
  (setq-default fringes-outside-margins t)
  (define-fringe-bitmap 'git-gutter-fr:added [#b11111000] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [#b11111000] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [#b11111000] nil nil '(center repeated)))

;;;;;;;;;;;;;
;;;; org ;;;;

;; NOTE document org
(use-package org
  :config
  (setopt org-return-follows-link t
          org-startup-folded 'content)
  ;; Open Org links in current window. Default is `'find-file-other-window'
  ;;
  ;; HACK: Can I replace this hack with some `display-buffer-alist'
  ;; configuration?
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  ;; Recenter and pulse the current line, and display the hidden contents of Org and Outline headings.
  (with-eval-after-load 'pulsar
    (dolist (hook '(org-follow-link-hook))
      (add-hook hook #'pulsar-recenter-middle)
      (add-hook hook #'pulsar-reveal-entry))))

;; (use-package org-gtd)

(use-package org-agenda
  :config
  (bind-keys
   :map ctl-x-map
   ;; NOTE replaced abbrev maps, find somewhere to relocate them later
   ("a" . org-agenda)))

;;;;;;;;;;;;;;;;;;;;;;
;;;; bibliography ;;;;

(use-package biblio
  :config
  ;; A bibliography provides pointers to stuff in the world outside of our
  ;; writings like books, articles, web pages, videos, etc. When we want to
  ;; reference these outside things, we attach an indicator which is used to look
  ;; up that reference in our bibliography. Think of the bibliography as a
  ;; database of links and information about those links. Now when we want to
  ;; associate a piece of writing to some external thing, we don't have to rely on
  ;; the physical form of that thing.
  ;;
  ;; Biblio provides facilities to browse and gather bibliographic references from
  ;; various well-curated sources, and formats them as BibTeX entries, the file
  ;; format of my bibliography. This is better than typing all entries manually
  ;; with the built-in BibTeX mode, which is inefficient and could easily lead to
  ;; errors. Simply run `biblio-lookup', select the source, and enter a search
  ;; query. Once the search has completed, a new buffer opens with the
  ;; results. You can then select your target entry and insert it into the buffer
  ;; you called biblio-lookup from, copy it and paste it later, or a perform a
  ;; number of other possible commands.
  (defvar +bibliography-files
    '("~/OneDrive/zettelkasten/reference/bibliography.bib")
    "List of bibliography files.")

  (defun +biblio--combined-lookup ()
    "Combines `biblio-lookup' and `biblio-doi-insert-bibtex'."
    (let* ((dbs (biblio--named-backends))
           (db-list (append dbs '(("DOI" . biblio-doi-backend))))
           (db-selected (biblio-completing-read-alist
                         "Backend: "
                         db-list)))
      (if (eq db-selected 'biblio-doi-backend)
          (let ((doi (read-string "DOI: ")))
            (biblio-doi-insert-bibtex doi))
        (biblio-lookup db-selected))))
  (defun +biblio-lookup ()
    "Insert Biblio search results into the current buffer or selected
BibTeX file."
    (interactive)
    (if-let ((current-mode major-mode)
             +bibliography-files
             (bibfiles (length +bibliography-files))
             (bibfile (cond ((eq bibfiles 1)
                             (car +bibliography-files))
                            ((equal major-mode 'bibtex-mode)
                             (buffer-file-name))
                            (t
                             (completing-read
                              "Select BibTeX file: " +bibliography-files)))))
        (progn
          (find-file bibfile)
          (goto-char (point-max))
          (+biblio--combined-lookup)
          (save-buffer))
      (message "No BibTeX file(s) defined.")))

  (bind-keys
   :map +bib-prefix-map
   ;; One minor inconvenience is that you must jump to the relevant bibliography
   ;; file before inserting a new entry, and it provides two seperate search
   ;; functions (`biblio-lookup' and `biblio-doi-insert-bibtex'). We write a
   ;; function that prompts for a BibTeX file to insert into, and combines the
   ;; two search functions.
   ("l" . +biblio-lookup)))

;; `biblio-openlibrary' provides a backend for `biblio' that supports queries
;; based on ISBN using OpenLibrary's Read API. The API does allow queries based
;; on a multitude of identifiers, but this package only allows and expects
;; queries based on an ISBN idetifier because biblio already provides
;; functionality for the other more common use cases.
(use-package biblio-openlibrary)

;; `biblio-gbooks' provides a backend for `biblio' that supports queries using
;; Google's Books API. While the existing biblio backends provide good coverage
;; of peer-reviewed scientific articles, they don't have good coverage of
;; fiction and non-fiction books.
(use-package biblio-gbooks)

(use-package citar
  :config
  ;; Citar provides a highly configurable `completing-read' front-end to browse
  ;; and act on bibliographic data. It is a reference manager of sorts because
  ;; it is the tool I use to access and manage my bibliography. It has support
  ;; for cross-referenced entries, completion-at-point, bibliographic notes,
  ;; attachments, navigating to the source bibliography file, and contextual
  ;; Embark actions. It also integrates with `org-cite', Org mode's citation
  ;; module.
  ;;
  ;; `citar-embark-mode' adds contextual Embark actions in the minibuffer and
  ;; with org-at-point. The actions are generic and work the same across Org,
  ;; Markdown, and LaTeX modes.
  (citar-embark-mode 1)
  ;; I prefer to have the Embark menu open with `org-open-at-point'.
  (setopt citar-at-point-function 'embark-act)
  ;; Configure the formatting for the sections in the completing-read UI.
  (setopt citar-templates '((main . "${author editor:30%sn}    ${date year issued:4}    ${title:80}")
                            (suffix . "${=key= id:15}    ${=type=:15}    ${tags keywords keywords:*}")
                            (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
                            (note . "#+title: Notes on ${author editor}, ${title}")))

  (setopt
   citar-select-multiple nil
   org-cite-global-bibliography +bibliography-files
   org-cite-insert-processor 'citar
   org-cite-follow-processor 'citar
   org-cite-activate-processor 'citar
   citar-bibliography org-cite-global-bibliography
   citar-library-paths '("~/OneDrive/zettelkasten/reference/")
   citar-notes-paths '("~/OneDrive/zettelkasten/"))
  (bind-keys
   :map +file-prefix-map
   ("l" . citar-open) ; "lib" mnemonic
   :map +bib-prefix-map
   ("f" . citar-open) ; "find" mnemonic
   :map org-mode-map
   ("C-c i" . org-cite-insert)))

;;;;;;;;;;;;;;;
;;;; notes ;;;;

(use-package denote
  :config
  ;; Denote is a simple note-taking tool for Emacs. It is based on the idea that
  ;; notes should follow a predictable and descriptive file-naming scheme. The
  ;; file name must offer a clear indication of what the note is about, without
  ;; reference to any other metadata. Denote basically streamlines the creation
  ;; of such files while providing facilities to link between them.
  ;;
  ;; Denote's file-naming scheme is not limited to "notes". It can be used for
  ;; all types of files, including those that are not editable in Emacs, such as
  ;; videos. Naming files in a consistent way makes their filtering and
  ;; retrieval easier. Denote provides facilities to rename files, regardless of
  ;; file type.
  (setopt denote-directory (expand-file-name "~/OneDrive/zettelkasten")
          ;; If you want to have a "controlled vocabulary" of keywords, meaning
          ;; that you only use a predefined set of them, then you want
          ;; `denote-infer-keywords' set to nil, and `denote-known-keywords' to
          ;; have the keywords you need.
          denote-infer-keywords t
          denote-sort-keywords t
          denote-known-keywords '("emacs")
          ;; Prompt for title, keywords, and signature in Denote commands that
          ;; prompt for user input to construct a Denote file name.
          denote-prompts '(title keywords signature))
  ;; Highlight Denote file names in Dired buffers.
  ;;
  ;; If you only want the `denote-dired-mode' in select directories, then modify
  ;; the variable `denote-dired-directories' and use
  ;; `denote-dired-mode-in-directories'.
  ;;
  ;; If you want the generic approach, which is great if you rename files
  ;; Denote-style in lots of different places, use `denote-dired-mode'.
  (setopt denote-dired-directories `(,(expand-file-name "~/OneDrive/zettelkasten"))
          denote-dired-directories-include-subdirectories t)
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

  ;; Automatically rename Denote buffers when opening them so that instead of
  ;; their long file name they have a literal "[D]" followed by the file's title
  ;; and then the backlinks indicator. Read the doc string of
  ;; `denote-rename-buffer-format' for how to modify this.
  (setopt denote-rename-buffer-format "[D] %t%b")
  ;; Customize what the backlink indicator looks like.
  (setopt denote-buffer-has-backlinks-string " (<-->)")
  ;; `denote-rename-buffer-mode' provides the means to automatically rename the
  ;; buffer of a Denote file upon visiting the file.
  (denote-rename-buffer-mode 1)

  (bind-keys
   :map +notes-prefix-map
   ("n" . denote)
   ("N" . denote-type)
   ("o" . denote-sort-dired) ; "order" mnemonic
   ;; Note that `denote-rename-file' can work from any context, not just Dired
   ;; buffers. That is why we bind it globally.
   ("r" . denote-rename-file)
   :map text-mode-map
   ("C-c n b" . denote-backlinks)
   ("C-c n i" . denote-link) ; "insert" mnemonic
   ("C-c n I" . denote-add-links)
   ("C-c n r" . denote-rename-file)
   ("C-c n R" . denote-rename-file-using-front-matter)
   :map org-mode-map
   ("C-c n d b" . denote-org-extras-dblock-insert-backlinks)
   ("C-c n d l" . denote-org-extras-dblock-insert-links)
   :map dired-mode-map
   ("C-c n i" . denote-dired-link-marked-notes)
   ("C-c n r" . denote-dired-rename-marked-files)
   ("C-c n R" . denote-dired-rename-marked-files-using-front-matter)
   ("C-c n t" . denote-dired-rename-marked-files-with-keywords)))

(use-package consult-denote
  :config
  ;; This package is glue code to integrate `denote' with Daniel Mendler's
  ;; `consult' package. The idea is to enhance minibuffer interactions, such as
  ;; by providing a preview of the file-to-be-linked/opened and by adding more
  ;; sources to the `consult-buffer' command.
  (consult-denote-mode 1)
  (bind-keys
   :map +notes-prefix-map
   ("f" . consult-denote-find)
   ("g" . consult-denote-grep)
   :map +file-prefix-map
   ("n" . consult-denote-find)
   :map search-map
   ("n" . consult-denote-grep)))

(use-package citar-denote
  :config
  ;; `citar-denote' makes it possible to write notes on BibTeX entries with the
  ;; help of the `citar' package. These notes have the citation's unique key
  ;; associated with them in the file's front matter. They also get a
  ;; configurable keyword in their file name (`citar-denote-keyword'), making it
  ;; easy to find them in Dired and/or retrieve them with the various Denote
  ;; methods.
  (citar-denote-mode)

  (setopt
   ;; Allow multiple notes per bibliographic entry.
   citar-open-always-create-notes nil
   ;; Change the default keyword for bibliographic notes. I'm using these like
   ;; the literature notes in my zettelkasten.
   citar-denote-keyword "literature")

  (bind-keys
   :map +bib-prefix-map
   ;; Adds citation keys or converts existing Denote file to a bibliographic
   ;; note. When converting a regular Denote file, adds the
   ;; `citar-denote-keyword' to the front matter and renames the file
   ;; accordingly.
   ("t" . citar-denote-add-citekey)
   ;; Remove citation keys. When no more reference items are left, the
   ;; `citar-denote-keyword' is removed and the file is renamed.
   ("T" . citar-denote-remove-citekey)))

;; Simply saving, excerpting, or copying materials is not enough; information
;; needs to be processed to be transformed into useful knowledge. The reason is
;; that merely transporting material only increases the amount of information
;; without reprocessing it.

;; The Zettelkasten method emphasizes summarizing/reviewing in your own words
;; and establishing connections, providing multiple opportunities for
;; information processing. However, many introductions to the Zettelkasten
;; method often get caught up in the craze of double-linking, falling into the
;; trap of merely saving data-- essentially ignoring the method Niklas Luhmann
;; used to handle a large amount of literature notes.
;;
;; Luhmann had a habit of taking literature notes while reading. Each literature
;; was essentially an index of the material. He only excerpted the original text
;; from the book when absolutely necessary. Literature notes are an efficient
;; and in-depth method that records key points and inspirations, faciliting
;; quick review and deep reading, while also helping distinguish between
;; existing and new information.
(use-package org-remark
  :config
  ;; `org-remark' allows us to highlight and annotate any text file. It can
  ;; automatically create a literature note for a given text file. We can select
  ;; any text and highlight it, which applies an overlay with a a user-defined
  ;; text face through its custom highlighter pens facility. The highlight and
  ;; any associated notes are kept in an Org file functioning as a plain text
  ;; database. This lets us easily manage our marginal notes and use the
  ;; built-in Org facilities on them. The entries in this file simply save the
  ;; locations of our highlighted text. We can automatically load the highlights
  ;; from previous sessions, and we can display the marginal notes for the
  ;; highlight at point.
  ;;
  ;; These marginal notes are external to the source document, leveraging all
  ;; the power of Org while acting like notes that are made inside of the
  ;; document. They are an incredibly efficient way of taking literature notes
  ;; while reading any text document.
  ;;
  ;; These minor modes lets us highlight and annotate Info documentation,
  ;; websites, and EPUB books just like text files.
  (use-package org-remark-info :after info :config (org-remark-info-mode +1))
  (use-package org-remark-eww :after eww :config (org-remark-eww-mode +1))
  (use-package org-remark-nov :after nov :config (org-remark-nov-mode +1))
  ;; Automatically turn on highlights after re-starting Emacs. Without this
  ;; global minor mode we would need to remember to activate `org-remark-mode'
  ;; for each file where we add highlights and annotations, which is often
  ;; impractical.
  (org-remark-global-tracking-mode)

  ;; Don't tell me that a buffer has no highlights. Too noisy.
  (setopt org-remark-report-no-highlights nil)

  ;; Create a Denote-compatible marginal note
  (defun +org-remark-denote-filename-has-note-p (filename)
    "Find the Denote filename similar to FILENAME but with the 'literature' keyword."
    (let* ((files (denote-directory-files))
           (source-title (denote-retrieve-filename-title filename))
           (source-signature (denote-retrieve-filename-signature filename))
           (source-keywords (denote-retrieve-filename-keywords filename))
           (source-keywords (if source-keywords
                                (split-string source-keywords "_")
                              nil)))
      (cl-find-if (lambda (file)
                    (let* ((file-title (denote-retrieve-filename-title file))
                           (file-signature (denote-retrieve-filename-signature file))
                           (file-keywords (denote-retrieve-filename-keywords file))
                           (file-keywords
                            (if (and source-keywords file-keywords)
                                (split-string file-keywords "_")
                              nil)))
                      (and (string= file-title source-title)
                           (string= file-signature source-signature)
                           (member "literature" file-keywords)
                           (seq-set-equal-p
                            (seq-remove (lambda (elt) (member elt '("literature" "reference")))
                                        source-keywords)
                            (seq-remove (lambda (elt) (member elt '("literature" "reference")))
                                        file-keywords)))))
                  files)))
  (defun +org-remark-denote-file-name-function ()
    "Return a Denote-compatible file name for the current buffer.

When the current buffer is visiting a file, the name of the
marginal notes file will be \"DATE==SIGNATURE--TITLE__literature.org\"
in your `denote-directory'."
    (let* ((source-filename (cond ((eq major-mode 'nov-mode)
                                   (file-name-nondirectory nov-file-name))
                                  (t
                                   (org-remark-source-find-file-name))))
           ;; (source-filename (file-name-sans-extension (file-name-nondirectory (org-remark-source-find-file-name))))
           (denote-id (denote-retrieve-filename-identifier source-filename))
           (denote-signature (denote-retrieve-filename-signature source-filename))
           (denote-title (denote-retrieve-filename-title source-filename))
           (denote-keywords (denote-retrieve-filename-keywords source-filename)))
      (if-let ((literature-note (+org-remark-denote-filename-has-note-p source-filename)))
          literature-note
        (denote-format-file-name
         (denote-directory)
         (denote--find-first-unused-id (denote-get-identifier nil))
         (if denote-keywords
             (remove
              "reference"
              (append (split-string denote-keywords "_") '("literature")))
           nil)
         (or denote-title "")
         (or denote-file-type ".org")
         (or denote-signature "")))))
  (setopt org-remark-notes-file-name #'+org-remark-denote-file-name-function)

  (bind-keys
   :map +notes-prefix-map
   ("m" . org-remark-mark)
   ("M" . org-remark-mark-line)
   ("d" . org-remark-remove)
   ("D" . org-remark-delete)
   ("v" . org-remark-view)
   ("V" . org-remark-open)))

;;;;;;;;;;;;;
;;;; pdf ;;;;

;; (use-package pdf-tools)
;; (use-package saveplace-pdf-view)

;;;;;;;;;;;;;;
;;;; epub ;;;;

;; NOTE document nov
(use-package nov
  :init
  ;; Activate nov-mode for epub files
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :config
  (setq nov-text-width 80))
