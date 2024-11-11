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

;;;;;;;;;;;;;;
;;;; guix ;;;;

(use-package guix)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; :xdg-cache and :xdg-state ;;;;

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
  ;; mode-specific-map (`C-c'). The idea is to hit a series of keys to get the
  ;; desired command. Keymaps are organised thematically and rely on strong
  ;; mnemonics, such as `b' for buffers, `w' for windows, and so on.
  (defvar-keymap +buffer-prefix-map
    :doc "Prefix keymap for buffers."
    :prefix '+buffer-prefix)
  (defvar-keymap +file-prefix-map
    :doc "Prefix keymap for files."
    :prefix '+file-prefix
    "f" #'find-file
    "l" #'find-library
    "m" #'man)
  (defvar-keymap +window-prefix-map
    :doc "Prefix map for windows."
    :prefix '+window-prefix)
  (defvar-keymap +toggle-prefix-map
    :doc "Prefix map for minor mode toggles."
    :prefix '+toggle-prefix
    "f" #'flymake-mode
    "h" #'hl-line-mode
    ;; "k" #'keycast-mode-line-mode
    ;; "l" #'logos-focus-map
    "n" #'display-line-numbers-mode
    ;; "s" #'spacious-padding-mode
    ;; "r" #'rainbow-mode
    "v" #'variable-pitch-mode)
  (bind-keys
   :map mode-specific-map
   ;; "a" org-agenda ; replace abbrev stuff
   ("b" . +buffer-prefix)
   ;; "c" org-capture
   ;; "d" +dap-prefix
   ;; "e" +eval-prefix
   ("f" . +file-prefix)
   ;; "g"
   ;; "h"
   ;; "i"
   ("j" . dired-jump)
   ;; "k"
   ;; "l"
   ;; ("m" . +mail-prefix) ; C-x m
   ;; ("n" . +narrow-prefix)
   ;; "o" org
   ;; ("p" . +project-prefix)
   ;; "q"
   ;; "r" registers
   ;; "s" +search-prefix
   ;; "t" tab
   ;; "u" undo?
   ;; ("v" . +vc-prefix)
   ("w" . +window-prefix)
   ("x" . +toggle-prefix)
   ;; "y"
   ;; "z"
   ))

;; TODO replace with embark
(use-package which-key)

;;;;;;;;;;;;;;;
;;;; faces ;;;;

(use-package fontaine
  :if (display-graphic-p)
  :config
  ;; Define detailed font configurations and set them on command.
  (bind-keys
   :map +toggle-prefix
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
  (add-hook 'elfeed-show-mode-hook #'+enable-variable-pitch))

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

(use-package files
  :config
  (setopt y-or-n-p-use-read-key t
	  use-short-answers t
	  confirm-kill-processes nil
	  confirm-kill-emacs 'yes-or-no-p))

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
  (setopt completion-styles '(basic partial-completion orderless)))

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
   ("C-x b" . consult-buffer)
   :map goto-map
   ("g" . consult-goto-line)
   :map search-map
   ("b" . consult-buffer)
   ("f" . consult-find) ; fd
   ("g" . consult-grep) ; rg
   ("h" . consult-history)
   ("i" . consult-imenu)
   ("l" . consult-line)
   ("m" . consult-mark)
   ("s" . consult-outline)
   :map +buffer-prefix
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
  ;; producing a popup where the cursor is.
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
	  corfu-popupinfo-delay '(0.25 . 0.25)
	  tab-always-indent 'complete)

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
(use-package buffer
  :no-require
  :config
  (bind-keys
   :map +buffer-prefix
   ("c" . clone-indirect-buffer-other-window)
   ("g" . revert-buffer-quick)
   ("k" . +kill-current-buffer)
   ;; "m" #'+buffers-major-mode (prot) ; if i can filter in consult-buffer by major mode i don't need this
   ("r" . +rename-file-and-buffer)
   ;; "v" #'+buffers-vc-root (prot) ; if i can filter in consult-buffer by vc root i don't need this
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
   :map +window-prefix
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

;;;;;;;;;;;;;;;;;;;;
;;;; navigation ;;;;

(use-package avy
  :config
  (setopt avy-keys '(?n ?r ?t ?s ?h ?a ?e ?i) ; Graphite keyboard layout
	  avy-timeout-seconds 0.27
	  avy-single-candidate-jump nil)
  (bind-keys
   :map global-map
   ;; NOTE i dont know which key i prefer yet
   ("C-'" . avy-goto-char-timer)
   ("M-j" . avy-goto-char-timer)))

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
   ("C-j" . +join-line-below)

   ;; The `+comment-dwim' command is like the built-in `comment-dwim', but
   ;; toggles linewise commenting instead of appending them by default.
   ("M-;" . +comment-dwim)))

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

;;;;;;;;;;;;
;;;; vc ;;;;

;; NOTE document magit
(use-package magit
  :config
  (setopt magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  
  (bind-keys
   :map mode-specific-map
   ;; NOTE potentially add to vc-prefix-map instead
   ("v" . magit-status)))

;;;;;;;;;;;;;;
;;;; diff ;;;;

(use-package git-gutter
  :config
  (add-hook 'prog-mode-hook #'git-gutter-mode)
  (setopt fringes-outside-margins t
	  git-gutter:update-interval 0))

(use-package git-gutter-fringe
  :config
  (setopt git-gutter-fr:side 'left-fringe)
  (define-fringe-bitmap 'git-gutter-fr:added [#b11111000] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [#b11111000] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [#b11111000] nil nil '(center repeated)))

;;;;;;;;;;;;;
;;;; org ;;;;

;; (use-package org)
;; (use-package org-gtd)

(use-package org-agenda
  :config
  (bind-keys
   :map ctl-x-map
   ("a" . org-agenda)))

;;;;;;;;;;;;;;;;;;;;;;
;;;; bibliography ;;;;

;; (use-package citar)
;; (use-package biblio)
;; (use-package biblio-openlibrary)
;; (use-package biblio-gbooks)

;;;;;;;;;;;;;;;
;;;; notes ;;;;

;; (use-package denote)
;; (use-package consult-denote)
;; (use-package org-remark)

;;;;;;;;;;;;;
;;;; pdf ;;;;

;; (use-package pdf-tools)
;; (use-package saveplace-pdf-view)

;;;;;;;;;;;;;;
;;;; epub ;;;;

;; (use-package nov)
