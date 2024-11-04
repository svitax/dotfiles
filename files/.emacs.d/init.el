;;; init.el --- This is my init. -*- lexical-binding: t; -*-

;;; Commentary:

;; This is where my Emacs config starts.

;;; Code:

;;;;;;;;;;;;;;;;;
;;;; startup ;;;;

(use-package startup
  :no-require
  :init
  (setopt inhibit-startup-screen t
	  initial-major-mode 'fundamental-mode
	  initial-scratch-message nil))

;;;;;;;;;;;;;;;
;;;; faces ;;;;

(use-package modus-themes
  :config
  (setopt modus-themes-common-palette-overrides
	  `((bg-region bg-sage)
	    ,@modus-themes-preset-overrides-faint))
  (modus-themes-select 'modus-vivendi))
