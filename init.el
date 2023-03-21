;;; init.el --- Initialization file -*- lexical-binding: t; -*-

;; Copyright (c) 2023 Zoltán Király
;; Created: March 19, 2023

;; Author: Zoltán Király <zoliky@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file specifies how to initialize Emacs
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html

;;; Code:
;;;; Startup

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs loaded in %s."
		     (emacs-init-time))))

(setq custom-file (locate-user-emacs-file "custom.el"))

;;;; Package system

;; Initialize the package system
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setq package-archive-priorities '(("melpa"  . 100)
				   ("gnu"    .  50)
				   ("nongnu" .  25)))

;; Ensure that the use-package macro is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package use-package
  :custom
  (use-package-always-ensure t))

;;;; Defaults

(setq tab-width 2)

;;;; Packages
;;;;; Avy

(use-package avy
  :bind ("M-s" . avy-goto-char))

;;;;; Ef themes

(use-package ef-themes
  :config
  (load-theme 'ef-summer t))

;;;;; Modus themes

(use-package modus-themes
  :defer t)

;;;;; Vertico

(use-package vertico
  :custom
  (vertico-cycle t)
  :config
  (vertico-mode))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
	      ("RET" . vertico-directory-enter)
	      ("DEL" . vertico-directory-delete-char)
	      ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-align 'right)
  :config
  (marginalia-mode))

;;;;; Rainbow delimiters

;; Rainbow delimiters highlights delimiters such as parentheses,
;; brackets or braces according to their depth.
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;;;; Outshine

(use-package outshine
  :defer t)

;;;;; Olivetti

(use-package olivetti
  :hook (org-mode . olivetti-mode)
  :custom
  (olivetti-body-width 80))

;;;;; Try

(use-package try
  :defer t)

;;;;; Dired

(use-package dired
  :ensure nil
  :bind ("C-x C-j" . dired-jump)
  :custom
  (dired-auto-revert-buffer t)
  (delete-by-moving-to-trash t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-listing-switches "-agho --group-directories-first"))

(use-package dired-narrow
  :after dired
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("<backtab>" . dired-subtree-cycle)
              ("<tab>"     . dired-subtree-toggle)))

;; Hide hidden files
(use-package dired-hide-dotfiles
  :after dired
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (:map dired-mode-map
	      ("." . dired-hide-dotfiles-mode))
  :custom
  (dired-hide-dotfiles-verbose nil))

;;;;; Ibuffer

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer))

;;;;; Lua

;; Major mode for editing Lua files
(use-package lua-mode
  :mode "\\.lua\\'")

;;;;; Helpful

(use-package helpful
  :bind
  ([remap describe-key]      . helpful-key)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-function] . helpful-callable))

;;;;; Magit

(use-package magit
  :bind ("C-c g" . magit-status))

;;;;; Which key

(use-package which-key
  :custom
  (which-key-idle-delay 1)
  :config
  (which-key-mode))

;;;;; Gruvbox

(use-package gruvbox-theme
  :defer t)

;;;;; TOML

(use-package toml-mode
  :mode "\\.toml\\'")

;;;;; YAML

(use-package yaml-mode
  :mode "\\.yml\\'")

;;;; Other

;; Variables configured via the interactive customize interface
;(when (file-exists-p custom-file)
;  (load custom-file))

;;;; Org mode

(setq org-modules '())

;;;;; Appear

(setq org-hide-emphasis-markers t)

(use-package org-appear
  :hook (org-mode . org-appear-mode))

;;; init.el ends here
