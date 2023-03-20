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

;;;; Other

(use-package try)

;; Variables configured via the interactive customize interface
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
