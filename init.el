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
;;;; Flags

(defcustom enable-vertico t  "Set 'nil' to 't' to enable vertical completion")

;;;; Startup

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs loaded in %s."
		     (emacs-init-time))))

(setq custom-file (locate-user-emacs-file "custom.el"))

;; Default
(set-face-attribute 'default nil :family "Hack" :height 180)

;; Fixed-pitch
(set-face-attribute 'fixed-pitch nil :family "Hack")

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

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;;;; Defaults


(prefer-coding-system 'utf-8)         ; Set default encoding to UTF-8
(set-language-environment 'utf-8)     ; Set default language environment to UTF-8


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

(when enable-vertico
  (use-package vertico
    :custom
    (vertico-cycle t)
    :config
    (vertico-mode))

  (use-package marginalia
    :after vertico
    :custom
    (marginalia-align 'right)
    :config
    (marginalia-mode)))

;;;;; Rainbow delimiters

;; Rainbow delimiters highlights delimiters such as parentheses,
;; brackets or braces according to their depth.
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;;;; Outshine

(use-package outshine)

(use-package outline-minor-faces
  :after outline
  :config (add-hook 'outline-minor-mode-hook
		    #'outline-minor-faces-mode))

;;;; Org mode
;;;;; Superstar

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  (org-superstar-configure-like-org-bullets))

;(use-package org-bullets
;  :after org
;  :hook (org-mode . org-bullets-mode))

;;;; Other

;; Variables configured via the interactive customize interface
;(when (file-exists-p custom-file)
;  (load custom-file))

;;; init.el ends here
