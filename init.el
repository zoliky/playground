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

;; File used for storing customization information
(setq custom-file (locate-user-emacs-file "custom.el"))

;; Default
(set-face-attribute 'default nil :family "Hack" :height 180)

(setq tab-width 2)
(prefer-coding-system 'utf-8)         ; Set default encoding to UTF-8
(set-language-environment 'utf-8)     ; Set default language environment to UTF-8

;;;; Installing Packages

;; Configure package sources
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
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;;;; Spell checking

(use-package ispell
  :ensure nil
  :defer 0.2
  :custom
  (ispell-program-name "hunspell")
  ;; English (US), Hungarian, and Romanian
  (ispell-dictionary "en_US,hu_HU,ro_RO")
  :config
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,hu_HU,ro_RO"))

(use-package flyspell
  :ensure nil
  :after ispell
  :bind ("C-c s" . flyspell-mode))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-wrapper)))

;;;; File

(use-package files
  :ensure nil
  :custom
  (backup-directory-alist '(("." . "~/.emacs.d/backups")))
  (backup-by-copying t)               ; Always use copying to create backup files
  (delete-old-versions t)             ; Delete excess backup versions
  (kept-new-versions 6)               ; Number of newest versions to keep when a new backup is made
  (kept-old-versions 2)               ; Number of oldest versions to keep when a new backup is made
  (version-control t)                 ; Make numeric backup versions unconditionally
  (auto-save-default nil)             ; Stop creating #autosave# files
  (mode-require-final-newline nil)    ; Don't add newlines at the end of files
  (large-file-warning-threshold nil)) ; Open large files without requesting confirmation

;;;; Other Defaults

(use-package display-line-numbers
  :ensure nil
  :hook ((text-mode prog-mode conf-mode) . display-line-numbers-mode))

;;;; Packages
;;;;; Avy

;; Avy allows to quickly jump to any position in a buffer
(use-package avy
  :bind ("M-s" . avy-goto-char))

;;;;; Corfu

(use-package corfu
  :hook (prog-mode . corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.1)
  (corfu-quit-no-match 'separator))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;;;;; Consult

(use-package consult
  :bind (("C-s"   . consult-line)
	 ("C-x b" . consult-buffer)))

;;;;; Dashboard

(use-package dashboard
  :custom
  (dashboard-items '((recents  . 5)))
  (dashboard-set-footer nil)
  (dashboard-set-init-info t)
  (dashboard-center-content t)
  (dashboard-startup-banner 'logo)
  :config
  (dashboard-setup-startup-hook))

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

;; Hide dotfiles
(use-package dired-hide-dotfiles
  :after dired
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (:map dired-mode-map
	      ("." . dired-hide-dotfiles-mode))
  :custom
  (dired-hide-dotfiles-verbose nil))

;;;;; Ef themes

(use-package ef-themes
  :config
  (load-theme 'ef-summer t))

;;;;; Exec path

;; (use-package exec-path-from-shell
;;   :init
;;   (setq exec-path-from-shell-arguments nil)
;;   :config
;;   (setq exec-path-from-shell-debug t)
;;   (exec-path-from-shell-initialize))

;;;;; Gruvbox

(use-package gruvbox-theme
  :defer t)

;;;;; JavaScript

(use-package js2-mode
  :mode "\\.jsx?\\'")

;;;;; Helpful

(use-package helpful
  :bind
  ([remap describe-key]      . helpful-key)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-function] . helpful-callable))

;;;;; Ibuffer

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer))

;;;;; Indent guides

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-method 'character))

;;;;; Magit

(use-package magit
  :bind ("C-c g" . magit-status))

;;;;; Markdown

(use-package markdown-mode
  :init
  (setq markdown-command "multimarkdown")
  :hook (markdown-mode . (lambda () (display-line-numbers-mode -1)))
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;;;;; Modus themes

(use-package modus-themes
  :defer t)

;;;;; Move text

(use-package move-text
  :bind (("M-p" . move-text-up)
	 ("M-n" . move-text-down))
  :config
  (move-text-default-bindings))

;;;;; Olivetti

(use-package olivetti
  :hook (org-mode . olivetti-mode)
  :custom
  (olivetti-body-width 80))

;;;;; Outshine

(use-package outshine
  :defer t)

;;;;; Projectile

;; (use-package projectile
;;   :init
;;   (projectile-mode)
;;   :bind ("C-c p" . projectile-command-map))

;;;;; Rainbow delimiters

;; Rainbow delimiters highlights delimiters such as parentheses,
;; brackets or braces according to their depth.
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;;;; Try

(use-package try
  :defer t)

;;;;; TOML

(use-package toml-mode
  :mode "\\.toml\\'")

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

;;;;; Web mode

(use-package web-mode
  :mode "\\.html\\'"
  :custom
  (web-mode-attr-indent-offset 2)
  (web-mode-enable-css-colorization t)
  (web-mode-enable-auto-closing t)
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-current-element-highlight t))

(use-package auto-rename-tag
  :hook (web-mode . auto-rename-tag-mode))

;;;;; Which key

(use-package which-key
  :custom
  (which-key-idle-delay 1)
  :config
  (which-key-mode))

;;;;; Lua

;; Major mode for editing Lua files
(use-package lua-mode
  :mode "\\.lua\\'")

;;;;; YAML

(use-package yaml-mode
  :mode "\\.yml\\'")

;;;;; Doom modeline

(use-package doom-modeline
  :init
  (doom-modeline-mode)
  :custom
  (doom-modeline-mu4e t)
  (doom-modeline-height 38))

;;;; Email

(use-package mu4e
  :ensure nil
  ;:ensure-system-package mu
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :bind (("C-c m" . mu4e)
         :map mu4e-view-mode-map
         ("n"         . next-line)
         ("p"         . previous-line)
         ("<tab>"     . org-next-link)
         ("<backtab>" . org-previous-link)
         ("<RET>"     . mu4e~view-browse-url-from-binding))
  :hook (mu4e-compose-mode
         . (lambda ()
             (flyspell-mode)
             (auto-fill-mode -1)
             (display-line-numbers-mode -1)))
  :custom
  (mail-user-agent 'mu4e-user-agent)
  (mu4e-get-mail-command "mbsync -c ~/.mbsyncrc -a")
  (mu4e-update-interval 600)
  (mu4e-split-view nil)
  (mu4e-confirm-quit nil)
  (mu4e-use-fancy-chars t)
  (mu4e-view-show-images t)
  (mu4e-view-prefer-html t)
  (mu4e-view-show-addresses t)
  (mu4e-hide-index-messages t)
  (mu4e-attachment-dir "~/Downloads")
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-change-filenames-when-moving t)
  (mu4e-sent-messages-behavior 'delete)
  (mu4e-index-update-error-warning nil)
  (mu4e-html2text-command "w3m -dump -I utf-8 -O utf-8 -T text/html"))

(use-package mu4e-headers
  :ensure nil
  :after mu4e
  :hook (mu4e-headers-mode . (lambda () (eldoc-mode -1)))
  :custom
  (mu4e-headers-auto-update t)
  (mu4e-headers-fields `((:human-date . 12)
                         (:flags      .  6)
                         (:from       . 22)
                         (:subject)))
;  (mu4e-headers-fields `((:human-date . 12)
;                         (:flags      .  6)
;                         (:from       . 22)
;                         (:subject    . ,(- (window-body-width) 50))))
  :config
  (setq mu4e-headers-attach-mark '("a" . "📎")))

(use-package message
  :ensure nil
  :after mu4e
  :custom
  (message-kill-buffer-on-exit t)
  (message-send-mail-function 'smtpmail-send-it))

(use-package smtpmail
  :ensure nil
  :after mu4e
  :custom
  (smtpmail-smtp-service 587)
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-auth-credentials "~/.authinfo.gpg")
  (smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))))

(use-package org-mime
  :defer t
  :config
  (setq org-mime-export-options '(:section-numbers nil
                                  :with-author nil
                                  :with-toc nil)))

(use-package mu4e-context
  :ensure nil
  :after mu4e
  :custom
  (mu4e-context-policy 'pick-first)
  (mu4e-compose-context-policy 'always-ask)
  :config
  (setq mu4e-contexts
        (list
         (make-mu4e-context
          ;; Personal context
          :name "personal"
          :enter-func (lambda () (mu4e-message "Entering personal context"))
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches
                           msg '(:from :to :cc :bcc) "zoliky@gmail.com")))
          :vars '((user-mail-address  . "zoliky@gmail.com")
                  (user-full-name     . "Zoltan Kiraly")
                  (mu4e-sent-folder   . "/gmail-zoliky/[Gmail].Sent Mail")
                  (mu4e-drafts-folder . "/gmail-zoliky/[Gmail].Drafts")
                  (mu4e-trash-folder  . "/gmail-zoliky/[Gmail].Trash")
                  (smtpmail-queue-dir . "~/Maildir/gmail-zoliky/queue/cur")
                  (smtpmail-smtp-user . "zoliky")
                  (mu4e-maildir-shortcuts
                   . ((:maildir "/gmail-zoliky/INBOX"             :key ?i)
                      (:maildir "/gmail-zoliky/[Gmail].Starred"   :key ?r)
                      (:maildir "/gmail-zoliky/[Gmail].Sent Mail" :key ?s)
                      (:maildir "/gmail-zoliky/[Gmail].Drafts"    :key ?d)
                      (:maildir "/gmail-zoliky/[Gmail].Trash"     :key ?t)))))
         (make-mu4e-context
          ;; Work context
          :name "work"
          :enter-func (lambda () (mu4e-message "Entering work context"))
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches
                           msg '(:from :to :cc :bcc) "zolikydev@gmail.com")))
          :vars '((user-mail-address  . "zolikydev@gmail.com")
                  (user-full-name     . "Zoltan Kiraly")
                  (mu4e-sent-folder   . "/gmail-zolikydev/[Gmail].Sent Mail")
                  (mu4e-drafts-folder . "/gmail-zolikydev/[Gmail].Drafts")
                  (mu4e-trash-folder  . "/gmail-zolikydev/[Gmail].Trash")
                  (smtpmail-queue-dir . "~/Maildir/gmail-zolikydev/queue/cur")
                  (smtpmail-smtp-user . "zolikydev")
                  (mu4e-maildir-shortcuts
                   . ((:maildir "/gmail-zolikydev/INBOX"             :key ?i)
                      (:maildir "/gmail-zolikydev/[Gmail].Starred"   :key ?r)
                      (:maildir "/gmail-zolikydev/[Gmail].Sent Mail" :key ?s)
                      (:maildir "/gmail-zolikydev/[Gmail].Drafts"    :key ?d)
                      (:maildir "/gmail-zolikydev/[Gmail].Trash"     :key ?t))))))))

;; Load mu4e in the background when Emacs starts
;(run-at-time
; "0.2 sec" nil (lambda ()
;                (let ((current-prefix-arg '(4)))
;                  (call-interactively 'mu4e)
;                  (message nil))))

(use-package mu4e-alert
  :after mu4e
;  :hook ((after-init . mu4e-alert-enable-mode-line-display))
  :custom
  ;; Notify only of unread emails in the inbox
  (mu4e-alert-interesting-mail-query "flag:unread maildir:/INBOX/")
  :config
  (mu4e-alert-enable-mode-line-display)
  (mu4e-alert-set-default-style 'libnotify))

;;;; Org mode

(setq org-modules '())

;;;;; Org

(use-package org
  :ensure nil
  :hook (org-mode . (lambda ()
                      (org-indent-mode)))
  :bind ("C-c l" . org-store-link)
  :custom
  (org-ellipsis " ▾")
  (org-tags-column 0)
  (org-log-done 'time)
  (org-startup-folded t)
  (org-log-into-drawer t)
  (org-clock-into-drawer t)
  (org-log-reschedule 'time)
  (org-image-actual-width nil)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-hide-emphasis-markers t)
  (org-directory "~/orgfiles")
  (org-export-with-tags nil)
  (org-export-headline-levels 5)
  (org-export-backends '(html latex))
  (org-startup-with-inline-images t)
  (org-modules '(org-crypt org-habit))
  (org-tag-alist '(("crypt"    . ?c)
                   ("temp"     . ?t)
                   ("home"     . ?h)
                   ("work"     . ?w)
                   ("urgent"   . ?u)
                   ("export"   . ?e)
                   ("noexport" . ?n)
                   ("expired"  . ?x)
                   ("TOC"      . ?T)))
  (org-tags-sort-function 'org-string-collate-lessp)
  (org-tags-exclude-from-inheritance '("crypt"))
  (org-todo-keywords '((sequence "TODO(t)"
                                 "NEXT(n)"
                                 "REPEAT(r)"
                                 "WAITING(w)"
                                 "POSTPONED(e)"
                                 "SOMEDAY(s)"
                                 "DELEGATED(o)"
                                 "PROJECT(p)" "|"
                                 "DONE(d)"
                                 "FORWARDED(f)"
                                 "CANCELLED(c)")
                       (sequence "GOAL(g)" "|"
                                 "ACHIEVED(a)"
                                 "FAILED(x)")))
  (org-todo-repeat-to-state "REPEAT")
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-targets '((org-agenda-files . (:maxlevel . 4)))))

;;;;; Agenda

(use-package org-agenda
  :ensure nil
  :bind ("C-c a" . org-agenda)
  :custom
  (org-agenda-files
   (seq-filter #'file-exists-p
               (mapcar #'(lambda (file) (file-name-concat org-directory file))
                       '("bookmarks.org"
                         "calendar.org"
                         "contacts.org"
                         "personal.org"
                         "work.org"
                         "misc.org"
                         "notes.org"
                         "people.org"
                         "refile.org"
                         "elfeed.org"
                         "english.org"
                         "spanish.org"
                         "private.org"))))
  (org-agenda-include-diary t)
  (org-habit-graph-column 80)
  (org-habit-today-glyph ?⧖)
  (org-habit-completed-glyph ?✓))

;;;;; Appear

(setq org-hide-emphasis-markers t)

(use-package org-appear
  :hook (org-mode . org-appear-mode))

;;;;; Bullets

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (org-superstar-configure-like-org-bullets))

;;;;; Denote

(use-package denote
  :bind ("C-c d" . denote)
  :hook (dired-mode . denote-dired-mode)
  :custom
  (denote-sort-keywords t)
  (denote-allow-multi-word-keywords nil)
  (denote-directory "~/tmp"))

;;;; Emms

(use-package emms
  :bind (("C-c u"  . emms)
         ("C-c U"  . emms-browser)
         ("<C-f1>" . emms-show)
         ("<C-f2>" . emms-volume-lower)
         ("<C-f3>" . emms-volume-raise)
         ("<C-f5>" . emms-previous)
         ("<C-f6>" . emms-next)
         ("<C-f7>" . emms-pause)
         ("<C-f8>" . emms-stop)
         :map emms-playlist-mode-map
         ("p" . previous-line)
         ("n" . next-line))
  :custom
  (emms-info-asynchronously t)
  (emms-volume-amixer-card 1)
  (emms-volume-amixer-control "PCM")
  (emms-playlist-buffer-name "*Music*")
  (emms-player-list '(emms-player-mpv))
  (emms-source-file-default-directory "/run/media/zoliky/Lara/Music")
  (emms-source-file-directory-tree-function
   'emms-source-file-directory-tree-find)
  :config
  (require 'emms-setup)
  (require 'emms-history)
  (require 'emms-volume)
  (require 'emms-volume-amixer)
  (require 'emms-mode-line)
  (emms-all)
  (emms-history-load)
  (emms-mode-line nil))

;;;; Other

;; Variables configured via the interactive customize interface
;(when (file-exists-p custom-file)
;  (load custom-file))

(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold 800000)
             (garbage-collect)) t)

;;; init.el ends here
