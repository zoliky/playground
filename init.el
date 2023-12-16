;;; init.el --- Initialization file -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023 Zoltán Király

;; Author: Zoltán Király <zoltan.public@fastmail.com>

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file specifies how to initialize Emacs
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File

;;; Code:

;; Check to see if the minimum version requirement of Emacs is met
;(let ((minver "29.1"))
;  (when (version< emacs-version minver)
;    (error "This configuration requires Emacs %s or higher." minver)))

;;;; Package management

;; Configure package sources and priorities
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setq package-archive-priorities '(("melpa"  . 100)
                                   ("gnu"    .  50)
                                   ("nongnu" .  25)))

;; Treat every package as though it had specified using :ensure t
(use-package use-package-ensure
  :custom
  (use-package-always-ensure t))

;;;; General configuration
;;;;; Defaults

;; Override various defaults
(use-package emacs
  :custom
  (inhibit-startup-screen t)           ; Disable the startup screen
  (indent-tabs-mode nil)               ; Insert space characters instead of tabs
  (tab-width 2)                        ; The number of spaces a tab is equal to
  (cursor-type 'bar)                   ; Display the cursor as a vertical bar
  (column-number-mode t)               ; Display the column number in the mode line
  (major-mode 'text-mode)              ; Set the default major mode to text-mode
  (ring-bell-function 'ignore)         ; Disable the beep sound triggered by various events
  (cursor-in-non-selected-windows nil) ; Hide the cursor in non-selected windows
  (inhibit-compacting-font-caches nil) ; Prevent compacting font caches during garbage collection
  :config
  (savehist-mode t)                    ; Save the minibuffer history
  (save-place-mode t)                  ; Jump to the last known position when reopening a file
  (electric-pair-mode t)               ; Enable automatic brackets pairing
  (global-hl-line-mode t)              ; Enable line highlighting in all buffers
  (delete-selection-mode t)            ; Replace selected text when typing
  (global-auto-revert-mode t)          ; Automatically reload externally modified files
  (fset 'yes-or-no-p 'y-or-n-p)        ; Replace "yes/no" prompts with "y/n"
  (prefer-coding-system 'utf-8)        ; Set default encoding to UTF-8
  (set-language-environment 'utf-8)    ; Set default language environment to UTF-8
  )

;; File-related customizations
(use-package files
  :ensure nil
  :custom
  (backup-directory-alist `(("." . ,(concat user-emacs-directory "backup"))))
  (backup-by-copying t)               ; Always use copying to create backup files
  (delete-old-versions t)             ; Delete excess backup versions
  (kept-new-versions 6)               ; Number of newest versions to keep when a new backup is made
  (kept-old-versions 2)               ; Number of oldest versions to keep when a new backup is made
  (version-control t)                 ; Make numeric backup versions unconditionally
  (create-lockfiles nil)              ; Stop creating lock files
  (auto-save-default nil)             ; Stop creating #autosave# files
  (delete-by-moving-to-trash t)       ; Move deleted files to the trash
  (mode-require-final-newline nil)    ; Don't add newlines at the end of files
  (large-file-warning-threshold nil)) ; Open large files without requesting confirmation

;; Disable file used for storing customization information
(setq custom-file (make-temp-file "emacs-custom-"))

;; Enable line numbering
(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode conf-mode) . display-line-numbers-mode))

;;;;; Fonts

;; Default font
(set-face-attribute 'default nil :family "Hack" :height 180)

;; Variable-pitch font
(set-face-attribute 'variable-pitch nil :family "Hack" :height 180)

;; Fixed-pitch
(set-face-attribute 'fixed-pitch nil :family "Hack")

;;;;; Spell checking

;; Configure ispell for multiple dictionaries
(use-package ispell
  :ensure nil
  :defer 0.5
  :custom
  (ispell-program-name "hunspell")
  ;; English (US), Hungarian, and Romanian
  (ispell-dictionary "en_US,hu_HU,ro_RO")
  :config
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,hu_HU,ro_RO"))

;; Enable automatic spell checking
(use-package flyspell
  :ensure nil
  :after ispell
  :bind ("C-c s" . flyspell-mode))

;;;; Packages
;;;;; Color schemes
;;;;;; Ef themes

(use-package ef-themes
  :init
  (load-theme 'ef-kassio t)
  :bind ("<f9>" . ef-themes-select))

;;;;;; Modus themes

(use-package modus-themes
  :defer t)

;;;;; General enhancements
;;;;;; Avy

;; A package for efficient character-based navigation
(use-package avy
  :bind ("M-s" . avy-goto-char))

;;;;;; Consult

;; Enhanced search and navigation commands
(use-package consult
  :bind (("C-s"     . consult-line)
         ("C-x b"   . consult-buffer)
         ("C-x C-r" . consult-recent-file)))

(use-package consult-notes
  :bind ("<f5>" . consult-notes)
  :custom
  (consult-notes-denote-files-function (function denote-directory-text-only-files))
  :config
  (consult-notes-denote-mode))

;;;;;; Dashboard

;; An extensible Emacs startup screen
(use-package dashboard
  :after nerd-icons
  :custom
  (dashboard-items '((recents  . 5)
                     (projects . 5)
                     (agenda   . 5)))
  (dashboard-set-footer nil)
  (dashboard-set-init-info t)
  (dashboard-center-content t)
  (dashboard-set-file-icons t)
  (dashboard-set-heading-icons t)
  (dashboard-startup-banner 'logo)
  (dashboard-projects-backend 'project-el)
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda ()
                                (get-buffer-create "*dashboard*")
                                (dashboard-refresh-buffer))))

;;;;;; Dired

(use-package dired
  :ensure nil
  :after nerd-icons-dired
  :bind ("C-x C-j" . dired-jump)
  :hook (dired-mode . (lambda ()
                        (nerd-icons-dired-mode)
                        (dired-hide-details-mode)))
  :custom
  (dired-auto-revert-buffer t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-hide-details-hide-information-lines nil)
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

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (:map dired-mode-map
              ("." . dired-hide-dotfiles-mode))
  :custom
  (dired-hide-dotfiles-verbose nil))

(use-package nerd-icons-dired
  :after nerd-icons)

;;;;;; Doom modeline

;; A modeline for Emacs inspired by the Doom theme collection
(use-package doom-modeline
  :after nerd-icons
  :init
  (doom-modeline-mode)
  :custom
  (doom-modeline-mu4e t)
  (doom-modeline-height 38))

;;;;;; Editorconfig

(use-package editorconfig
  :defer 0.5
  :config
  (editorconfig-mode))

;;;;;; Exec path

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-arguments nil)
  :config
  (exec-path-from-shell-initialize))

;;;;;; Helpful

;; Helpful improves the built-in Emacs help system by providing
;; more contextual information
(use-package helpful
  :bind
  ([remap describe-key]      . helpful-key)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-function] . helpful-callable))

;;;;;; Ibuffer

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer))

(use-package nerd-icons-ibuffer
  :after (nerd-icons ibuffer)
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;;;;;; Icons

(use-package nerd-icons)

;;;;;; Indent guides

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-method 'character))

;;;;;; Magit

(use-package magit
  :bind ("C-c g" . magit-status))


;;;;;; Move text

(use-package move-text
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down))
  :config
  (move-text-default-bindings))

;;;;;; Olivetti

(use-package olivetti
  :hook ((org-mode          . olivetti-mode)
         (markdown-mode     . olivetti-mode)
         (mu4e-view-mode    . olivetti-mode)
         (elfeed-show-mode  . olivetti-mode)
         (mu4e-compose-mode . olivetti-mode))
  :custom
  (olivetti-body-width 80))

;;;;;; Outshine

(use-package outshine
  :defer t)

;;;;;; Rainbow delimiters

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;;;;; Try

(use-package try
  :defer t)

;;;;;; Vertico

(use-package vertico
  :custom
  (vertico-cycle t)
  :config
  (vertico-mode))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
	      ("RET"   . vertico-directory-enter)
	      ("DEL"   . vertico-directory-delete-char)
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

(use-package nerd-icons-completion
  :after nerd-icons
  :config
  (nerd-icons-completion-mode))

;;;;;; Web mode

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

;;;;;; Which key

(use-package which-key
  :defer 0.2
  :custom
  (which-key-idle-delay 1)
  :config
  (which-key-mode))

;;;;; Languages
;;;;;; Lua

(use-package lua-mode
  :mode "\\.lua\\'")

;;;;;; YAML

(use-package yaml-mode
  :mode "\\.yml\\'")

;;;;;; TOML

(use-package toml-mode
  :mode "\\.toml\\'")

;;;;;; Markdown

(use-package markdown-mode
  :init
  (setq markdown-command "multimarkdown")
  :hook (markdown-mode . (lambda () (display-line-numbers-mode -1)))
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;;;; Applications
;;;;; Email

(use-package mu4e
  :ensure nil
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

(use-package mu4e-alert
  :after mu4e
  :custom
  ;; Notify only of unread emails in the inbox
  (mu4e-alert-interesting-mail-query "flag:unread maildir:/INBOX/")
  :config
  (mu4e-alert-enable-mode-line-display)
  (mu4e-alert-set-default-style 'libnotify))

;;;;; Elfeed

(use-package elfeed
  :preface
  (defun king/elfeed-search-mark-all-read ()
    "Mark all feeds as read"
    (interactive)
    (mark-whole-buffer)
    (elfeed-search-untag-all-unread))

  (defun king/elfeed-search-browse-url (&optional use-generic-p)
    "Open selected feeds in a browser"
    (interactive "P")
    (let ((entries (elfeed-search-selected)))
      (cl-loop for entry in entries
               when (elfeed-entry-link entry)
               do (if use-generic-p
                      (browse-url-generic (elfeed-entry-link entry))
                    (browse-url (elfeed-entry-link entry))))
      (mapc #'elfeed-search-update-entry entries)
      (unless (or elfeed-search-remain-on-entry (use-region-p)))))

  (defun king/elfeed-search-open-enclosure (&optional use-generic-p)
    "Play podcasts and YouTube videos"
    (interactive "P")
    (let ((entries (elfeed-search-selected)))
      (cl-loop for entry in entries
               when (elfeed-entry-link entry)
               do (call-process-shell-command
                   (format "mpv --force-window '%s'" (elfeed-entry-link entry)) nil 0))
      (mapc #'elfeed-search-update-entry entries)
      (unless (or elfeed-search-remain-on-entry (use-region-p))))
    (message "Loading...")
    (add-hook 'focus-out-hook (lambda () (message nil))))
  :bind (("C-c e" . elfeed)
         :map elfeed-search-mode-map
         ("M" . elfeed-toggle-starred)
         ("b" . king/elfeed-search-browse-url)
         ("R" . king/elfeed-search-mark-all-read)
         ("P" . king/elfeed-search-open-enclosure))
  :custom
  (elfeed-db-directory (concat user-emacs-directory "elfeed/"))
  :config
  (setq shr-width 80))

(use-package elfeed-search
  :ensure nil
  :after elfeed
  :custom
  (elfeed-search-title-max-width 100)
  (elfeed-search-filter "@3-months-ago +unread ")
  :config
  ;; Star and unstar feeds
  (defalias 'elfeed-toggle-starred
    (elfeed-expose #'elfeed-search-toggle-all 'starred))
  ;; Custom tag faces
  (defface elfeed-search-starred-title-face nil "Starred feeds")
  (push '(starred elfeed-search-starred-title-face) elfeed-search-face-alist)
  (defface elfeed-search-podcast-title-face nil "Podcast entries")
  (push '(podcast elfeed-search-podcast-title-face) elfeed-search-face-alist)
  (defface elfeed-search-youtube-title-face nil "YouTube entries")
  (push '(youtube elfeed-search-youtube-title-face) elfeed-search-face-alist))

(use-package elfeed-org
  :after elfeed
  :custom
  (rmh-elfeed-org-files '("~/orgfiles/elfeed.org"))
  :config
  (elfeed-org))

;;;;; Emms

(use-package emms
  :bind (("C-c p"  . emms)
         ("C-c P"  . emms-browser)
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

;;;; Custom key bindings

(keymap-global-unset "C-z")                 ; Disable C-z
(keymap-global-set "M-o" 'other-window)     ; Bind M-o to other-window
(keymap-global-set "M-z" 'zap-up-to-char)   ; Bind M-z to zap-up-to-char
(keymap-global-set "C-S-d" 'duplicate-line) ; Bind C-S-d to duplicate-line

;; Disable secondary selection commands
(keymap-global-unset "M-<mouse-1>")
(keymap-global-unset "M-<mouse-2>")
(keymap-global-unset "M-<mouse-3>")
(keymap-global-unset "M-<drag-mouse-1>")
(keymap-global-unset "M-<down-mouse-1>")

;;;; Custom functions

;; Move the cursor to the first non-whitespace character of the line.
;; If the cursor is already there, then move it to the beginning of the line.

(defun king/smarter-beginning-of-line ()
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

(keymap-global-set "C-a" 'king/smarter-beginning-of-line)

;; Use colors from the active theme palette
(defun king/colors-active-theme ()
  (let ((next      "violetred")
        (repeat    "violetred")
        (waiting   "slateblue")
        (postponed "chocolate")
        (someday   "chocolate")
        (delegated "slateblue")
        (project   "royalblue")
        (failed    "slategray")
        (cancelled "slategray")
        (starred   "violetred")
        (podcast   "darkcyan")
        (youtube   "chocolate"))
    (when (and (featurep 'ef-themes) (ef-themes--list-enabled-themes))
      (ef-themes-with-colors
        (setq next      magenta-warmer
              repeat    magenta-warmer
              waiting   magenta-cooler
              postponed yellow-warmer
              someday   yellow-warmer
              delegated magenta-cooler
              project   blue
              failed    fg-dim
              cancelled fg-dim
              starred   magenta-warmer
              podcast   cyan-cooler
              youtube   yellow-warmer)))
    (setq org-todo-keyword-faces
          `(("NEXT"      . (:foreground ,next      :weight bold))
            ("REPEAT"    . (:foreground ,repeat    :weight bold))
            ("WAITING"   . (:foreground ,waiting   :weight bold))
            ("POSTPONED" . (:foreground ,postponed :weight bold))
            ("SOMEDAY"   . (:foreground ,someday   :weight bold))
            ("DELEGATED" . (:foreground ,delegated :weight bold))
            ("PROJECT"   . (:foreground ,project   :weight bold))
            ("FAILED"    . (:foreground ,failed    :weight bold))
            ("CANCELLED" . (:foreground ,cancelled :weight bold))))

    ;; Elfeed
    (custom-set-faces
     `(elfeed-search-starred-title-face ((t :foreground ,starred)))
     `(elfeed-search-podcast-title-face ((t :foreground ,podcast)))
     `(elfeed-search-youtube-title-face ((t :foreground ,youtube))))

    ;; Restart Org mode
    (when (derived-mode-p 'org-mode)
      (org-mode-restart))))

(king/colors-active-theme)
(add-hook 'ef-themes-post-load-hook 'king/colors-active-theme)


;;;; Custom input methods

(quail-define-package
   "custom-input-method" "" "" t
   "Custom input method

  Documentation goes here."
   nil t nil nil nil nil nil nil nil nil t)

  (quail-define-rules
   ;; Phonetic symbols
   ("\\uh" ?ə) ; UNSTRESSED SCHWA VOWEL
   ("\\uH" ?ʌ) ; STRESSED SCHWA VOWEL
   ("\\ii" ?ɪ) ; NEAR-CLOSE NEAR-FRONT UNROUNDED VOWEL
   ("\\uu" ?ʊ) ; NEAR-CLOSE NEAR-BACK ROUNDED VOWEL
   ("\\ee" ?ɛ) ; OPEN-MID FRONT UNROUNDED VOWEL
   ("\\er" ?ɜ) ; OPEN-MID CENTRAL UNROUNDED VOWEL
   ("\\oh" ?ɔ) ; OPEN-MID BACK ROUNDED VOWEL
   ("\\ae" ?æ) ; NEAR-OPEN FRONT UNROUNDED VOWEL
   ("\\ah" ?ɑ) ; OPEN BACK UNROUNDED VOWEL
   ("\\th" ?θ) ; VOICELESS DENTAL FRICATIVE
   ("\\tH" ?ð) ; VOICED DENTAL FRICATIVE
   ("\\sh" ?ʃ) ; VOICELESS POSTALVEOLAR FRICATIVE
   ("\\zs" ?ʒ) ; VOICED POSTALVEOLAR FRICATIVE
   ("\\be" ?β) ; VOICED BILABIAL FRICATIVE
   ("\\vv" ?ɣ) ; VOICED VELAR FRICATIVE
   ("\\hh" ?ɥ) ; VOICED LABIAL-PALATAL APPROXIMANT
   ("\\la" ?ʎ) ; VOICED PALATAL LATERAL APPROXIMANT
   ("\\jj" ?ʝ) ; VOICED PALATAL FRICATIVE
   ("\\mm" ?ɱ) ; VOICED LABIODENTAL NASAL
   ("\\ts" ?ʧ) ; VOICELESS POSTALVEOLAR AFFRICATE
   ("\\dz" ?ʤ) ; VOICED POSTALVEOLAR AFFRICATE
   ("\\ny" ?ɲ) ; VOICED PALATAL NASAL
   ("\\ng" ?ŋ) ; VOICED VELAR NASAL
   ("\\rr" ?ɹ) ; VOICED ALVEOLAR APPROXIMANT
   ("\\ta" ?ɾ) ; VOICED ALVEOLAR TAP
   ("\\ir" ?ʁ) ; VOICED UVULAR FRICATIVE
   ("\\dl" ?ɫ) ; VELARIZED ALVEOLAR LATERAL APPROXIMANT
   ("\\as" ?ʰ) ; ASPIRATED
   ("\\ps" ?ˈ) ; PRIMARY STRESS
   ("\\ss" ?ˌ) ; SECONDARY STRESS
   ("\\li" ?‿) ; LIAISON
   ("\\ri" ?↗) ; RISING INFLECTION
   ("\\fi" ?↘) ; FALLING INFLECTION
   ("\\lw" ?ʷ) ; LABIAL HIGH ROUNDED
   ("\\ly" ?ʸ) ; PALATAL HIGH UNROUNDED
   ("\\st" ?̚) ; NO AUDIBLE RELEASE

   ;; Common symbols
   ("\\copy"   ?©)  ; COPYRIGHT
   ("\\tm"     ?™)  ; TRADEMARK
   ("\\mdot"   ?·)  ; INTERPUNCT
   ("\\ha"     ?á)  ; A-ACUTE
   ("\\endash" ?–)  ; EN DASH
   ("\\emdash" ?—)  ; EM DASH
   ("\\female" ?♀)  ; FEMALE
   ("\\male"   ?♂)  ; MALE
   ("\\eur"    ?€)) ; EURO

;;;; Org mode
;;;;; Org

(use-package org
  :ensure nil
  :hook (org-mode . (lambda ()
                      (org-indent-mode)))
  :bind (:map org-mode-map
              ("C-c l" . org-store-link)
              ("C-a"   . org-beginning-of-line))
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
  (org-habit-completed-glyph ?✓)
  (org-agenda-window-setup 'current-window))

;;;;; Appear

(use-package org-appear
  :hook (org-mode . org-appear-mode))

;;;;; Bullets

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  (org-superstar-configure-like-org-bullets))

;;;;; Denote

(use-package denote
  :after org
  :bind ("C-c d" . denote)
  :hook (dired-mode . denote-dired-mode)
  :custom
  (denote-sort-keywords t)
  (denote-directory "~/notes/")
  (denote-allow-multi-word-keywords nil))

;;;;; Calendar

(use-package holidays
  :ensure nil
  :after org
  :custom
  (holiday-bahai-holidays nil)
  (holiday-christian-holidays
   '((holiday-fixed  1  6     "Epiphany (Vízkereszt)")
     (holiday-easter-etc -46  "Ash Wednesday (Hamvazószerda)")
     (holiday-easter-etc -7   "Palm Sunday (Virágvasárnap)")
     (holiday-easter-etc -2   "Holy Friday (Nagypéntek)")
     (holiday-easter-etc  0   "Easter Sunday (Húsvétvasárnap)")
     (holiday-easter-etc  1   "Easter Monday (Húsvéthétfő)")
     (holiday-easter-etc 39   "Ascension (Áldozócsütörtök)")
     (holiday-easter-etc 49   "Pentecost (Pünkösd)")
     (holiday-easter-etc 56   "Trinity Sunday (Szentháromság Vasárnapja)")
     (holiday-easter-etc 60   "Corpus Christi (Úrnapja)")
     (holiday-greek-orthodox-easter)
     (holiday-fixed  8 15     "Assumption (Nagyboldogasszony)")
     (holiday-fixed 11  1     "All Saints' Day (Mindenszentek Napja)")
     (holiday-fixed 11  2     "Day of the Dead (Hallotak Napja)")
     (holiday-fixed 12 25     "Christmas Day (Karácsony Napja)")))
  (holiday-general-holidays
   '((holiday-fixed  1  1     "New Year's Day (Újév)")
     (holiday-fixed  2 14     "Valentine's Day (Valentin Nap)")
     (holiday-fixed  3  8     "International Women's Day (Nemzetközi Nőnap)")
     (holiday-fixed 10 31     "Halloween (Észak-Amerikai Ünnep)")
     (holiday-float 11  4  4  "Thanksgiving (Észak-Amerikai Ünnep)")))
  (holiday-local-holidays
   '((holiday-fixed  5  1     "Labor Day (A Munka Ünnepe)")
     (holiday-float  5  0  1  "Mother's Day (Anyák Napja)")))
  (holiday-hebrew-holidays nil)
  (holiday-islamic-holidays nil)
  (holiday-oriental-holidays nil))

;;;;; Export

(use-package ox-latex
  :ensure nil
  :defer 0.5
  :custom
  (org-latex-compiler "xelatex")
  :config
  (add-to-list
   'org-latex-classes
   '("org-plain-latex"
     "\\documentclass{article}
     [NO-DEFAULT-PACKAGES]
     [PACKAGES]
     [EXTRA]"
     ("\\section{%s}"       . "\\section*{%s}")
     ("\\subsection{%s}"    . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}"     . "\\paragraph*{%s}")
     ("\\subparagraph{%s}"  . "\\subparagraph*{%s}"))))

;;;; Other

(add-hook 'after-init-hook
          `(lambda ()
             ;(setq gc-cons-threshold 800000)
             (setq gc-cons-threshold (* 20 1024 1024))
             (garbage-collect)) t)

;;; init.el ends here
