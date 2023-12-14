;;; early-init.el --- Early init file -*- lexical-binding: t; -*-

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

;; This file is loaded before the package system and GUI is initialized
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html

;;; Code:

;; Reduce the frequency of garbage collection
(setq gc-cons-threshold (* 128 1024 1024))

(defvar file-name-handler-alist-old file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'after-init-hook
          #'(lambda ()
              (setq file-name-handler-alist file-name-handler-alist-old)))

;; Maximize the Emacs frame on startup
(push '(fullscreen . maximized) default-frame-alist)

;; Set default frame size to 1920x1080 pixels
(push '(width  . (text-pixels . 1920)) default-frame-alist)
(push '(height . (text-pixels . 1080)) default-frame-alist)

;; Set background color to prevent startup glare
(push '(background-color . "#fff2f3") default-frame-alist)

;; Remove host name from titlebar information
(setq frame-title-format '(multiple-frames "%b" ("" "%b - GNU Emacs")))

;; Disable the menu bar, tool bar, and scroll bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Suppress warnings and errors during asynchronous native compilation
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors nil))

;;; early-init.el ends here
