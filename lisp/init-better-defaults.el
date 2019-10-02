;;; init-helm.el --- -*- lexical-binding: t no-byte-compile: t; -*-
;;
;; Copyright (C) 2019 Yilun Guan
;;
;; Author: Yilun Guan <zoom.aaron@gmail.com>
;; URL: https://github.com/guanyilun/.emacs.d-light/
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Better default configurations
;;
;;
;;; Code:

;; Fill column
(setq-default fill-column 70) 

;; Indentation no tab
(setq-default indent-tabs-mode nil)

;; show column number
(setq column-number-mode t)

;; Remove menu bar and toolbar
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(fset 'yes-or-no-p 'y-or-n-p)

;; split and follow
(global-set-key "\C-x2" (lambda () (interactive) (split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive) (split-window-horizontally) (other-window 1)))

;; type with a selected region removes it
(delete-selection-mode t)

;; highlight current line
 (global-hl-line-mode nil)

;; show parenthesis
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)

  ;; Highlight enclosing parenthesis as well
  (define-advice show-paren-function (:around (fn) fix-show-paren-function)
    (cond ((looking-at-p "\\s(") (funcall fn))
          (t (save-excursion
               (ignore-errors (backward-up-list))
               (funcall fn))))))

;; better defaults for buffer management 
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder
(setq make-backup-files nil)               ; Forbide to make backup files
(setq auto-save-default nil)               ; Disable auto save


(provide 'init-better-defaults)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-better-defaults.el ends here
