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
;; Python configurations
;;

;; Code:
;; (use-package yapfify
;;   :diminish yapf-mode
;;   ;; do not turn on unless told to do so
;;   :hook (python-mode . yapf-mode)
;;   ))

;; enable elpy
;; (use-package elpy
;;   :ensure t
;;   :init
;;   (elpy-enable))

;; (add-hook 'python-mode-hook 'linum-mode)
(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  )
(provide 'init-python)
