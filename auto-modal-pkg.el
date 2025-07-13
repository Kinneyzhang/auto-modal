;;; auto-modal-pkg.el --- Package metadata for auto-modal -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Kinney Zhang

;; This file is part of auto-modal.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; Package metadata for auto-modal.

;;; Code:

(define-package "auto-modal" "0.1.0"
  "Automatically switch to a VI-like control mode based on different conditions"
  '((emacs "29.1")
    (bind-key "2.4"))
  :authors '(("Kinney Zhang" . "kinneyzhang666@gmail.com"))
  :maintainer '("Kinney Zhang" . "kinneyzhang666@gmail.com")
  :keywords '("convenience" "modal" "editing")
  :url "https://github.com/Kinneyzhang/auto-modal")

;;; auto-modal-pkg.el ends here