;;; sallet-projectile.el --- Sallet for projectile -*- lexical-binding: t -*-

;; Copyright (C) 2016 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 17th September 2016
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'dash)
(require 'projectile nil t)

(require 'sallet-source)

(sallet-defsource projectile-projects nil
  (candidates (lambda () (projectile-relevant-known-projects)))
  (matcher sallet-matcher-flx)
  (action (lambda (_source c) (projectile-switch-project-by-name c)))
  (header "Jump to project"))

(defun sallet-projectile-projects ()
  "Switch project."
  (interactive)
  (sallet (list sallet-source-projectile-projects)))


(provide 'sallet-projectile)
;;; sallet-projectile.el ends here
