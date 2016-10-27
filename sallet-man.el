;;; sallet-man.el --- Sallet for man(1)

;; Copyright (C) 2016 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 27th October 2016
;; Package-requires: ((dash "2.10.0"))
;; Keywords: convenience, help

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

(require 'sallet-source)

(sallet-defsource man (asyncio)
  (generator
   (sallet-make-generator-linewise-asyncio
    (sallet-process-creator-first-token-only
     (lambda (prompt)
       (start-process "man" nil "man" "-k" prompt)))
    'identity))
  (header "man")
  (action (lambda (_ c)
            (man (car (split-string c " "))))))

(provide 'sallet-man)
;;; sallet-man.el ends here
