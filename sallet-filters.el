;;; sallet-filters.el --- Common predicates, filters and matchers for sallet -*- lexical-binding: t -*-

;; Copyright (C) 2015 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 10th October 2015
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
(require 'flx)

(require 'sallet-core)
(require 'sallet-state)


;;; Predicates

(defun sallet--predicate-flx (candidate index pattern matches-property score-property &optional flx-cache)
  "Match CANDIDATE at INDEX against PATTERN and update its properties.

MATCHES-PROPERTY is the name of property where matching positions
of candidate are stored.

SCORE-PROPERTY is the score of the flx match of this CANDIDATE
against PATTERN. "
  (-when-let (flx-data (flx-score candidate pattern flx-cache))
    (sallet-update-index
     index
     (list matches-property (cdr flx-data) '-concat)
     (list score-property (car flx-data)))))

(defun sallet-predicate-flx (candidate index pattern)
  "Match and score CANDIDATE at INDEX against PATTERN."
  (sallet--predicate-flx candidate index pattern :flx-matches :flx-score))

(defun sallet-predicate-path-flx (candidate index pattern)
  "Match and score path CANDIDATE at INDEX against PATTERN."
  (sallet--predicate-flx candidate index pattern :flx-matches-path :flx-score-path flx-file-cache))

(defun sallet-predicate-buffer-major-mode (candidate index pattern)
  "Match and score CANDIDATE buffer's `major-mode' at INDEX against PATTERN.

Matching is done using flx alogrithm."
  (sallet--predicate-flx candidate index pattern :flx-matches-mm :flx-score-mm))

(defun sallet--predicate-regexp (candidate index pattern matches-property)
  "Match CANDIDATE at INDEX against PATTERN and update its properties.

MATCHES-PROPERTY is the name of property where matching positions
of candidate are stored."
  (save-match-data
    (when (string-match pattern candidate)
      (sallet-update-index
       index
       (list matches-property (cons (match-beginning 0) (match-end 0)) 'cons)))))

(defun sallet-predicate-regexp (candidate index pattern)
  "Match and score CANDIDATE at INDEX against PATTERN."
  (sallet--predicate-regexp candidate index pattern :regexp-matches))

(defun sallet-predicate-path-regexp (candidate index pattern)
  "Match and score path CANDIDATE at INDEX against PATTERN."
  (sallet--predicate-regexp candidate index pattern :regexp-matches-path))


;;; Filters

;; TODO: figure out how the caching works
(defun sallet-filter-flx (candidates indices pattern)
  "Match PATTERN against CANDIDATES at INDICES.

CANDIDATES is a vector of candidates.

INDICES is a list of processed candidates.

Uses the `flx' algorithm."
  (if (equal "" pattern) indices
    (--keep (sallet-predicate-flx (sallet-candidate-aref candidates it) it pattern) indices)))

(defun sallet-filter-path-flx (candidates indices pattern)
  "Match PATTERN against path CANDIDATES at INDICES.

CANDIDATES is a vector of candidates.

INDICES is a list of processed candidates.

Uses the `flx' algorithm."
  (if (equal "" pattern) indices
    (--keep (sallet-predicate-path-flx (sallet-candidate-aref candidates it) it pattern) indices)))

;; TODO: this shouldn't be written in terms of regexp matching but
;; something like flx only that it takes substrigs.  So we should
;; match "more important" parts first and score properly etc.
;; TODO: make a specialized version for file names
(defun sallet-filter-substring (candidates indices pattern)
  "Match PATTERN against CANDIDATES at INDICES.

CANDIDATES is a vector of candidates.

INDICES is a list of processed candidates.

Uses substring matching."
  (let ((quoted-pattern (regexp-quote pattern)))
    (--keep (sallet-predicate-regexp (sallet-candidate-aref candidates it) it quoted-pattern) indices)))

(defun sallet-filter-file-extension (candidates indices pattern)
  "Match PATTERN against CANDIDATES at INDICES matching pattern as file extension.

CANDIDATES is a vector of candidates.

INDICES is a list of processed candidates."
  (let ((quoted-pattern (concat "\\." (regexp-quote pattern) "[^.]*\\'")))
    (--keep (sallet-predicate-regexp (sallet-candidate-aref candidates it) it quoted-pattern) indices)))


;;; Filter combinators

(defun sallet--filter-flx-then-substring (candidates indices pattern flx-filter flx-score)
  "Match PATTERN against CANDIDATES with flx- or substring-matching.

FLX-FILTER is a filter using some flx algorithm, typically with
special preferences (file paths, general strings) for different
kinds of candidates.

FLX-SCORE is the property on which we decide whether to use flx
or substring maching.

This is an internal method, for the general logic see
`sallet-filter-flx-then-substring'."
  (if (or (not (consp (car indices)))
          (not (plist-member (cdar indices) :flx-score)))
      (funcall flx-filter  candidates indices pattern)
    (sallet-filter-substring candidates indices (regexp-quote pattern))))

(defun sallet-filter-flx-then-substring (candidates indices pattern)
  "Match PATTERN against CANDIDATES with flx- or substring-matching.

CANDIDATES are strings.

We use following check to determine which algorithm to use:
1. Pick the first index from INDICES.
2. If it contains metadata related to flx-matching, we substring
   match, otherwise flx-matching was never performed so we flx-match."
  (sallet--filter-flx-then-substring
   candidates indices pattern
   'sallet-filter-flx :flx-score))

(defun sallet-filter-path-flx-then-substring (candidates indices pattern)
  "Match PATTERN against path CANDIDATES with flx- or substring-matching.

CANDIDATES are strings.

We use following check to determine which algorithm to use:
1. Pick the first index from INDICES.
2. If it contains metadata related to flx-matching, we substring
   match, otherwise flx-matching was never performed so we flx-match."
  (sallet--filter-flx-then-substring
   candidates indices pattern
   'sallet-filter-path-flx :flx-score-path))

;; TODO: turn into a transformer returning a filter closure
(defun sallet-pipe-filters (filters candidates indices pattern)
  "Run all FILTERS in sequence, filtering CANDIDATES against PATTERN."
  (--reduce-from (funcall it candidates acc pattern) indices filters))

;; TODO: maybe we can add support for "... ..." patterns by "spliting
;; as sexps" in a temporary buffer where we set everything to word
;; syntax except spaces and quotes. Then each sexp is one token
;; (symbols converted to strings first)
;; TODO: Add optimization where we only re-run changed tokens.  We can
;; keep the index from the last update and just work on that
;; (similarly as we keep the pattern from one update ago).
;; TODO: turn into a transformer returning a filter closure
(defun sallet-compose-filters-by-pattern (filter-alist candidates indices pattern)
  "Filter CANDIDATES using rules from FILTER-ALIST.

FILTER-ALIST is an alist of (SUBPATTERN . FILTERS) or (SUBPATTERN
MATCH-GROUP . FILTERS).

SUBPATTERN is a regular expression, FILTERS is a list of filters,
MATCH-GROUP is a match group of SUBPATTERN.

First, PATTERN is split on whitespace into a list of TOKENS.

Then for each TOKEN we find first SUBPATTERN that matches it and
filter INDICES through the associated list of filters.  The
pattern passed to the filter is the value of first match group of
SUBPATTERN or MATCH-GROUP if specified.

The special SUBPATTERN `t' signifies a default branch.  As soon
as this SUBPATTERN is found the search stops and its filters are
applied.

Return INDICES filtered in this manner by all the TOKENS."
  (let* ((input (split-string pattern)))
    (-each input
      (lambda (token)
        (-when-let ((input . filters)
                    (--some
                     (let ((subpattern (car it))
                           (match-group (if (numberp (cadr it)) (cadr it) 0))
                           (filters (if (numberp (cadr it)) (cddr it) (cdr it))))
                       (if (eq t subpattern)
                           (cons token filters)
                         (when (string-match subpattern token)
                           (cons (match-string match-group token) filters))))
                     filter-alist))
          (unless (equal input "")
            (setq indices (sallet-pipe-filters filters candidates indices input))))))
    indices))

(defun sallet-make-tokenized-filter (filter)
  "Return a variant of FILTER which matches each token from input pattern separately.

Input pattern is split on whitespace to create list of tokens.
Each candidate is then matched against each token.  Only
candidates matching all tokens will pass the test."
  (lambda (candidates indices pattern)
    (let ((tokens (split-string pattern)))
      (--reduce-from (funcall filter candidates acc it) indices tokens))))

(defun sallet-ignore-first-token-filter (filter)
  "Return a variant of FILTER which ignores first token of prompt.

Input pattern is split on whitespace to create list of tokens.
The first token is dropped and then the resulting strings are
concatenated again and passed to FILTER.

This is useful in asyncio sources where we pass the first token
to the asyncio process and the rest to the matcher."
  (lambda (candidates indices pattern)
    (let* ((tokens (split-string pattern " "))
           (new-pattern (mapconcat 'identity (cdr tokens) " ")))
      (if (equal new-pattern "") indices
        (funcall filter candidates indices new-pattern)))))


;;; Matchers

(defun sallet-matcher-default (candidates state)
  "Default matcher.

The prompt is split on whitespace, then candidate must
substring-match each token to pass the test."
  (let ((prompt (sallet-state-get-prompt state))
        (indices (sallet-make-candidate-indices candidates)))
    (funcall (sallet-make-tokenized-filter 'sallet-filter-substring) candidates indices prompt)))

(defun sallet-matcher-flx-then-substring (candidates state)
  "Flx match on first token and then substring match on the rest."
  (let ((prompt (sallet-state-get-prompt state))
        (indices (sallet-make-candidate-indices candidates)))
    (funcall (sallet-make-tokenized-filter 'sallet-filter-flx-then-substring) candidates indices prompt)))

;; TODO: write a "defmatcher" macro which would automatically define
;; prompt and indices variables
(defun sallet-matcher-flx (candidates state)
  "Match candidates using flx matching."
  (let ((prompt (sallet-state-get-prompt state))
        (indices (sallet-make-candidate-indices candidates)))
    (sallet-filter-flx candidates indices prompt)))


;;; Matcher combinators

(defun sallet-make-matcher (filter)
  "Make a sallet matcher from a filter."
  (lambda (candidates state)
    (let ((prompt (sallet-state-get-prompt state))
          (indices (sallet-make-candidate-indices candidates)))
      (funcall filter candidates indices prompt))))


;;; Sorters

;; TODO: figure out how to compose this when multiple filters are in
;; place and not all of them provide the sorting attribute
(defun sallet-sorter-flx (processed-candidates _)
  "Sort PROCESSED-CANDIDATES by :flx-score."
  (sort processed-candidates
        (lambda (a b)
          ;; UGLY!!!!
          (if (and (consp a) (consp b))
              (-when-let* (((_ &keys :flx-score sa) a)
                           ((_ &keys :flx-score sb) b))
                (> sa sb))
            (> a b)))))

(provide 'sallet-filters)
;;; sallet-filters.el ends here
