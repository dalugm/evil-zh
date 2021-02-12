;;; evil-zh.el --- Evil search Chinese characters -*- lexical-binding: t; -*-

;; Copyright (C) 2021 dalu

;; Author: dalu <mou.tong@qq.com>
;; Maintainer: dalu <mou.tong@qq.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25") (evil "1"))
;; Homepage: https://github.com/dalugm/evil-zh
;; Keywords: Chinese, location

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package extends evil to search Chinese characters.
;;

;;; Code:

(require 'evil)
(require 'zh-lib)

(defvar evil-zh-with-search-rule 'custom
  "Enable the /search/ feature.

Possible values:
- 'always: always enable zhongwen search.
- 'never: never enable zhongwen search.
- 'custom: enable zhongwen search when pattern started with, default `:'.")

(defvar evil-zh-start-pattern ":"
  "`evil-zh' start pattern.")

(evil-define-motion find-char (count char)
  "Move to the next COUNT'th occurrence of CHAR."
  :type inclusive
  (interactive "<c><C>")
  (setq count (or count 1))
  (let ((fwd (> count 0)))
    (setq evil-last-find (list #'find-char char fwd))
    (when fwd (forward-char))
    (let ((case-fold-search nil))
      (unless
        (prog1
          (search-forward-regexp
            (zh-lib-build-regexp char)
            (unless evil-cross-lines
              (if fwd
                  (line-end-position)
                (line-beginning-position)))
            t count)
          (when fwd (backward-char)))
        (user-error "Can't find %c" char)))))

(evil-define-motion find-char-backward (count char)
  "Move to the previous COUNT'th occurrence of CHAR."
  :type exclusive
  (interactive "<c><C>")
  (find-char (- (or count 1)) char))

(evil-define-motion find-char-to (count char)
  "Move before the next COUNT'th occurrence of CHAR."
  :type inclusive
  (interactive "<c><C>")
  (unwind-protect
    (progn
      (find-char count char)
      (if (> (or count 1) 0)
          (backward-char)
        (forward-char)))
    (setcar evil-last-find #'find-char-to)))

(evil-define-motion find-char-to-backward (count char)
  "Move before the previous COUNT'th occurrence of CHAR."
  :type exclusive
  (interactive "<c><C>")
  (find-char-to (- (or count 1)) char))

(evil-define-motion repeat-find-char (count)
  "Repeat the last find COUNT times."
  :type inclusive
  (setq count (or count 1))
  (if evil-last-find
      (let ((cmd (car evil-last-find))
             (char (nth 1 evil-last-find))
             (fwd (nth 2 evil-last-find))
             evil-last-find)
        ;; ensure count is non-negative
        (when (< count 0)
          (setq count (- count)
            fwd (not fwd)))
        ;; skip next character when repeating t or T
        (and (eq cmd #'find-char-to)
          evil-repeat-find-to-skip-next
          (= count 1)
          (or (and fwd (or (= (char-after (1+ (point))) char)
                         (string-match-p
                           (zh-lib-build-regexp char)
                           (string (char-after (1+ (point)))))))
            (and (not fwd) (or (= (char-before) char)
                             (string-match-p
                               (zh-lib-build-regexp char)
                               (string (char-before))))))
          (setq count (1+ count)))
        (funcall cmd (if fwd count (- count)) char)
        (unless (nth 2 evil-last-find)
          (setq evil-this-type 'exclusive)))
    (user-error "No previous search")))

(evil-define-motion repeat-find-char-reverse (count)
  "Repeat the last find COUNT times in the opposite direction."
  :type inclusive
  (repeat-find-char (- (or count 1))))

;;;###autoload
(define-minor-mode evil-zh-mode
  "Evil search Chinese characters by zhongwen."
  :init-value nil
  (advice-add 'evil-ex-pattern-regex :around
    #'evil-ex-pattern-regex-advice)
  (if (and evil-zh-mode evil-motion-state-local-map)
      (progn
        (define-key evil-motion-state-local-map
          [remap evil-find-char]
          #'find-char)
        (define-key evil-motion-state-local-map
          [remap evil-find-char-backward]
          #'find-char-backward)
        (define-key evil-motion-state-local-map
          [remap evil-find-char-to]
          #'find-char-to)
        (define-key evil-motion-state-local-map
          [remap evil-find-char-to-backward]
          #'find-char-to-backward)
        (define-key evil-motion-state-local-map
          [remap evil-repeat-find-char]
          #'repeat-find-char)
        (define-key evil-motion-state-local-map
          [remap evil-repeat-find-char-reverse]
          #'repeat-find-char-reverse))
    (when evil-motion-state-local-map
      (define-key evil-motion-state-local-map
        [remap evil-find-char] nil)
      (define-key evil-motion-state-local-map
        [remap evil-find-char-backward] nil)
      (define-key evil-motion-state-local-map
        [remap evil-find-char-to] nil)
      (define-key evil-motion-state-local-map
        [remap evil-find-char-to-backward] nil)
      (define-key evil-motion-state-local-map
        [remap evil-repeat-find-char] nil)
      (define-key evil-motion-state-local-map
        [remap evil-repeat-find-char-reverse] nil))))

(defun evil-ex-pattern-regex-advice (fn &rest args)
  "Advice for FN `evil-ex-pattern-regex' with ARGS args."
  (let ((re (apply fn args)))
    (if (and evil-zh-mode re evil-zh-mode
          (cond (; always
                  (eq evil-zh-with-search-rule 'always) t)
            (; never
              (eq evil-zh-with-search-rule 'never) nil)
            (; custom
              (eq evil-zh-with-search-rule 'custom)
              (and re (= (string-to-char re) (string-to-char evil-zh-start-pattern)))))
          (not (string-match-p "\[.*+?[\\$]" re)))
        (zh-lib-build-regexp (if (eq evil-zh-with-search-rule 'custom) (substring re 1) re))
      re)))

(defun evil-ex-pattern-clear()
  "Clear all pollutions."
  (advice-remove 'evil-ex-pattern-regex #'evil-ex-pattern-regex-advice))

;;;###autoload
(define-globalized-minor-mode
  global-evil-zh-mode
  evil-zh-mode
  evil-zh-mode)

(provide 'evil-zh)

;;; evil-zh.el ends here
