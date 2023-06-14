;;; evil-zh.el --- Evil search Chinese characters -*- lexical-binding: t; -*-

;; Author: dalu <mou.tong@qq.com>
;; Maintainer: dalu <mou.tong@qq.com>
;; Version: 0.1.0
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

(defgroup evil-zh nil
  "Search Zhongwen when searching with evil."
  :group 'evil-zh)

(defcustom evil-zh-search-rule 'custom
  "Enable the /search/ feature.

Possible values:
- \\='always: always enable zhongwen search.
- \\='never: never enable zhongwen search.
- \\='custom: enable zhongwen search with a pre-char, default `:'."
  :type 'symbol
  :group 'evil-zh)

(defcustom evil-zh-pre-char ?:
  "The prepend char to make `evil-zh' start zhongwen search."
  :type 'char
  :group 'evil-zh)

(evil-define-motion evil-zh-find-char (count char)
  "Move to the next COUNT'th occurrence of CHAR.

Movement is restricted to the current line unless
`evil-cross-lines' is non-nil."
  :type inclusive
  (interactive "<c><C>")
  (setq count (or count 1))
  (let ((fwd (> count 0))
        (visual (and evil-respect-visual-line-mode
                     visual-line-mode))
        case-fold-search)
    (setq evil-last-find (list #'evil-zh-find-char char fwd))
    (when fwd (evil-forward-char 1 evil-cross-lines))
    (unless (prog1
                (search-forward-regexp
                 (zh-lib-build-regexp char)
                 (cond
                  (evil-cross-lines
                   nil)
                  ((and fwd visual)
                   (save-excursion
                     (end-of-visual-line)
                     (point)))
                  (fwd
                   (line-end-position))
                  (visual
                   (save-excursion
                     (beginning-of-visual-line)
                     (point)))
                  (t
                   (line-beginning-position)))
                 t count)
              (when fwd (backward-char)))
      (user-error "Can't find `%c'" char))))

(evil-define-motion evil-zh-find-char-backward (count char)
  "Move to the previous COUNT'th occurrence of CHAR."
  :type exclusive
  (interactive "<c><C>")
  (evil-zh-find-char (- (or count 1)) char))

(evil-define-motion evil-zh-find-char-to (count char)
  "Move before the next COUNT'th occurrence of CHAR."
  :type inclusive
  (interactive "<c><C>")
  (unwind-protect
      (progn
        (evil-zh-find-char count char)
        (if (> (or count 1) 0)
            (backward-char)
          (forward-char)))
    (setcar evil-last-find #'evil-zh-find-char-to)))

(evil-define-motion evil-zh-find-char-to-backward (count char)
  "Move before the previous COUNT'th occurrence of CHAR."
  :type exclusive
  (interactive "<c><C>")
  (evil-zh-find-char-to (- (or count 1)) char))

(evil-define-motion evil-zh-repeat-find-char (count)
  "Repeat the last find COUNT times."
  :type inclusive
  (setq count (or count 1))
  (if evil-last-find
      (let ((cmd (car evil-last-find))
            (char (nth 1 evil-last-find))
            (fwd (nth 2 evil-last-find))
            evil-last-find)
        ;; Ensure count is non-negative.
        (when (< count 0)
          (setq count (- count)
                fwd (not fwd)))
        ;; Skip next character when repeating t or T.
        (and (eq cmd #'evil-zh-find-char-to)
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

(evil-define-motion evil-zh-repeat-find-char-reverse (count)
  "Repeat the last find COUNT times in the opposite direction."
  :type inclusive
  (evil-zh-repeat-find-char (- (or count 1))))

(defun evil-zh--ex-pattern-regex-advice (fn &rest args)
  "Advice for FN `evil-ex-pattern-regex' with ARGS."
  (let ((re (apply fn args)))
    (if (and re
             (cond
              ((eq evil-zh-search-rule 'always) t)  ; always
              ((eq evil-zh-search-rule 'never) nil) ; never
              ((eq evil-zh-search-rule 'custom)     ; custom
               (and re (= (string-to-char re)
                          evil-zh-pre-char))))
             (not (string-match-p
                   (rx "[" (+? (zero-or-more nonl)) "]" (opt "$"))
                   re)))
        (zh-lib-build-regexp (if (eq evil-zh-search-rule 'custom)
                                 (substring re 1)
                               re))
      re)))

;;;###autoload
(define-minor-mode evil-zh-mode
  "Evil search Chinese characters by zhongwen."
  :init-value nil
  (evil-normalize-keymaps)
  (if evil-zh-mode
      (progn
        (advice-add 'evil-ex-pattern-regex
                    :around #'evil-zh--ex-pattern-regex-advice)
        (define-key evil-motion-state-local-map
                    [remap evil-find-char]
                    #'evil-zh-find-char)
        (define-key evil-motion-state-local-map
                    [remap evil-find-char-backward]
                    #'evil-zh-find-char-backward)
        (define-key evil-motion-state-local-map
                    [remap evil-find-char-to]
                    #'evil-zh-find-char-to)
        (define-key evil-motion-state-local-map
                    [remap evil-find-char-to-backward]
                    #'evil-zh-find-char-to-backward)
        (define-key evil-motion-state-local-map
                    [remap evil-repeat-find-char]
                    #'evil-zh-repeat-find-char)
        (define-key evil-motion-state-local-map
                    [remap evil-repeat-find-char-reverse]
                    #'evil-zh-repeat-find-char-reverse))
    (progn
      (advice-remove 'evil-ex-pattern-regex
                     #'evil-zh--ex-pattern-regex-advice)
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

;;;###autoload
(define-globalized-minor-mode
  global-evil-zh-mode
  evil-zh-mode
  evil-zh-mode
  :group 'evil-zh)

(provide 'evil-zh)

;;; evil-zh.el ends here
