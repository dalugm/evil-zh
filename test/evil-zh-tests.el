;;; evil-zh-tests.el --- Tests for evil-zh -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'evil-zh)

(ert-deftest evil-zh-repeat-find-char-is-safe-at-buffer-boundary ()
  (with-temp-buffer
    (insert "a")
    (goto-char (point-max))
    (let ((evil-last-find (list #'evil-zh-find-char-to ?a t))
          (evil-repeat-find-to-skip-next t)
          seen-count)
      (cl-letf (((symbol-function 'evil-zh-find-char-to)
                 (lambda (count _char)
                   (setq seen-count count))))
        (evil-zh-repeat-find-char 1))
      (should (= seen-count 1)))))

(ert-deftest evil-zh-mode-uses-its-own-remapping-map ()
  (let ((original (lookup-key evil-motion-state-map
                              [remap evil-find-char])))
    (unwind-protect
        (with-temp-buffer
          (evil-local-mode 1)
          (evil-normal-state)
          (evil-zh-mode 1)
          (should (eq (command-remapping #'evil-find-char)
                      #'evil-zh-find-char)))
      (evil-zh-mode -1))
    (should (equal (lookup-key evil-motion-state-map
                               [remap evil-find-char])
                   original))
    (should-not (advice-member-p #'evil-zh--ex-pattern-regex-advice
                                 'evil-ex-pattern-regex))))

(ert-deftest evil-zh-custom-search-requires-prefix ()
  (let ((evil-zh-search-rule 'custom))
    (should-not (evil-zh--ex-pattern-regex-advice
                 (lambda (&rest _args) nil)))
    (should (equal (evil-zh--ex-pattern-regex-advice
                    (lambda (&rest _args) "plain"))
                   "plain"))
    (let ((regexp (evil-zh--ex-pattern-regex-advice
                   (lambda (&rest _args) ":a"))))
      (should-not (equal regexp ":a"))
      (should (string-match-p regexp "a"))
      (should (string-match-p regexp "阿")))))

(provide 'evil-zh-tests)
;;; evil-zh-tests.el ends here
