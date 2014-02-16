;;; espuds.el --- Ecukes step definitions

;; Copyright (C) 2010-2013 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.2.6
;; Keywords: test
;; Package-Requires: ((s "1.7.0") (dash "2.2.0") (noflet "0.0.10") (f "0.12.1"))
;; URL: http://github.com/ecukes/espuds

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'f)
(require 's)
(require 'dash)
(require 'noflet)
(require 'cl-lib)
(require 'edmacro)


;;;; Variables

(eval-when-compile
  (defvar ecukes-message-log))

(defvar espuds-action-chain nil
  "List of actions to execute.")

(defvar espuds-chain-active nil
  "Is t if chaining is active, nil otherwise.")

(defvar espuds-previous-keyboard-input nil
  "Previous input command (keybinding).")


;;;; Helpers

(defun espuds-fake-eval (contents)
  "Dump CONTENTS to a temp file and then load it."
  (let ((file (make-temp-file "espuds-")))
    (f-write contents 'utf-8 file)
    (load file)))

(defun espuds-region ()
  "Return the text selected by region, if any."
  (if mark-active
      (buffer-substring-no-properties (region-beginning) (region-end))
    ""))

(defun espuds-quit ()
  "Quit without signal."
  (noflet ((signal (&rest args) nil))
    (keyboard-quit)))

(defun espuds-goto-line (line)
  "Go to LINE."
  (goto-char (point-min))
  (forward-line (1- line)))


;;;; Definitions

;; (Given "^\\(?:I am in buffer\\|I switch to buffer\\) BUFFER-NAME$")

(defun espuds-switch-to-buffer (buffer)
  "Switches to BUFFER."
  (if (s-matches? "\\s-" buffer)
      (switch-to-buffer buffer)
    (let ((v (vconcat [?\C-x ?b] (string-to-vector buffer))))
      (execute-kbd-macro v))))

(Given "I am in buffer :arg-1" 'espuds-switch-to-buffer)
(Given "I switch to buffer :arg-1" 'espuds-switch-to-buffer)

(Then "I should be in buffer :arg-1"
  "Asserts that the current buffer is BUFFER."
  (lambda (buffer)
    (let ((message "Expected to be in buffer '%s', but was in '%s'"))
      (cl-assert (equal buffer (buffer-name)) nil message buffer (buffer-name)))))

(Then "I should be in file :arg-1"
  "Asserts that the current buffer is connected to FILE."
  (lambda (file)
    (let ((file-name (buffer-file-name)))
      (if file-name
          (let ((match (equal file (file-name-nondirectory file-name))))
            (cl-assert match nil "Expected file to be '%s', but was '%s'." file file-name))
        (cl-assert file-name nil "Expected file to be '%s', but not visiting any file." file)))))

(Given "the buffer is empty"
  "Clears all text in the current buffer."
  'erase-buffer)

(Given "I clear the buffer"
  "Clears all text in the current buffer."
  'erase-buffer)

(When "I go to line :arg-1"
  "Goes to LINE if it exist."
 (lambda (line)
   (let ((num-lines (count-lines (point-min) (point-max)))
         (line-num (string-to-number line))
         (message "Requested line '%s', but buffer only has '%d' line(s)."))
     (cl-assert (<= line-num num-lines) nil message line num-lines)
     (espuds-goto-line line-num))))

(When "I go to point :arg-1"
  "Goes to POINT if it exist."
 (lambda (point)
   (let ((size (buffer-size))
         (point-num (string-to-number point))
         (message "Requested point '%s', but buffer only has '%d' point(s)."))
     (cl-assert (<= (1- point-num) size) nil message point-num size)
     (goto-char point-num))))

(When "I go to word :arg-1"
  "Go to WORD if it exist."
  (lambda (word)
    (goto-char (point-min))
    (let ((search (re-search-forward (format "\\b%s\\b" word) nil t))
          (message "Can not go to word '%s' since it does not exist in the current buffer: %s"))
      (cl-assert search nil message word (buffer-string)))
    (backward-char (length word))))

(Then "the cursor should be at point :arg-1"
  "Checks that the cursor is at a specific position."
  (lambda (point)
    (let ((message "Expected cursor to be at point '%s', but was at '%s'"))
      (cl-assert (= (string-to-number point) (point)) nil message point (point)))))

(Then "the cursor should be before :arg-1"
  "Checks that the cursor is before some text."
  (lambda (expected)
    (let ((actual
           (progn
             (buffer-substring-no-properties (point) (min (point-max) (+ (point) 5)))))
          (message "Expected '%s' to be before point but was '%s'."))
      (cl-assert (looking-at (regexp-quote expected)) nil message expected actual))))

(Then "the cursor should be after :arg-1"
  "Checks that the cursor is after some text."
  (lambda (expected)
    (let ((actual
           (progn
             (buffer-substring-no-properties (point) (max (point-min) (- (point) 5)))))
          (message "Expected '%s' to be after point but was '%s'."))
      (cl-assert (looking-back (regexp-quote expected)) nil message expected actual))))

(Then "the cursor should be between :arg-1 and :arg-2"
  "Checks that the cursor is between some text."
  (lambda (left right)
    (let ((search
           (and
            (looking-back (regexp-quote left))
            (looking-at (regexp-quote right))))
          (message "Expected '%s' to be left of point and '%s' to be right of point, but was: '%s[CURSOR]%s'")
          (before
           (buffer-substring-no-properties
            (max (point-min) (- (point) 5))
            (point)))
          (after
           (buffer-substring-no-properties
            (point)
            (min (point-max) (+ (point) 5)))))
      (cl-assert search nil message left right before after))))

(When "I place the cursor between :arg-1 and :arg-2"
  "Places the cursor between text."
  (lambda (left right)
    (goto-char (point-min))
    (let ((search (search-forward (concat left right) nil t))
          (message "Can not place cursor between '%s' and '%s', because there is no such point: '%s'"))
      (cl-assert search nil message left right (buffer-string)))
    (backward-char (length right))))

(When "I place the cursor before :arg-1"
  "Places the cursor before first instance of text."
  (lambda (arg)
    (goto-char (point-min))
    (let ((search (search-forward arg nil t))
          (message "Can not place cursor before '%s', because there is no such point: '%s'"))
      (backward-char (length arg))
      (cl-assert search nil message arg (buffer-string)))))

(When "I place the cursor after :arg-1"
  "Places the cursor after first instance of text."
  (lambda (arg)
    (goto-char (point-min))
    (let ((search (search-forward arg nil t))
          (message "Can not place cursor after '%s', because there is no such point: '%s'"))
      (cl-assert search nil message arg (buffer-string)))))

(When "I go to beginning of buffer"
  "Places the cursor at the beginning of buffer."
  'beginning-of-buffer)

(When "I go to end of buffer"
  "Places the cursor at the end of buffer."
  'end-of-buffer)

(When "I go to beginning of line"
  "Places the cursor at the beginning of the line."
  (lambda ()
    (call-interactively 'move-beginning-of-line)))

(When "I go to end of line"
  "Places the cursor at the end of the line."
  (lambda ()
    (call-interactively 'move-end-of-line)))

(When "I start an action chain"
  "Starts an action chain."
  (lambda ()
    (setq espuds-action-chain nil)
    (setq espuds-chain-active t)))

(When "I execute the action chain"
  "Executes the action chain."
  (lambda ()
    (execute-kbd-macro espuds-action-chain)
    (setq espuds-chain-active nil)))

(When "I press :arg-1"
  "Execute the function that KEYBINDING is bound to.

Note: If action chaining is active. Add KEYBINDING to the action
chain instead of executing."
  (lambda (keybinding)
    (when (and
           (equal espuds-previous-keyboard-input "C-y")
           (equal keybinding "M-y")
           (eq (key-binding (kbd "M-y")) 'yank-pop))
      (setq this-command 'yank))
    (let ((macro (edmacro-parse-keys keybinding)))
      (if espuds-chain-active
          (setq espuds-action-chain (vconcat espuds-action-chain macro))
        (if (and (equal keybinding "C-g")
                 (eq (key-binding (kbd "C-g")) 'keyboard-quit))
            (espuds-quit)
          (execute-kbd-macro macro))))
    (setq espuds-previous-keyboard-input keybinding)))

(When "I quit"
  "Quit without signal."
  'espuds-quit)

(When "I type :arg-1"
  "If action chaining is active. Add TYPING to the action
chain. Otherwise simulate the TYPING."
  (lambda (typing)
    (if espuds-chain-active
        (setq espuds-action-chain (vconcat espuds-action-chain (string-to-vector typing)))
      (execute-kbd-macro (string-to-vector typing)))))

(When "I turn on :arg-1"
  "Turns on some mode."
  (lambda (mode)
    (let ((v (vconcat [?\C-u 1 ?\M-x] (string-to-vector mode))))
      (execute-kbd-macro v))))

(When "I set :arg-1 to :arg-2"
  "Set some variable."
  (lambda (var val)
    (set (intern var) val)))

(When "I load the following\:"
  "Loads CONTENTS with Emacs load command."
  (lambda (contents)
    (espuds-fake-eval contents)))

(When "I open temp file :arg-1"
  "Creates a new temp file called FILE and opens it."
  (lambda (file)
    (find-file (make-temp-file file))))

(Then "I should see message :arg-1"
  "Asserts that MESSAGE has been printed."
  (lambda (message)
    (let ((msg "Expected '%s' to be included in the list of printed messages, but was not."))
      (setq message (s-replace "\\\"" "\"" message))
      (cl-assert (-contains? (-map 's-trim ecukes-message-log) message) nil msg message))))

(Given "there is no region selected"
  "Deactivates mark."
  (lambda ()
    (deactivate-mark)))

(Given "transient mark mode active"
  "Activates transient mark mode."
  (lambda (status)
    (transient-mark-mode 1)))

(Given "transient mark mode is inactive"
  "Deactivates transient mark mode."
  (lambda (status)
    (transient-mark-mode -1)))

(When "I set the mark"
  "Sets the mark at point."
  (lambda ()
    (set-mark (point))))

(When "I pop the mark"
  "Pop and move point to the top position on the mark-ring."
  (lambda ()
    (set-mark-command 4)))

(defun espuds-assert-region (expected)
  (let ((actual (espuds-region))
          (message "Expected the region to be '%s', but was '%s'."))
      (cl-assert (equal expected actual) nil message expected actual)))

(Then "the region should be\:" 'espuds-assert-region)
(Then "the region should be :arg-1" 'espuds-assert-region)

(Then "the region should not be active"
  "Asserts that the region is not active."
  (lambda ()
    (let ((message "Expected the region not to be active, but it was."))
      (cl-assert (not (region-active-p)) nil message))))

(defun espuds-insert-contents (contents)
  "Inserts CONTENTS into the current buffer."
  (insert contents))

(When "I insert\:" 'espuds-insert-contents)
(When "I insert :arg-1" 'espuds-insert-contents)

(defun espuds-assert-buffer-includes-text (expected)
  "Asserts that the current buffer includes some text."
  (let ((actual (buffer-string))
        (message "Expected '%s' to be part of '%s', but was not."))
    (cl-assert (s-contains? expected actual) nil message expected actual)))

(Then "I should see\:" 'espuds-assert-buffer-includes-text)
(Then "I should see :arg-1" 'espuds-assert-buffer-includes-text)

(defun espuds-assert-buffer-does-not-include-text (expected)
  "Asserts that the current buffer does not include some text."
  (let ((actual (buffer-string))
        (message "Expected '%s' to not be part of '%s', but was."))
    (cl-assert (not (s-contains? expected actual)) nil message expected actual)))

(Then "I should not see\:" 'espuds-assert-buffer-does-not-include-text)
(Then "I should not see :arg-1" 'espuds-assert-buffer-does-not-include-text)

(defun espuds-assert-buffer-matches-text (expected)
  "Asserts that the current buffer matches some text."
  (let ((actual (buffer-string))
        (message "Expected to see pattern '%s' in '%s', but did not."))
    (cl-assert
     (s-matches? expected actual) nil message expected actual)))

(Then "I should see pattern\:" 'espuds-assert-buffer-matches-text)
(Then "I should see pattern :arg-1" 'espuds-assert-buffer-matches-text)

(defun espuds-assert-buffer-does-not-match-text (expected)
  (let ((actual (buffer-string))
        (message "Expected to not see pattern '%s' in '%s', but did."))
    (cl-assert
     (not (s-matches? expected actual)) nil message expected actual)))

(Then "I should not see pattern\:" 'espuds-assert-buffer-does-not-match-text)
(Then "I should not see pattern :arg-1" 'espuds-assert-buffer-does-not-match-text)

(When "I select :arg-1"
  "Selects TEXT if found. Otherwise signal an error."
  (lambda (text)
    (goto-char (point-min))
    (let ((search (re-search-forward text nil t)))
      (cl-assert search nil "The text '%s' was not found in the current buffer." text))
    (set-mark (point))
    (re-search-backward text)))

(defun espuds-assert-buffer-empty ()
  "Asserts that there nothing to see in the current buffer."
  (let ((message "Expected buffer to be empty, but had content: '%s'"))
    (cl-assert (equal (buffer-size) 0) nil message (buffer-string))))

(Then "I should not see anything" 'espuds-assert-buffer-empty)
(Then "the buffer should be empty" 'espuds-assert-buffer-empty)



(provide 'espuds)

;;; espuds.el ends here
