;;; cbox.el --- inserting block commands for explaining big chunks of code

;; Copyright © 2017 Samuel Domínguez Lorenzo

;; Filename: cbox.el
;; Maintainer: samueldominguez@protonmail.com
;; Keywords: comment, box

;; Created: 05 Jan 2017
;; URL: https://github.com/samueldominguez/cbox

;;
;; Example configuration: (~/.emacs config):
;;
;; 1. Make sure cbox.el somewhere in your load
;;    path, it will load automatically like any
;;    other .el file in your load path.
;;
;; 2. Use autoload (or any other method) to make
;;    cbox visible:
;;    (autoload 'cbox-trigger "cbox" "Comment blocks editing" t)
;;
;; 3. Map cbox-trigger to your prefered
;;    keybinding e.g. :
;;    (global-set-key (kbd "C-c /") 'cbox-trigger)
;;
;; Usage:
;;
;; Invoke cbox-trigger, to start typing your
;; comment block and type C-c C-c to insert
;; the text into the original buffer at the
;; relative point (marker) in which you left.
;;
;; You can type C-c C-k to cancel.
;;

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;; cbox operation:
;; Steps:
;; 1.  User types keybinding mapped to cbox-trigger / invokes cbox-trigger
;; 2.  We save current position via marker
;; 3.  Split window vertically
;; 4.  Open temporary buffer
;; 5.  Major mode is set to fundamental and we set auto-fill minor mode
;; 6.  User types the comment text
;; 7.  User types C-c C-c / invokes cbox-trigger or C-c C-k and goto step 10
;; 8.  Parse all text in buffer to add necessary characters to box it in comments
;; 9.  Copy it to previously stored marker in original buffer
;; 10. Kill temporary buffer
;; 11. Kill window which held temporary buffer
;; 12. Go back to the buffer which invoked cbox-trigger originally
;; 13. Set mark to the next line of the last comment box line

;;    Default boxing style:

;; /*==================================*
;;  | This box has text from top to    |
;;  | bottom, so no empty lines at the |
;;  | top and bottom, and one space    |
;;  | at the start of each line        |
;;  *==================================*/

;;; Code:

(defconst cbox-comment-buffer-name " cbox edit "
  "Name given to the buffer used to write the comments")

(defvar cbox-insert-marker nil
  "Contains the point at which we will later insert the inputted comments")

(defvar cbox-comment-buffer nil
  "Contains the buffer used to write the comments")

(defvar cbox-source-buffer nil
  "Contains the buffer where cbox was called from")

(defvar cbox-comment-buffer-lines nil
  "Contains all the text lines in a list which make up cbox-comment-buffer,
this is created when the user finishes typing their comment and re-invoke
cbox-trigger")

(defvar cbox-comment-buffer-max-line-length nil
  "Contains the number of characters of the longest line in cbox-comment-buffer")

(defvar cbox-return-marker nil
  "Contains the marker to return to when we are done with everything")

(defvar cbox-editing-existing nil
  "Determines whether we are modifying an existing comment or if we
are creating a new comment")

(defvar cbox-original-comment nil
  "If editing an existing comment, this holds the original copy which will be
further proceesed to place in the temporary buffer and in case the user aborts
the operation we can put back exactly what we got in the first place")

(defvar cbox-original-comment-points nil
  "If editing an existing comment this holds the character counts that mark the are
the original comment is defined in")

(defvar cbox-original-text nil
  "Holds the processed text inside cbox-original-text")

(setq cbox-insert-marker (make-marker))
(setq cbox-return-marker (make-marker))

(defun cbox-trigger ()
  "Main function of cbox, it opens a buffer in text mode with auto-fill minor mode in a vertically
split window, allowing the user to type a comment and later when re-invoked insert the text in comment
form in the original buffer where cbox-trigger was initially invoked."
  (interactive)
  (if (eq (buffer-live-p cbox-comment-buffer) nil)
      (progn
	(set-marker cbox-insert-marker (point))
	(if (cbox-is-on-comment)
	    (progn
	      (setq cbox-editing-existing t)
	      (cbox-extract-comment)
	      (cbox-extract-text))
	  (setq cbox-editing-existing nil))
	(setq cbox-source-buffer (current-buffer))
	(split-window-right)
	(other-window 1)
	(setq cbox-comment-buffer (generate-new-buffer cbox-comment-buffer-name))
	(switch-to-buffer cbox-comment-buffer)
	(auto-fill-mode)
	(local-set-key "\C-c\C-c" 'cbox-trigger)
	(local-set-key "\C-c\C-k" 'cbox-abort)
	(when cbox-editing-existing
	    (cbox-insert-original-text)))
    (progn
      (setq cbox-comment-buffer-lines (split-string (buffer-string) "\n"))
      (setq cbox-comment-buffer-max-line-length (+ 2 (cbox-determine-max-line)))
      (switch-to-buffer cbox-source-buffer)
      (when cbox-editing-existing
	(cbox-delete-original-comment))
      (cbox-insert-comment)
      (kill-buffer cbox-comment-buffer)
      (delete-window)
      (goto-char cbox-return-marker))))

(defun cbox-abort ()
  "Invoked by C-c C-k, buffer local. Aborts inserting comment block."
  (interactive)
  (kill-buffer cbox-comment-buffer)
  (delete-window))

(defun cbox-determine-max-line ()
  "Sets cbox-comment-buffer-max-line-length to the longest line in cbox-comment-buffer"
  (let ((max 0))
    (dolist (el cbox-comment-buffer-lines max)
      (when (> (length el) max)
	(setq max (length el))))))

(defun cbox-insert-comment ()
  "Puts the cbox edit buffer contents into the original source buffer in
comment format"
  (unless cbox-editing-existing
    (goto-char cbox-insert-marker))
  (insert (concat "/*" (make-string cbox-comment-buffer-max-line-length ?=) "*\n"))
  (dolist (line cbox-comment-buffer-lines)
    (progn
      (when (eq line "") (setq line "\n"))
      (insert (concat " | " line (make-string (- (- cbox-comment-buffer-max-line-length 2) (length line)) ? ) " |\n"))))
  (insert (concat " *" (make-string cbox-comment-buffer-max-line-length ?=) "*/\n"))
  (set-marker cbox-return-marker (point))
  (switch-to-buffer cbox-comment-buffer))

(defun cbox-is-on-comment ()
  "Returns t if we are in a comment or nil if we are not. If we are
we set the cbox-edit-comment to the positions of start/finish so we can
later extract the text and redo the comment"
  (or (nth 4 (syntax-ppss))
      (memq (get-text-property (point) 'face)
	    '(font-lock-comment-face font-lock-comment-delimiter-face))))

(defun cbox-extract-comment ()
  "Scans area around point to copy the entire comment that the point is on,
it stores it in cbox-original-comment"
  (let (line (start -1) (end -1))
    (loop do
	  (setq line (thing-at-point 'line t))
	  (when (string-match-p (regexp-quote "/*") line)
	    (progn
	      (beginning-of-line)
	      (setq start (point))
	      (return)))
	  while (= 0 (forward-line -1)))
    (goto-char cbox-insert-marker)
    (loop do
	  (setq line (thing-at-point 'line t))
	  (when (string-match-p (regexp-quote "*/") line)
	    (progn
	      (end-of-line)
	      (setq end (point))
	      (return)))
	  while (= 0 (forward-line 1)))
    (goto-char cbox-insert-marker)
    (setq cbox-original-comment (buffer-substring-no-properties start end))
    (setq cbox-original-comment-points (list start end))))

(defun cbox-extract-text ()
  "Discards comment delimiter tokens and boxing tokens"
  (setq cbox-original-text "")
  (let ((lines (split-string cbox-original-comment "\n")))
    (setq lines (butlast (rest lines)))
    (dolist (line lines)
      (setq cbox-original-text
	    (concat cbox-original-text (apply (function string) (butlast (nthcdr 3 (string-to-list line)) 2)) "\n")))))

(defun cbox-insert-original-text ()
  "Inserts cbox-original-text into the newly created temporary buffer"
  (insert cbox-original-text))

(defun cbox-delete-original-comment ()
  "Deletes the original comment that was edited"
  (goto-char (first cbox-original-comment-points))
  (delete-char (- (second cbox-original-comment-points) (first cbox-original-comment-points))))

;;; cbox.el ends here
