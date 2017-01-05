;;; cbox.el --- inserting block commands for explaining big chunks of code

;; Copyright © 2017 Samuel Domínguez Lorenzo

;; Filename: cbox.el
;; Maintainer: samueldominguez@protonmail.com
;; Keywords: comment, box

;; Created: 05 Jan 2017
;; URL: https://github.com/samueldominguez/cbox

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
;; 1.  User types keybinding 'C-c /'
;; 2.  We save current position (via marker?)
;; 3.  Split window vertically
;; 4.  Open temporary buffer
;; 5.  Set mode to fundamental and autofill
;; 6.  User types whatever
;; 7.  User types keybinding 'C-c /'
;; 8.  Parse all text in buffer to add necessary characters to box it in comments
;; 9.  Copy it to previously stored marker in original buffer
;; 10. Close temporary buffer
;; 11. Return to previous window state / just close the window (delete-window) (bound to C-x 0)

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

(defvar cbox-comment-buffer-lines nil
  "Contains all the text lines in a list which make up cbox-comment-buffer,
this is created when the user finishes typing their comment and re-invoke
cbox-trigger")

(defvar cbox-comment-buffer-max-line-length nil
  "Contains the number of characters of the longest line in cbox-comment-buffer")

(setq cbox-insert-marker (make-marker))

(defun cbox-trigger ()
  "Main function of cbox, it opens a buffer in text mode with auto-fill minor mode in a vertically
split window, allowing the user to type a comment and later when re-invoked insert the text in comment
form in the original buffer where cbox-trigger was initially invoked."
  (interactive)
  (if (eq (buffer-live-p cbox-comment-buffer) nil)
      (progn
	(set-marker cbox-insert-marker (point))
	(split-window-right)
	(other-window 1)
	(setq cbox-comment-buffer (generate-new-buffer cbox-comment-buffer-name))
	(switch-to-buffer cbox-comment-buffer)
	(auto-fill-mode))
    (progn
      (setq cbox-comment-buffer-lines (split-string (buffer-string) "\n"))
      (setq cbox-comment-buffer-max-line-length (cbox-determine-max-line))
      (kill-buffer cbox-comment-buffer)
      (delete-window))))

(defun cbox-determine-max-line ()
  "Sets cbox-comment-buffer-max-line-length to the longest line in cbox-comment-buffer"
  (let ((max 0))
    (dolist (el cbox-comment-buffer-lines max)
      (when (> (length el) max)
	(setq max (length el))))))
   
;; for debugging purposes:
(global-set-key (kbd "C-c /") 'cbox-trigger)
