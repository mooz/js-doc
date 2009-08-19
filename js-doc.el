;;; js-doc.el --- Insert js-doc style comment easily

;; Author: mooz <stillpedant@gmail.com>
;; Version: 0.0.1
;; Keywords: document, comment
;; X-URL: http://www.d.hatena.ne.jp/mooz/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Custom:
(defgroup js-doc nil
  "Insert js-doc style comment easily."
  :group 'comment
  :prefix "js-doc")

;; Variables
(defcustom js-doc-mail-address ""
  "Author's E-mail address."
  :group 'js-doc)

(defcustom js-doc-author ""
  "Author of the source code."
  :group 'js-doc)

(defcustom js-doc-url ""
  "Author's Home page address."
  :group 'js-doc)

;;; Lines
;; special string format
;; %F => file name
;; %a => author name
;; %d => current date
;; %p => parameter name
;; %f => function name

(defcustom js-doc-file-line " * @file %F\n"
  "file name."
  :group 'js-doc)

(defcustom js-doc-author-line " * @author %a\n"
  "author name."
  :group 'js-doc)

(defcustom js-doc-date-line " * @date %d\n"
  "time of the comment inserted."
  :group 'js-doc)

(defcustom js-doc-brief-line " * @brief\n"
  "brief description of the source code."
  :group 'js-doc)

(defcustom js-doc-top-line "/**\n"
  "top line of the js-doc style comment."
  :group 'js-doc)

(defcustom js-doc-description-line" * \n"
  "description line."
  :group 'js-doc)

(defcustom js-doc-parameter-line " * @param {} %p\n"
  "parameter line.
 %p will be replaced with the parameter name."
  :group 'js-doc)

(defcustom js-doc-return-line " * @return\n"
  "return line."
  :group 'js-doc)

(defcustom js-doc-bottom-line " */\n"
  "bottom line."
  :group 'js-doc)

(defcustom js-doc-return-regexp "return "
  "return"
  :group 'js-doc)

;;; Variables
(defvar js-doc-format-string-alist
  '(
    ("%F" . (buffer-name))
    ("%P" . (buffer-file-name))
    ("%a" . js-doc-author)
    ("%d" . (current-time-string))
    ("%p" . js-doc-current-parameter-name)
    ("%f" . js-doc-current-function-name)
    ))

;; %F => file name
;; %P => file path
;; %a => author name
;; %d => current date
;; %p => parameter name
;; %f => function name

(defun js-doc-format-string (arg)
  "format string"
  (let ((rlist js-doc-format-string-alist)
        (case-fold-search nil))
    (while rlist
      (while (string-match (caar rlist) arg)
        (setq arg
              (replace-match
               (eval (cdar rlist)) t nil arg)))
      (setq rlist (cdr rlist))
      )
    )
  arg)

;;; Main codes:

(defun js-doc-tail (list)
  (if (cdr list)
      (js-doc-tail (cdr list))
    (car list))
  )

(defun js-doc-pick-symbol-name (str)
  "Pick up symbol-name from str"
  (js-doc-tail (delete "" (split-string str "[^a-zA-Z0-9_$]")))
  )

(defun js-doc-insert-file-doc ()
  "Insert specified-style comment top of the file"
  (interactive)
  (goto-char 1)
  (insert js-doc-top-line)
  (dolist (line-format (list js-doc-file-line
                             js-doc-date-line
                             js-doc-brief-line
                             js-doc-author-line
                             js-doc-bottom-line))
    (insert (js-doc-format-string line-format))
    )
  (insert "\n")
  )

(defun js-doc-has-return-p (begin end)
  (save-excursion
    ;; (insert (concat "{\n"
    ;;                 (buffer-substring-no-properties begin end)
    ;;                 "\n}\n"))
    (goto-char begin)
    (if (re-search-forward js-doc-return-regexp end t)
        t
      nil)
    )
  )

(defun js-doc-insert-function-doc ()
  "Insert specified-style comment top of the function"
  (interactive)
  (beginning-of-defun)
  ;; Parse function info
  (let ((params '())
	(head-of-func (point))
	from
	to
        begin
	end
        has-return)
    (save-excursion
      (setq from
            (search-forward "(" nil t))
      (setq to
            (1- (search-forward ")" nil t)))
      ;; Now we got string between ()
      (when (> to from)
        (dolist (param-block
                 (split-string (buffer-substring-no-properties from to) ","))
          (add-to-list 'params (js-doc-pick-symbol-name param-block) t)
          )
        )
      (setq begin (search-forward "{" nil t))
      (setq end (scan-lists (1- begin) 1 0))
      (setq has-return
            (js-doc-has-return-p begin end))
      ;; (insert (buffer-substring-no-properties begin end))
      )
    (beginning-of-line)
    (setq from (point))                 ; for indentation
    (insert js-doc-top-line)
    ;; Insert description line
    (insert js-doc-description-line)
    ;; Insert parameter lines
    (dolist (param params)
      (setq js-doc-current-parameter-name param)
      (insert (js-doc-format-string js-doc-parameter-line))
      )
    ;; Insert return value line
    (when has-return
      (insert js-doc-return-line))
    (insert js-doc-bottom-line)
    ;; Indent
    (indent-region from (point))
    )
  )

;; (defun js-doc-insert ()
;;   "Check if the current point is in the function block;
;; if in the function block, call insert-function-doc;
;; otherwise, call insert-file-doc."
;;   (interactive)
;;   (let ((old-point (point))
;; 	(new-point nil))
;;     (beginning-of-defun)
;;     (setq new-point (point))
;;     (end-of-defun)
;;     (if (or (< (point) old-point)
;; 	    (= 1 new-point))
;; 	(js-doc-insert-file-doc)
;;       (js-doc-insert-function-doc)))
;;   )

(provide 'js-doc)
