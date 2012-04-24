;; regpop.el
;;   Uses popup.el to display a list of regex matches 
;;   which upon selection will jump to the line containing
;;   the regex.
;;
;; author: Jason Filsinger (https://github.com/filsinger)
;;
;; reuirements:
;;   popup.el (https://github.com/m2ym/popup-el)
;;
;; bugs:
;;   - (which-function) is returning a function for matches that exist outside of functions
;;     but exist on lines below a function definition. [should maybe look into
;;     using CEDET or ThingAtPoint+
;;

(require 'popup)

(defvar regpop-todo "[tT][oO][dD][oO].+?:?[[:space:]]*\\(.?+\\)[[:space:]]?+\\*?/?$")
(defvar regpop-stub "[sS][tT][uU][bB].+?:?[[:space:]]*\\(.?+\\)[[:space:]]?+\\*?/?$")
(defvar regpop-assert "assert([[:space:]]?+\\(.?+\\)[[:space:]]?+)")

(defvar regpop-isearch t 
  "When set, regpop will use isearch with the popup")

(defvar regpop-display-containing-function t
 "When value is t, the regpop will prefix the popup entry with the 
name of the function containing the regex.")


(defun regpop-buffer-lines-to-list(&optional buffer)
  "Returns a list of strings for every line in a buffer."
  (interactive "P")
  (let ((prevBuffer (current-buffer))
	(needBufferSwitch (and buffer (not (eq buffer (current-buffer))))))
    (when needBufferSwitch (set-buffer buffer))
    (setq buffer-lines (split-string (buffer-substring-no-properties (buffer-end -1) (buffer-end 1)) "\n" nil))
    (when needBufferSwitch (set-buffer prevBuffer))
    buffer-lines))

(defun regpop-delete-surrounding-whitespace (string)
  "Deletes the leading and trailing whitespace in a string."
  (interactive "MString: ")
  (replace-regexp-in-string "^[[:space:]]?+" "" (replace-regexp-in-string "[[:space:]]?+$" "" string))
)

(defun regpop-get-match-list (regexToMatch &optional buffer)
  "Return a list of popup-items that match the regex."
  (interactive "MThe regex to match: ")
  (let ((retList (list))
	(buffer-lines (regpop-buffer-lines-to-list (or buffer (current-buffer))))
	(currLineNumber 0)
	(initialLine (line-number-at-pos nil))
	(prevBuffer (current-buffer))
	(needBufferSwitch (and buffer (not (eq buffer (current-buffer)))))
	)
    (when needBufferSwitch (set-buffer buffer))
    (loop for currLine in buffer-lines do
	  (setq currLineNumber (+ currLineNumber 1))
	  (when (and currLine (string-match regexToMatch currLine))
	      (let ((newEntry (or (match-string 1 currLine) (regpop-delete-surrounding-whitespace currLine))))
		(when newEntry
		    (progn
		      (goto-line currLineNumber)
		      (when regpop-display-containing-function (let ((entryFunc (which-function))) (when entryFunc (setq newEntry (format "[%s] %s" entryFunc newEntry)))))
		      (setq retList (append retList (list (popup-make-item newEntry :value currLineNumber)))
	))))))
    (when (> (length retList) 1) (goto-line initialLine))
    (when needBufferSwitch (set-buffer prevBuffer))
    (if (> (length retList) 0) retList nil)))


(defun regpop (regexToMatch &optional buffer)
  "Display a popup for all instinces of a regex in the current Buffer."
  (interactive "MThe regex to match: ")
  (let ((regexList (regpop-get-match-list regexToMatch buffer)))
    (when regexList
      (setq tempLine (if (> (length regexList) 1)
	(popup-menu* regexList :isearch regpop-isearch)
	(car (popup-item-value regexList))))
      (when tempLine
	(progn
	  (when buffer (set-buffer buffer))
	  (goto-line tempLine))))))

(defun regpop-todo ()
  "popup all todos in the current buffer."
  (interactive)
  (regpop regpop-todo)
)

(defun regpop-stub ()
  "popup all stubs in the current buffer."
  (interactive)
  (regpop regpop-stub)
)

(defun regpop-assert ()
  "popup all stubs in the current buffer."
  (interactive)
  (regpop regpop-assert)
)

(provide 'regpop)
