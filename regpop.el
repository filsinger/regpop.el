;; regpop.el
;;   An emacs minor-mode to search for a regex and display
;;   the results in a popup.
;;
;; website: https://github.com/filsinger/regpop.el
;;
;; author: Jason Filsinger (https://github.com/filsinger)
;;
;; requirements:
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

(defvar regpop-display-containing-function nil
  "When value is t, the regpop will prefix the popup entry with the
name of the function containing the regex.")

(defvar regpop-display-line t
  "When value is t, the regpop will prefix the popup entry with the
line number containing the regex.")

(defun regpop-goto-line (line)
  "because goto-line isnt supposed to be used in lisp code."
  (interactive)
  (goto-char (point-min))
  (forward-line (1- line)))

(defun regpop-buffer-lines-to-list(&optional buffer)
  "Returns a list of strings for every line in a buffer."
  (let ((prevBuffer (current-buffer))
	(needBufferSwitch (and buffer (not (eq buffer (current-buffer))))) buffer-lines)
    (when needBufferSwitch (set-buffer buffer))
    (setq buffer-lines (split-string (buffer-substring-no-properties (buffer-end -1) (buffer-end 1)) "\n" nil))
    (when needBufferSwitch (set-buffer prevBuffer))
    buffer-lines))

(defun regpop-delete-surrounding-whitespace (string)
  "Deletes the leading and trailing whitespace in a string."
  (replace-regexp-in-string "^[[:space:]]?+" "" (replace-regexp-in-string "[[:space:]]?+$" "" string)))

(defun regpop-format-prefix (functionName lineNumber)
  "Format the function name based on the regpop options"
  (let ((retStr (when regpop-display-line (number-to-string lineNumber))))
    (when (and regpop-display-containing-function functionName) (setq retStr (if retStr (concat retStr ":" functionName) functionName)))
    (when (and retStr (> (length retStr) 0)) (concat "[" retStr "] "))))

(defun regpop-get-match-list (regexToMatch &optional index buffer)
  "Return a list of popup-items that match the regex."
  (let ((retList (list))
	(buffer-lines (regpop-buffer-lines-to-list (or buffer (current-buffer))))
	(currLineNumber 0)
	(initialLine (line-number-at-pos nil))
	(prevBuffer (current-buffer))
	(needBufferSwitch (and buffer (not (eq buffer (current-buffer))))))
    (when needBufferSwitch (set-buffer buffer))
    (loop for currLine in buffer-lines do
	  (setq currLineNumber (+ currLineNumber 1))
	  (when (and currLine (string-match regexToMatch currLine))
	    (let ((newEntry (or (match-string (if index index 1) currLine) (regpop-delete-surrounding-whitespace currLine))))
	      (when newEntry
		(progn
		  (regpop-goto-line currLineNumber)
		  (let ((currPrefix (regpop-format-prefix (if regpop-display-containing-function (which-function) nil) currLineNumber)))
		    (when currPrefix (setq newEntry (format "%s%s" currPrefix newEntry))))
		  (setq retList (append retList (list (popup-make-item newEntry :value currLineNumber)))))))))
    (when (> (length retList) 1) (regpop-goto-line initialLine))
    (when needBufferSwitch (set-buffer prevBuffer))
    (if (> (length retList) 0) retList nil)))

(defun* regpop* (regex &key index buffer point)
  "Display a popup for all instinces of a regex in a buffer."
  (let ((regexList (regpop-get-match-list regex index buffer)) tempLine)
    (when regexList
      (setq tempLine (if (> (length regexList) 1)
			 (popup-menu* regexList :isearch regpop-isearch :point point)
		       (car (popup-item-value regexList))))
      (when tempLine
	(progn
	  (when buffer (set-buffer buffer))
	  (regpop-goto-line tempLine))))))

;;;###autoload
(defun regpop (regex &optional index buffer)
  "Display a popup for all instinces of a regex in a buffer."
  (interactive (list (read-from-minibuffer "Regex: ")
		     (string-to-number (read-from-minibuffer "Regex group index: " nil nil nil nil "0"))))
  (regpop* regex :index index :buffer buffer))

;;;###autoload
(defun regpop-todo ()
  "popup all todos in the current buffer."
  (interactive)
  (regpop* regpop-todo :index 1))

;;;###autoload
(defun regpop-stub ()
  "popup all stubs in the current buffer."
  (interactive)
  (regpop* regpop-stub :index 1))

;;;###autoload
(defun regpop-assert ()
  "popup all stubs in the current buffer."
  (interactive)
  (regpop* regpop-assert :index 1))

(provide 'regpop)
