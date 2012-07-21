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

(defcustom regpop-todo "[tT][oO][dD][oO].+?:?[[:space:]]*\\(.?+\\)[[:space:]]?+\\*?/?$"
  "Regex filter used by regpop for filtering TODO items."
  :type 'regexp
  :group 'regpop)

(defcustom regpop-stub "[sS][tT][uU][bB].+?:?[[:space:]]*\\(.?+\\)[[:space:]]?+\\*?/?$"
  "Regex filter used by regpop for filtering STUB items."
  :type 'regexp
  :group 'regpop)

(defcustom regpop-assert "assert([[:space:]]?+\\(.?+\\)[[:space:]]?+)"
  "Regex filter used by regpop for filtering assert items."
  :type 'regexp
  :group 'regpop)

(defcustom regpop-isearch t
  "When set, regpop will use isearch with the popup"
  :type 'boolean
  :group 'regpop)

(defcustom regpop-display-containing-function nil
  "When value is t, the regpop will prefix the popup entry with the
name of the function containing the regex."
  :type 'boolean
  :group 'regpop)

(defcustom regpop-display-line t
  "When value is t, the regpop will prefix the popup entry with the
line number containing the regex."
  :type 'boolean
  :group 'regpop)

(defcustom regpop-push-mark-on-jump t
  "When value is t, the regpop will push a new mark before jumping to the matching entry."
  :type 'boolean
  :group 'regpop)


(defun regpop-delete-surrounding-whitespace (string)
  "Deletes the leading and trailing whitespace in a string."
  (replace-regexp-in-string "^[[:space:]]?+" "" (replace-regexp-in-string "[[:space:]]?+$" "" string)))

(defun regpop-make-popup-item (function start text)
  "Make a popup menu item for regpop."
  (let ((item-function (if regpop-display-containing-function function nil))
		(item-line (if regpop-display-line (number-to-string (line-number-at-pos start) ) nil)))
	(let ((item-prefix
		   (or (and (and (and item-function (stringp item-function)) item-line)
					(concat item-function ":" item-line)) (or (when (boundp item-function)item-function) (when item-line item-line)))))
	  (popup-make-item
	   (concat (or (when item-prefix (concat "[" item-prefix "] ")) "") (regpop-delete-surrounding-whitespace text))
	   :value start
	   ))))


(defun regpop-get-match-list (regex &optional subexp buffer)
  "Return a list of popup-items that match the regex."
  (let ((regexp-subexp (if subexp subexp 0))
		ret-list)
	(with-current-buffer (if buffer buffer (current-buffer))
	  (save-excursion
		(goto-char (point-max))
		(while (re-search-backward regex nil t)
		  (add-to-list 'ret-list (regpop-make-popup-item
								  (if (featurep 'which-func) (which-function) nil)
								  (match-beginning regexp-subexp)
								  (match-string-no-properties regexp-subexp))))))
	ret-list))


(defun* regpop* (regex &key subexp buffer point)
  "Display a popup for all instinces of a regex in a buffer."
  (let ((regpop-list (regpop-get-match-list regex subexp buffer)) item-point)
    (when regpop-list
      (let ((item-point (if (> (length regpop-list) 1)
							(popup-menu* regpop-list :isearch regpop-isearch :point point)
						  (popup-item-value (car regpop-list) ))))
		(when item-point
		  (progn
			(when buffer (set-buffer buffer))
			(when regpop-push-mark-on-jump (push-mark))
			(goto-char item-point)
			(recenter)))))))

;;;###autoload
(defun regpop (regex &optional subexp buffer)
  "Display a popup for all instinces of a regex in a buffer."
  (interactive (list (read-from-minibuffer "Regex: ")
					 (string-to-number (read-from-minibuffer "Regex group subexp: " nil nil nil nil "0"))))
  (regpop* regex :subexp subexp :buffer buffer))

;;;###autoload
(defun regpop-todo ()
  "popup all todos in the current buffer."
  (interactive)
  (regpop* regpop-todo :subexp 1))

;;;###autoload
(defun regpop-stub ()
  "popup all stubs in the current buffer."
  (interactive)
  (regpop* regpop-stub :subexp 1))

;;;###autoload
(defun regpop-assert ()
  "popup all stubs in the current buffer."
  (interactive)
  (regpop* regpop-assert :subexp 1))

(provide 'regpop)
