# regpop.el
An [emacs](http://www.gnu.org/software/emacs/) minor-mode to search for a regex and display the results in a popup.

## requirements
- [popup.el](https://github.com/m2ym/popup-el) 

## installation
```lisp
(add-to-list 'load-path <path_to_regpop>)
(require 'regpop)
```
## usage
```
M-x regpop
```

## functions
### regpop
#### arguments:
* `regex` -- The regular expression to search for
** if the regex contains a group, the first group in the regex will be displayed in the popup.  This provides a very basic filtering out of the text displayed in the popup.
* `buffer` -- *(optional)* The buffer to perform the search on.
** if no buffer is provided, the current buffer will be used.
#### example usage
Display a list of all TODOS in the current buffer.
```lisp
(regpop "[tT][oO][dD][oO].+?:?[[:space:]]*\\(.?+\\)[[:space:]]?+\\*?/?$")
```

## credits
* Author: [Jason Filsinger](http://filsinger.me)

