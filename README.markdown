# regpop.el
An [emacs](http://www.gnu.org/software/emacs/) minor-mode to search for a regex and display the results in a popup.

## requirements
- [popup.el](https://github.com/m2ym/popup-el)

## installation
```lisp
(add-to-list 'load-path <path_to_regpop>)
(require 'regpop)
```

## configuration
### options
* `regpop-isearch` -- When set to `t` _(default)_, the popup will provide isearch functionality.  Set to `nil` to disable isearch.
* `regpop-display-containing-function` -- When set to `nil` _(default)_, each entry will be prefixed with the function that contains the result.  Set to `t` to enable the function display (requires which-function to be enabled).
* `regpop-display-line` -- When set to `t` _(default)_, each entry will be prefixed with the line that contains the regex.  Set to `nil` to disable the line display.

#### example
```lisp
(setq regpop-isearch nil)                      ; disable isearch in the popup
(setq regpop-display-containing-function nil)  ; disable function information
(setq regpop-display-line nil)                 ; disable line number
```

## usage
`M-x regpop` -- user will be prompted for a regex.

## functions
### regpop
Search for a user provided regex and display the results in a popup.  Selecting an entry will jump to the line containing the regex.

#### arguments:
* `regex` -- The regular expression to search for
** if the regex contains a group, the first group in the regex will be displayed in the popup.  This provides a very basic filtering out of the text displayed in the popup.
* `index` -- *(optional)* The regex group index to display in the popup. (default is 1)
* `buffer` -- *(optional)* The buffer to perform the search on.
** if no buffer is provided, the current buffer will be used.

#### example usage
Display a list of all TODOs in the current buffer.
```lisp
(regpop "[tT][oO][dD][oO].+?:?[[:space:]]*\\(.?+\\)[[:space:]]?+\\*?/?$")
```

## credits
* Author: [Jason Filsinger](http://filsinger.me)
