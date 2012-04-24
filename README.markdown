# regpop.el

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
Arguments:
* `regex` -- The regular expression to search for
** if the regex contains a group, the first group in the regex will be displayed in the popup.  This provides a very basic filtering out of the text displayed in the popup.
* `buffer` -- *(optional)* The buffer to perform the search on.
** if no buffer is provided, the current buffer will be used.

## credits
* Author: [Jason Filsinger](http://filsinger.me)

