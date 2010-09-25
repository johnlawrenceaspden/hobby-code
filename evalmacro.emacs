;; Keyboard Macro Editor.  Press C-c C-c to finish; press C-x k RET to cancel.
;; Original keys: M-C-f C-u C-x C-e M-C-b RET

Command: last-kbd-macro
Key: none

Macro:

M-C-f			;; forward-sexp
C-u C-x C-e		;; slime-eval-last-expression
M-C-b			;; backward-sexp
RET			;; reindent-then-newline-and-indent
