# Emacs Notes

## Debugging

I seem to remember knowing this a long time ago.

Use `edebug` (not the classic elisp `debug-on-entry`).

You need to instrument the function first by doing `M-x
edebug-eval-top-level-form`.  Then run the function.

Edebug commands during execution:

```
SPC or n - Step to next expression
c - Continue execution
g - Go (run without stopping)
f - Forward one sexp
h - Continue to here (where cursor is)
b - Set breakpoint
u - Unset breakpoint
q - Quit debugging
e - Evaluate expression
? - Show help
I - instrument a function before you call it. Put cursor on open paren of the function call.
```

More info in [Edebug](https://emacsdocs.org/docs/elisp/Edebug).
