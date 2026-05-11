;; Disable JIT native compilation — MELPA ships .elc files compiled on
;; Emacs 31, which bake in runtime calls to compile-time macros
;; (incf, static-when, etc.) that don't exist as functions on Emacs 30.
;; The native compiler reproduces the same breakage when JIT-compiling.
(setq native-comp-jit-compilation nil)
