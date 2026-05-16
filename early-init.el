;; Disable native compilation entirely — .eln files go stale when
;; packages update their macro environments (compat, etc.) and produce
;; inscrutable wrong-number-of-arguments errors.  The performance
;; benefit is negligible for interactive use.
(setq native-comp-jit-compilation nil
      native-comp-async-jobs-number 0)
