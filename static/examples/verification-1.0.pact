;;
;; A little example showing off the merits of formal
;; verification and why it pays off.
;;

(module verification MODULE_ADMIN
  @doc "Little example to show off the usefulness of formal verification."

  ; no-op module admin for example purposes.
  ; in a real contract this could enforce a keyset, or
  ; tally votes, etc.
  (defcap MODULE_ADMIN () true)

  (defun absBug:integer (num:integer)
     @doc "Ensure positive result"

     ;; This property fails
     ;; Would you have caught that with unit tests?
     @model [(property (>= result 0))]

     (if (= (- (* (- num 6) (+ num 11)) (* 42 num)) (* (* 64 7) 52270780833))
         (- 1)
         (abs num)
     )
   )
 )
