;;
;; Pact Example: "payments", a simple cash ledger.
;;

(namespace "free")

(module payments MODULE_ADMIN

  ; no-op module admin for example purposes.
  ; in a real contract this could enforce a keyset, or
  ; tally votes, etc.
  (defcap MODULE_ADMIN () true)

  ; account admin capability, also a noop for example purposes.
  (defcap ACCOUNT_ADMIN () true)

  ; user debit capability
  (defcap USER_DEBIT (user-id)
    "enforces row guard to allow debiting operations"
    (with-read accounts-table user-id { "guard":= guard }
      (enforce-guard guard)))

  ; define table schema
  (defschema accounts
    balance:decimal
    guard:guard)

  ; define table
  (deftable accounts-table:{accounts})

  (defun create-account (id initial-balance keyset)
    "Create a new account for ID with INITIAL-BALANCE funds, must be administrator."
    (with-capability (ACCOUNT_ADMIN)
      (enforce (>= initial-balance 0.0) "Initial balances must be >= 0.")
      (insert accounts-table id
        { "balance": initial-balance,
          "guard": keyset })))

  (defun get-balance (id)
    "Read account balance."
    (at "balance" (read accounts-table id)))

  (defun pay (from to amount)
    "Make a payment debiting FROM and crediting TO for AMOUNT."
    (with-capability (USER_DEBIT from)
      (with-read accounts-table from { "balance":= from-bal }
        (with-read accounts-table to { "balance":= to-bal }
          (enforce (> amount 0.0) "Negative Transaction Amount")
          (enforce (>= from-bal amount) "Insufficient Funds")
          (update accounts-table from
                  { "balance": (- from-bal amount) })
          (update accounts-table to
                  { "balance": (+ to-bal amount) })
          (format "{} paid {} {}" [from to amount])))))

)

;define table
(create-table accounts-table)

;;;; create accounts
; (env-data { "sarah-keyset": ["sarah"], "james-keyset": ["james"] })
; (use payments)
; (create-account "Sarah" 100.25 (read-keyset "sarah-keyset"))
; (create-account "James" 250.0 (read-keyset "james-keyset"))


;;;; simulate SARAH keyset.
; (env-keys ["sarah"])

;;;; show failure trying to debit James with Sarah's key
; (pay "James" "Sarah" 25.0)

;;;; success Sarah paying James with Sarah's key
; (pay "Sarah" "James" 25.0)
; (format "Sarah's balance is {}" [(get-balance "Sarah")])
; (format "James's balance is {}" [(get-balance "James")])
