;;
;; "Hello, world!" smart contract/module
;;
;; To try it out, click "Load into REPL" and type into the repl:
;; (hello-world.set-message "universe")
;; (hello-world.greet)
;;
;; Check lines 21 and 34 to play with Formal Verification
;;

;; Define the module.
(module hello-world MODULE_ADMIN
  "A smart contract to greet the world."

  ; no-op module admin for example purposes.
  ; in a real contract this could enforce a keyset, or
  ; tally votes, etc.
  (defcap MODULE_ADMIN () true)

  (defschema message-schema
    @doc "Message schema"
    @model [(invariant (!= msg ""))]

    msg:string)

  (deftable
    message:{message-schema})

  (defun set-message
    (
      m:string
    )
    "Set the message that will be used next"
    ; uncomment the following to make the model happy!
    ; (enforce (!= m "") "set-message: must not be empty")
    (write message "0" {"msg": m})
  )

  (defun greet ()
    "Do the hello-world dance"
    (with-default-read message "0" { "msg": "" } { "msg":= msg }
      (format "Hello {}!" [msg])))
)

(create-table message)

(set-message "world")
(greet)
