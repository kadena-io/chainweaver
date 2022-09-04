;;
;; "Hello, world!" smart contract/module
;;

(namespace "free")

;;---------------------------------
;;
;;  Create an 'admin-keyset' and add some key, for loading this contract!
;;
;;  Make sure the message is signed with this added key as well.
;;
;;  When deploying new contracts, ensure to use a *unique keyset name*
;;  and *unique module name* from any previously deployed contract
;;
;;
;;---------------------------------


;; Keysets cannot be created in code, thus we read them in
;; from the load message data.
(define-keyset "free.admin-keyset" (read-keyset "admin-keyset"))

;; Define the module. The module name must be unique
(module hello-world GOV
  "A smart contract to greet the world."

  ;; Define module governance function
  (defcap GOV ()
    (enforce-keyset "free.admin-keyset"))

  (defun hello (name:string)
    "Do the hello-world dance"
    (format "Hello {}!" [name]))
)

;; and say hello!
(hello "world")
