(asdf:defsystem :rcv
  :description "Ranked-choice voting in Common Lisp."
  :author "Nick Shannon <public.nshan651.com>"
  :homepage "https://placeholder.test"

  :license "AGPL 3.0"
  :version "0.1"

  :depends-on (:clingon :cl-csv)
  :serial t
  :components ((:module "src" :serial t
		:components ((:file "main")
			     ;; (:file "cli")
  ))))

(asdf:defsystem :rcv/bin
  :depends-on (:rcv)      ; This system needs the core HELLO system…
  :components ((:module "src"
		:components ((:file "main"))))) ; …and includes one
						; additional file.

;; (asdf:defsystem :rcv/test
;;   :description "Test suite for rcv."
;;   :author "Nick Shannon <public.nshan651.com>"

;;   :license "AGPL 3.0"
;;   :version "0.1"

;;   :depends-on (:rcv :fiveam)
;;   :serial t
;;   :components ((:module "test"
;;                 :serial t
;;                 :components ((:file "package")
;;                              (:file "tests"))))

;;   :perform (asdf:test-op (op system)
;;              (funcall (read-from-string "rcv.test:run-tests"))))
