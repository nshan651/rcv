(asdf:defsystem :rcv
  :description "Ranked-choice voting in Common Lisp."
  :author "nshan651 <public.nshan651.com>"
  :homepage "https://placeholder.test"

  :license "AGPL 3.0"
  :version "0.1"

  :depends-on (:clingon :cl-csv :uiop)
  :serial t
  :components ((:module "src" :serial t
		:components (
			     (:file "rcv"))))
			     ;; (:file "main")
			     ;; (:file "cli")
  :build-operation "program-op"
  :build-pathname "rcv"
  :entry-point "rcv::main"
)

;; (asdf:defsystem :rcv/bin
;;   :depends-on (:rcv)
;;   :components ((:module "src"
;; 		:components ((:file "main")))))

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
