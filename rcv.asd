(defsystem "rcv"
  :author "nshan651 <public.nshan651.com>"
  :license "AGPL 3.0"
  :version "0.1"
  :homepage "https://github.com/nshan651/rcv"
  :bug-tracker "https://github.com/nshan651/rcv/issues"
  :source-control (:git "git@github.com/nshan651/rcv.git")
  :description "Ranked-choice voting command-line tool."
  :depends-on ("clingon"
	       "cl-csv"
	       "uiop")
  ;; Needed for pre-configuring default cli options.
  :defsystem-depends-on (:clingon)
  :components ((:module "src"
		:serial t
		:components (
			     ;; (:file "main")
			     (:file "stdin")
			     (:file "tournament")
			     (:file "rcv")
			     )))
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "rcv/tests")))

  :build-operation "program-op"
  :build-pathname "bin/rcv"
  :entry-point "rcv::main"
)

(defsystem "rcv/tests"
  :description "Test suite for rcv."
  :author "Nick Shannon <public.nshan651.com>"

  :license "AGPL 3.0"
  :version "0.1"

  :depends-on ("rcv"
	       "fiveam")
  :components ((:module "t"
                :serial t
                :components ((:file "main")
			     )
		))
  ;; :perform (test-op (op c) (symbol-call :rove :run c)))
)

;;; Customize default options for clingon.
;;; Add -h and -v as shorthands by updating default options.
(setf clingon:*default-options*
      (list
       (clingon:make-option :flag
			    :description "Display usage information and exit."
			    :long-name "help"
			    :short-name #\h
			    :key :clingon.help.flag)
       (clingon:make-option :flag
			    :description "Display version and exit."
			    :long-name "version"
			    :short-name #\v
			    :key :clingon.version.flag)
       clingon:*default-bash-completions-flag*))

;;; Enable SBCL core compression to greatly reduce binary size.
#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c)
		   :executable t
		   :compression t))
