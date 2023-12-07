(pushnew (uiop:getcwd) ql:*local-project-directories*)
(ql:quickload :rcv)
(asdf:make :rcv)
;; (asdf:load-system :rcv)
