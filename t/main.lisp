(defpackage #:rcv/t/main
  (:use #:cl
	#:fiveam))
(in-package #:rcv/t/main)

;; (deftest example-test
;;   (ok (= 1 1)))

;; (deftest testing-length
;;   (testing "An example test to get this thing working."
;;     (ok (= (length #(1 2 3)) 3))))

;; (deftest another-test
;;   (testing "Another example."
;;     (ok (stringp "Am I a string?"))))

;; (deftest test-test
;;   (testing "Foo."
;;     (ok (stringp 5))))

;; (rove:run-suite (make-symbol (package-name *package*)))

(def-suite my-system
  :description "Test my system")
(in-suite my-system)

(fiveam:test sum-1
  (fiveam:is (= 3 (+ 1 2))))

;; We'll also add a failing test case
(fiveam:test sum2
  (fiveam:is (= 4 (+ 1 2))))

(fiveam:run! 'my-system)

(def-suite our-system
  :description "Test our system")
(in-suite our-system)

(fiveam:test is-string
  (fiveam:is (stringp "This is a string...")))

(fiveam:run! 'our-system)
;; (fiveam:run!)
