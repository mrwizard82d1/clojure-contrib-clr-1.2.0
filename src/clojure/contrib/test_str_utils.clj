;; Defines unit tests for clojure.contrib.str-utils.
;;


(ns clojure.contrib.test-str-utils
  (:use clojure.contrib.str-utils
	clojure.test))


(declare test-string all-chars all-digits)


(deftest test-re-split
  (is (= ["abc" "def"] (re-split all-digits test-string)))
  (is (= [test-string] (re-split all-digits test-string 1)))
  (is (= ["abc" "def"] (re-split all-digits test-string 2)))
  (is (= ["abc" "def"] (re-split all-digits test-string 3)))
  (is (= ["abc" "def"] (re-split all-digits test-string 0)))
  (is (= [test-string] (re-split #"g" test-string))))


(deftest test-re-partition
  (is (= (list "" "abc" "123" "def")
         (re-partition all-chars test-string))))


(deftest test-re-gsub
  (is (= "abc456def" (re-gsub all-digits "456" test-string)))
  (is (= "ghi123ghi" (re-gsub all-chars "ghi" test-string)))
  (is (= (.toUpperCase test-string)
	 (re-gsub all-chars #(.toUpperCase %) test-string)))
  (is (= "abc81012def" (re-gsub #"\d"
				#(str (* 2 (- (int (.charAt % 0))
					      (int \0))))
				"abc456def"))))


(deftest test-re-sub
  (is (= "abc456def" (re-sub all-digits "456" test-string)))
  (is (= "ghi123def" (re-sub all-chars "ghi" test-string)))
  (is (= "ABC123def"
	 (re-sub all-chars #(.toUpperCase %) test-string)))
  (is (= "abc81012def" (re-gsub #"\d"
				#(str (* 2 (- (int (.charAt % 0))
					      (int \0))))
				"abc456def"))))


(deftest test-str-join
  (is (= test-string (str-join "123" ["abc" "def"]))))


(deftest test-chop
  (is (= (.substring test-string 0 (dec (.length test-string)))
	 (chop test-string))))


(deftest test-chomp
  (is (= test-string (chomp (str test-string \n))))
  (is (= test-string (chomp (str test-string "\r\n")))))


(defn fixture-fn [f]
  (binding [test-string "abc123def"
	    all-chars #"[a-z]+"
	    all-digits #"\d+"]
    (f)))


(use-fixtures :each fixture-fn)


(run-tests)