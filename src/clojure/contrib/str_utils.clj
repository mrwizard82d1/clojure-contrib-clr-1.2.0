;;; str_utils.clj -- string utilities for Clojure

;; by Stuart Sierra <mail@stuartsierra.com>
;; April 8, 2008

;; Copyright (c) Stuart Sierra, 2008. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

;; DEPRECATED in 1.2: Promoted to clojure.java.string. Note that
;; many function names and semantics have changed

(ns 
  ^{:author "Stuart Sierra",
    :deprecated "1.2"
    :doc "String utilities for Clojure"}
  clojure.contrib.str-utils
  (:require [clojure.contrib.string :as ccs])
  (:import (System.Text.RegularExpressions Regex)))

(defn re-split
  "Splits the string on instances of 'pattern'.  Returns a sequence of
  strings.  Optional 'limit' argument is the maximum number of
  splits.  Like Perl's 'split'."
  ([^Regex pattern string] (seq (.Split pattern string)))
  ([^Regex pattern string limit] (seq (.Split pattern string limit))))

(defn re-partition
  "Splits the string into a lazy sequence of substrings, alternating
  between substrings that match the patthern and the substrings
  between the matches.  The sequence always starts with the substring
  before the first match, or an empty string if the beginning of the
  string matches.

  For example: (re-partition #\"[a-z]+\" \"abc123def\")

  Returns: (\"\" \"abc\" \"123\" \"def\")"
  [^Regex re string]
  (ccs/partition re string))

(defn re-gsub 
  "Replaces all instances of 'pattern' in 'string' with
  'replacement'.  Like Ruby's 'String#gsub'.
  
  If (ifn? replacment) is true, the replacement is called with the
  match.
  "
  [^Regex regex replacement ^String string]
  (if (ifn? replacement)
    (let [parts (vec (re-partition regex string))]
      (apply str
             (reduce (fn [parts match-idx]
                       (update-in parts [match-idx] replacement))
                     parts (range 1 (count parts) 2))))
    (.Replace regex string replacement)))

(defn re-sub
  "Replaces the first instance of 'pattern' in 'string' with
  'replacement'.  Like Ruby's 'String#sub'.
  
  If (ifn? replacement) is true, the replacement is called with
  the match.
  "
  [^Regex regex replacement ^String string]
  (if (ifn? replacement)
    (let [m (re-matcher regex string)]
      (if (.find m)
        (str (.Substring string 0 (.start m))
             (replacement (re-groups m))
             (.Substring string (.end m)))
        string))
    (.Replace regex string replacement 1)))


(defn str-join
  "Returns a string of all elements in 'sequence', separated by
  'separator'.  Like Perl's 'join'."
  [separator sequence]
  (apply str (interpose separator sequence)))


(defn chop
  "Removes the last character of string."
  [s]
  (subs s 0 (dec (count s))))

(defn chomp
  "Removes all trailing newline \\n or return \\r characters from
  string.  Note: String.Trim() is similar and faster."
  [s]
  (re-sub #"[\r\n]+$" "" s))
