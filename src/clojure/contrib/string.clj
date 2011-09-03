;;; string.clj -- functional string utilities for Clojure

;; by Stuart Sierra, http://stuartsierra.com/
;; January 26, 2010

;; Copyright (c) Stuart Sierra, 2010. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

;; Ported to the CLR by Larry Jones.

;; DEPRECATED in 1.2: Many functions have moved to clojure.string.

(ns ^{:author "Stuart Sierra"
       :doc "This is a library of string manipulation functions.  It
    is intented as a replacement for clojure.contrib.string.

    You cannot (use 'clojure.contrib.string) because it defines
    functions with the same names as functions in clojure.core.
    Instead, do (require '[clojure.contrib.string :as s]) 
    or something similar.

    Goals:
      1. Be functional
      2. Most significant argument LAST, to work with ->>
      3. At least O(n) performance for Strings of length n

    Some ideas are borrowed from
    http://github.com/francoisdevlin/devlinsf-clojure-utils/"}
 clojure.contrib.string
 (:refer-clojure :exclude (take replace drop butlast partition
                           contains? get repeat reverse partial))
 (:import (System.Text.RegularExpressions Regex
					  MatchEvaluator)))


(defmacro dochars 
  "bindings => [name string]

  Repeatedly executes body, with name bound to each character in
  string.  Does NOT handle Unicode supplementary characters (above
  U+FFFF)."
  [bindings & body]
  (assert (vector bindings))
  (assert (= 2 (count bindings)))
  ;; This seems to be the fastest way to iterate over characters.
  `(let [^String s# ~(second bindings)]
     (dotimes [i# (.Length s#)]
       (let [~(first bindings) (aget (.ToCharArray s#) i#)]
         ~@body))))


(defmacro docodepoints
  "bindings => [name string]

  Repeatedly executes body, with name bound to the integer code point
  of each Unicode character in the string.  Handles Unicode
  supplementary characters (above U+FFFF) correctly."
  [bindings & body]
  (assert (vector bindings))
  (assert (= 2 (count bindings)))
  (let [character (first bindings)
        string (second bindings)]
    `(let [^String s# ~string
           len# (.Length s#)]
       (loop [i# 0]
         (when (< i# len#)
           (let [~character (aget (.ToCharArray s#) i#)]
             (if (Char/IsHighSurrogate ~character)
               (let [~character (Char/ConvertToUtf32 s# i#)]
                 ~@body
                 (recur (+ 2 i#)))
               (let [~character (int ~character)]
                 ~@body
                 (recur (inc i#))))))))))

(defn codepoints
  "Returns a sequence of integer Unicode code points in s.  Handles
  Unicode supplementary characters (above U+FFFF) correctly."
  [^String s]
  (let [len (.Length s)
        f (fn thisfn [^String s i]
            (when (< i len)
              (let [c (aget (.ToCharArray s) i)]
                (if (Char/IsHighSurrogate c)
                  (cons (Char/ConvertToUtf32 s i) (thisfn s (+ 2 i)))
                  (cons (int c) (thisfn s (inc i)))))))]
    (lazy-seq (f s 0))))

(defn ^String escape
  "Returns a new String by applying cmap (a function or a map) to each
   character in s.  If cmap returns nil, the original character is
   added to the output unchanged."
   {:deprecated "1.2"}
  [cmap ^String s]
  (let [buffer (StringBuilder. (.Length s))]
    (dochars [c s]
      (if-let [r (cmap c)]
        (.Append buffer r)
        (.Append buffer c)))
    (.ToString buffer)))

(defn blank?
  "True if s is nil, empty, or contains only whitespace."
  {:deprecated "1.2"}
  [^String s]
  (every? (fn [^Char c] (Char/IsWhiteSpace c)) s))
  ;; (every? (fn [c] (Char/IsWhiteSpace c)) s))

(defn ^String take
  "Take first n characters from s, up to the length of s."
  [n ^String s]
  (if (< (count s) n)
    s
    (.Substring s 0 n)))

(defn ^String drop
  "Drops first n characters from s.  Returns an empty string if n is
  greater than the length of s."
  [n ^String s]
  (if (< (count s) n)
    ""
    (.Substring s n)))

(defn ^String butlast
  "Returns s without the last n characters.  Returns an empty string
  if n is greater than the length of s."
  [n ^String s]
  (if (< (count s) n)
    ""
    (.Substring s 0 (- (count s) n))))

(defn ^String tail
  "Returns the last n characters of s."
  [n ^String s]
  (if (< (count s) n)
    s
    (.Substring s (- (count s) n))))

(defn ^String repeat
  "Returns a new String containing s repeated n times."
  [n ^String s]
  (apply str (clojure.core/repeat n s)))

(defn ^String reverse
  "Returns s with its characters reversed."
  {:deprecated "1.2"}
  [^String s]
  (let [chars (.ToCharArray s)]
    (Array/Reverse chars)
    (String. chars)))

(defn replace-str
  "Replaces all instances of substring a with b in s."
  {:deprecated "1.2"}
  [^String a ^String b ^String s]
  (.Replace s a b))

(defn replace-char
  "Replaces all instances of character a with character b in s."
  {:deprecated "1.2"}
  ;; [^Char a ^Char b ^String s]
  [a b s]
  (.Replace s a b))

(defn replace-re
  "Replaces all matches of re with replacement in s."
  {:deprecated "1.2"}
  [re replacement ^String s]
  (.Replace re s replacement))

(defn replace-by
  "Replaces all matches of re in s with the result of 
  (f (re-groups the-match))."
  {:deprecated "1.2"}
  [re f ^String s]
  (.Replace re s (gen-delegate MatchEvaluator [m]
			       (f (str m)))))

(defn replace-first-str
  "Replace first occurance of substring a with b in s."
  {:deprecated "1.2"}
  [^String a ^String b ^String s]
  (.replaceFirst (re-matcher (Regex/Escape a) s) b))

(defn replace-first-re
  "Replace first match of re in s."
  {:deprecated "1.2"}
  [^Regex re ^String replacement ^String s]
  (.Replace re s replacement 1))

(defn replace-first-by
  "Replace first match of re in s with the result of
  (f (re-groups the-match))."
  {:deprecated "1.2"}
  [^Regex re f ^String s]
  (.Replace re s
	    (gen-delegate MatchEvaluator [m] (f (str m)))
	    1))

(defn partition
  "Splits the string into a lazy sequence of substrings, alternating
  between substrings that match the patthern and the substrings
  between the matches.  The sequence always starts with the substring
  before the first match, or an empty string if the beginning of the
  string matches.

  For example: (partition #\"[a-z]+\" \"abc123def\")
  returns: (\"\" \"abc\" \"123\" \"def\")"
  [^Regex re ^String s]
  (let [m (re-matcher re s)]
    ((fn step [prevend]
       (lazy-seq
        (if (.find m)
          (cons (.Substring s prevend (- (.start m) prevend))
                (cons (re-groups m)
                      (step (+ (.start m) (count (.group m))))))
          (when (< prevend (.Length s))
            (list (.Substring s prevend))))))
     0)))

(defn ^String join
  "Returns a string of all elements in coll, separated by
  separator.  Like Perl's join."
  {:deprecated "1.2"}
  [^String separator coll]
  (apply str (interpose separator coll)))

(defn ^String chop
  "Removes the last character of string, does nothing on a zero-length
  string."
  [^String s]
  (let [size (count s)]
    (if (zero? size)
      s
      (subs s 0 (dec (count s))))))

(defn ^String chomp
  "Removes all trailing newline \\n or return \\r characters from
  string.  Note: String.trim() is similar and faster.
  Deprecated in 1.2. Use clojure.string/trim-newline"
  {:deprecated "1.2"}
  [^String s]
  (replace-re #"[\r\n]+$" "" s))

(defn ^String swap-case
  "Changes upper case characters to lower case and vice-versa.
  Handles Unicode supplementary characters correctly.  Uses the
  locale-sensitive String.toUpperCase() and String.toLowerCase()
  methods."
  [^String s]
  (let [buffer (StringBuilder. (.Length s))]
    (docodepoints [c s]
      (if (Char/IsLower c)
        ;; Character.toUpperCase is not locale-sensitive, but
        ;; String.toUpperCase is; so we use a String.
        (.Append buffer (Char/ToUpper c))
        (.Append buffer (Char/ToLower c))))
    (.ToString buffer)))

(defn ^String capitalize
  "Converts first character of the string to upper-case, all other
  characters to lower-case."
  {:deprecated "1.2"}
  [^String s]
  (if (< (count s) 2)
    (.ToUpper s)
    (str (.ToUpper ^String (subs s 0 1))
         (.ToLower ^String (subs s 1)))))

(defn ^String ltrim
  "Removes whitespace from the left side of string.
   Deprecated in 1.2. Use clojure.string/triml."
  {:deprecated "1.2"}
  [^String s]
  (replace-re #"^\s+" "" s))

(defn ^String rtrim
  "Removes whitespace from the right side of string.
   Deprecated in 1.2. Use clojure.string/trimr."
  {:deprecated "1.2"}
  [^String s]
  (replace-re #"\s+$" "" s))

(defn split-lines
  "Splits s on \\n or \\r\\n."
  {:deprecated "1.2"}
  [^String s]
  (seq (.Split #"\r?\n" s)))

;; borrowed from compojure.string, by James Reeves, EPL 1.0
(defn ^String map-str
  "Apply f to each element of coll, concatenate all results into a
  String."
  [f coll]
  (apply str (map f coll)))

;; borrowed from compojure.string, by James Reeves, EPL 1.0
(defn grep
  "Filters elements of coll by a regular expression.  The String
  representation (with str) of each element is tested with re-find."
  [re coll]
  (filter (fn [x] (re-find re (str x))) coll))

(defn as-str
  "Like clojure.core/str, but if an argument is a keyword or symbol,
  its name will be used instead of its literal representation.

  Example:
     (str :foo :bar)     ;;=> \":foo:bar\"
     (as-str :foo :bar)  ;;=> \"foobar\" 

  Note that this does not apply to keywords or symbols nested within
  data structures; they will be rendered as with str.

  Example:
     (str {:foo :bar})     ;;=> \"{:foo :bar}\"
     (as-str {:foo :bar})  ;;=> \"{:foo :bar}\" "
  ([] "")
  ([x] (if (instance? clojure.lang.Named x)
         (name x)
         (str x)))
  ([x & ys]
     ((fn [^StringBuilder sb more]
        (if more
          (recur (. sb  (append (as-str (first more)))) (next more))
          (str sb)))
      (new StringBuilder ^String (as-str x)) ys)))


;;; WRAPPERS

;; The following functions are simple wrappers around java.lang.String
;; functions.  They are included here for completeness, and for use
;; when mapping over a collection of strings.

(defn ^String upper-case
  "Converts string to all upper-case."
  {:deprecated "1.2"}
  [^String s]
  (.ToUpper s))

(defn ^String lower-case
  "Converts string to all lower-case."
  {:deprecated "1.2"}
  [^String s]
  (.ToLower s))

(defn split
  "Splits string on a regular expression.  Optional argument limit is
  the maximum number of splits."
  {:deprecated "1.2"}
  ([^Regex re ^String s] (seq (.split re s)))
  ([^Regex re limit ^String s] (seq (.split re s limit))))

(defn ^String trim
  "Removes whitespace from both ends of string."
  {:deprecated "1.2"}
  [^String s]
  (.Trim s))

(defn ^String substring?
  "True if s contains the substring."
  [substring ^String s]
  (.Contains s substring))

(defn ^String get
  "Gets the i'th character in string."
  {:deprecated "1.2"}
  [^String s i]
  (aget (.ToCharArray s) i))

