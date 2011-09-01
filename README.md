A Port of clojure.contrib (1.2) to the CLR
==========================================

I like Clojure and want to experiment with it. Unfortunately, at least
for me, the primary platform (for now) of Clojure is the JVM. My
primary platform is the CLR.

Many articles and books refer to modules in clojure.contrib. These
useful modules are not accessible from the CLR. If these modules are
pure Clojure code, they should just work on the CLR. However, if these
modules refer to Java, for example, by invoking Java String members,
they will not work on the CLR.

More specifically, I wanted to use the functions available in
clojure.contrib.command-line. This module defined a private function:

	(defn- rmv-q
        "Remove ?"
        [^String s]
        (if (.endsWith s "?")
            (.substring s 0 (dec (count s)))
            s))
			
This module relies on the Java member functions `endsWith` and
`substtring`. Although these members port fairly easily to the CLR,
the code *does not* work "out-of-the-box."

At one time, I implemented data sets that contained historical time
