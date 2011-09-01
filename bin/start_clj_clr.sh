#! bash

# Starts Clojure CLR with command line arguments.



pushd ~/professional/projects/clj-contrib-clr12/src
~/professional/software/languages/clojure/clojure-clr-1.2.0/Clojure.Main.exe "$@"
popd

