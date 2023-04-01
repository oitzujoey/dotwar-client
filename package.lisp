(defpackage #:dotwar-client
  (:use #:cl)
  (:import-from #:com.inuoe.jzon
                #:parse
                #:stringify)
  (:import-from #:dex
                #:post)
  (:import-from #:alexandria
                #:iota)
  (:import-from #:sketch
                #:defsketch
                #:title
                #:width
                #:height
                #:+red+
                #:+green+
                #:+blue+
                #:+yellow+
                #:+magenta+
                #:+cyan+
                #:+orange+
                #:+white+
                #:+black+
                #:make-pen
                #:with-pen
                #:background
                #:line
                #:polyline
                #:circle)
  (:export #:track-target
           #:view))
