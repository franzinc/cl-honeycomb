;; This file is Franz Inc specific.
;; 
;; For others, this module can be built by simply compiling and
;; loading cl-honeycomb.cl
;;
(defpackage #:cl-honeycomb-system
  (:use #:common-lisp #:asdf #:asdf-extensions))

(defsystem cl-honeycomb
    :class franz-system
    :serial t
    :components
    ((:file "cl-honeycomb")))
