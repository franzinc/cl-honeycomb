(defpackage :cl-honeycomb
  (:use :cl :excl)
  (:export
   ;; compilation switch:
   #:*include-honeycomb-code-p*
   ;; runtime switch:
   #:*post-to-honeycomb-p*
   ;; config:
   #:*global-api-key* #:*local-api-key*
   #:*global-dataset* #:*local-dataset*
   ;; annotations:
   #:with-span #:add-span-attributes
   ;; passing around dynamic state:
   #:with-saved-context #:with-restored-context
   #:with-saved-serialized-context #:with-restored-serialized-context))

(in-package :cl-honeycomb)

;; You have to create a honeycomb account and look up
;; the API key in the "Team Settings" page.
;;
;; Then either set this variable:
(defvar-nonbindable *global-api-key* nil)
;; or bind:
(defvar *local-api-key* nil)

;; The name of the "dataset" in which the spans should be posted.
(defvar-nonbindable *global-dataset* "cl-honeycomb")
(defvar *local-dataset* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (require :st-json)
  (require :aserve))

;; https://docs.honeycomb.io/working-with-your-data/core-feature-guides/tracing/send-trace-data/#manual-tracing
;;
;; Include the following key/value pairs in your log events:
;;
;; Field            Description
;; ---------------- ------------------------------------------------------------------------------------------
;; name             The specific call location (like a function or method name)
;; trace.span_id    A unique ID for each span

;; trace.parent_id  The ID of this span’s parent span, the call location the current span was called from
;; trace.trace_id   The ID of the trace this span belongs to
;; service_name     The name of the service that generated this span
;; duration_ms      How much time the span took, in milliseconds

(defstruct span
  span-id
  parent-id
  trace-id
  ;; event name and source
  name ;; e.g. function name
  service-name ;; e.g. server or component name
  ;; timing:
  start-time end-time
  ;; properties:
  key-values
  ;; span hierarchy:
  child-spans
  max-child-spans
  flush-to-server-p
  proxy-for-remote-span-p ;; true if parent span is remote, so this span is responsible to http post its span subtree
  api-key
  dataset
  sent-to-honeycomb-p)

(defmethod print-object ((span span) stream)
  ;; Don't print the entire tree
  (declare (ignore stream))
  (let ((*print-level* 2))
    (call-next-method)))

;; The ID of the current span within a trace.
;; If a subspan is created, the value of *current-span* will be stored
;; as its parent. After that *current-span* is bound to the subspan.
(defvar *current-span* nil)

;; Flag to suppress the generation of honeycomb code.
;; Set this flag before compiling code that uses WITH-SPAN and
;; ADD-SPAN-ATTRIBUTES.
;;
;; If true, the regular honeycomb functionality happens. At runtime
;; there will be a check for API key.
;; If nil, macros like WITH-SPAN act like PROGN (and the parameters
;; are not evaluated).
(defvar *include-honeycomb-code-p* t)

(define-symbol-macro %api-key% (or *local-api-key* *global-api-key*))
(define-symbol-macro %dataset% (or *local-dataset* *global-dataset*))

(defmacro honeycomb-enabled-p ()
  ;; If no API key is set, don't do the span work.
  ;; Dataset is checked in CALL-WITH-SPAN.
  '(not (null %api-key%)))

;;; Lexical state

(defmacro with-saved-context (() &body body)
  (if* *include-honeycomb-code-p*
     then (let ((g (gensym "saved-context-")))
            `(let ((,g *current-span*))
               ;; ,G is not dynamic-extent
               (macrolet ((with-restored-context (() &body inner-body)
                            `(let ((*current-span* ,',g))
                               ,@inner-body)))
                 ,@body)))
     else `(progn ,@body)))

;;; Serialized state, e.g. to send to other processes over HTTP

(defmacro with-saved-serialized-context ((str) &body body)
  (if* *include-honeycomb-code-p*
     then `(let ((,str (get-serialized-context)))
             ,@body)
     else `(let ((,str ""))
             ,@body)))

(defun get-serialized-context (&optional (span *current-span*))
  (or (when span ;; Be robust against having being called outside of WITH-SPAN
        (let ((api-key %api-key%)
              (trace-id (span-trace-id span))
              (span-id (span-span-id span))
              (max-child-spans (span-max-child-spans span)))
          (when (and %api-key% trace-id span-id)
            ;; Sanity check. The primary use case is to send this
            ;; over HTTP, so this does not have to be super optimized.
            (check-type api-key string)
            (check-type trace-id string)
            (check-type span-id string)
            (check-type max-child-spans (or null fixnum))
            (assert (and (plusp (length api-key)) (plusp (length trace-id)) (plusp (length span-id))))
            (with-output-to-string (s)
              (write `(:honeycomb :span-id ,span-id :trace-id ,trace-id :api-key ,api-key :max-child-spans ,max-child-spans)
                     :stream s)))))
      ""))

(defmacro with-restored-serialized-context ((str) &body body)
  ;; STR must be generated by WITH-SAVED-CONTEXT-STRING.
  (if* *include-honeycomb-code-p*
     then `(call-with-restored-serialized-context ,str (lambda () ,@body))
     else `(progn ,@body)))

(defun call-with-restored-serialized-context (string func)
  (declare (dynamic-extent func))
  (check-type string string)
  (if* (string= string "")
     then (funcall func)
     else (let* ((*read-eval* nil)
                 (state (read-from-string string)))
            (check-type state (cons (eql :honeycomb)))
            (destructuring-bind (&key span-id trace-id api-key max-child-spans)
                (cdr state)
              ;; Sanity check. The primary use case is to receive this
              ;; over HTTP, so this does not have to be super optimized,
              (check-type api-key string)
              (check-type trace-id string)
              (check-type span-id string)
              (assert (and (plusp (length api-key)) (plusp (length trace-id)) (plusp (length span-id))))
              (check-type max-child-spans (or null fixnum))
              (let* ((*local-api-key* api-key)
                     (*current-span* (make-span :trace-id trace-id
                                                :span-id span-id
                                                :max-child-spans max-child-spans
                                                ;; Ensure this is sent to Honeycomb after span ends;
                                                ;; the real root span is somewhere external and won't
                                                ;; know about this child span.
                                                :proxy-for-remote-span-p t
                                                :api-key api-key)))
                (funcall func))))))
;;;

(defmacro with-span ((component function &rest kv-args) &body body &environment env)
  (if* *include-honeycomb-code-p*
     then (setf kv-args (copy-list kv-args))
          (destructuring-bind (&key max-child-spans flush-to-server-p &allow-other-keys)
              kv-args
            (when max-child-spans
              (check-type max-child-spans (integer 0 #.most-positive-fixnum))
              (remf kv-args :max-child-spans))
            (when flush-to-server-p
              (remf kv-args :flush-to-server-p))
            `(call-with-span ,component
                             ,function
                             (list ,@(loop for x in kv-args
                                         collect (if* (stringp x)
                                                    then x
                                                  elseif (constantp x env)
                                                    then (prin1-to-string x)
                                                    else `(prin1-to-string ,x))))
                             ,(when body `(lambda () ,@body))
                             ,max-child-spans
                             ,flush-to-server-p))
     else `(progn ,@body)))

(defmacro add-span-attributes (&rest kv-args &environment env)
  (if* *include-honeycomb-code-p*
     then `(when (honeycomb-enabled-p)
             (setf (span-key-values (or *current-span*
                                        (error "ADD-SPAN-ATTRIBUTES must be called inside WITH-SPAN")))
               (list* ,@(loop for x in kv-args
                            collect (if* (stringp x)
                                       then x
                                     elseif (constantp x env)
                                       then (prin1-to-string x)
                                       else `(prin1-to-string ,x)))
                      (span-key-values *current-span*))))
     else ()))

(defun call-with-span (component function key-values func max-child-spans flush-to-server-p)
  (declare (optimize speed (safety 0)) ;; This function should be fast
           (dynamic-extent func))
  ;; FUNC is nil for no body
  (let ((parent *current-span*))
    (when parent
      ;; Check child limit
      (let ((max-childs (span-max-child-spans parent)))
        (if* (and max-childs (<= (the fixnum max-childs) 0))
           then ;; Child limit reached: don't create a new span
                (when func
                  (funcall (the function func)))
                (return-from call-with-span))))
    (multiple-value-bind (api-key dataset)
        (if* parent
           then (values (span-api-key parent) (span-dataset parent))
           else (let ((api-key %api-key%)
                      (dataset %dataset%))
                  (if* (or (null api-key) (null dataset))
                     then (when func
                            (funcall (the function func)))
                          (return-from call-with-span)
                     else (values api-key dataset))))
      (let* ((curr-span (make-span :parent-id (when parent
                                                (span-span-id parent))
                                   :trace-id (if* parent
                                                then (span-trace-id parent)
                                                else (generate-trace-id component function))
                                   :span-id (generate-span-id component function)
                                   :service-name component
                                   :name function
                                   :key-values key-values
                                   :max-child-spans max-child-spans
                                   :flush-to-server-p flush-to-server-p
                                   :api-key api-key
                                   :dataset dataset))
             (*current-span* curr-span)
             (local-root-p (or (null parent)
                               (span-proxy-for-remote-span-p parent))))
        (when parent
          (push-atomic curr-span (span-child-spans parent))
          (when (span-max-child-spans parent)
            (decf (span-max-child-spans parent))))
        (setf (span-start-time curr-span) (get-high-res-time))
        (unwind-protect
            (when func
              (funcall (the function func)))
          ;; Cleanup
          (when func
            (setf (span-end-time curr-span) (get-high-res-time)))
          (when (or local-root-p ;; There's no parent, so it's now or never
                    flush-to-server-p ;; User specified to flush now
                    ;; Inside WITH-RESTORED-CONTEXT, parent already sent
                    (and parent (span-sent-to-honeycomb-p parent)))
            (post-span-hierarchy-to-honeycomb curr-span)))))))

;; Assumption by GET-CURRENT-TIME-MILLIS
(assert (= internal-time-units-per-second 1000))

(defmacro get-current-time-millis ()
  `(get-internal-real-time))

(defvar-nonbindable *id-counter* 0)

(defun generate-trace-id (component function)
  (declare (ignore component function))
  ;; Include PID to ensure ids of parallel processes don't clash.
  ;; Include universal time so restarting the server does not reuse old trace ids.
  (format nil "T-p~a-t~a-i~a" (excl.osi:getpid) (get-universal-time) (incf-atomic *id-counter*)))

(defun generate-span-id (component function)
  (declare (ignore component function))
  ;; A span only needs to be unique within the trace.
  ;; A trace may be executeed on multiple processes.
  (format nil "S-p~a-i~a" (excl.osi:getpid) (incf-atomic *id-counter*)))

(defun get-high-res-time ()
  (multiple-value-bind (ut fsecs)
      (get-universal-hi-res-time)
    (cons ut fsecs)))

;;; Send traces using HTTP

(defvar-nonbindable *post-to-honeycomb-p* t
  "A debug switch to disable posting to Honeycomb.")

(defvar-nonbindable *post-span-queue* (make-instance 'mp:queue))

(defun post-process ()
  (loop for span = (mp:dequeue *post-span-queue*
                               :wait t
                               :whostate "Waiting for span to send to Honeycomb")
      do (do-post-span-hierarchy-to-honeycomb span)))

(defvar-nonbindable *post-process*
    (let ((proc (mp:process-run-function "cl-honeycomb::*post-process*" #'post-process)))
      (setf (mp::process-keeps-lisp-alive-p proc) nil)
      (setf (mp::process-interruptible-p proc) nil)
      proc))

(defun post-span-hierarchy-to-honeycomb (span)
  (when (not *post-to-honeycomb-p*)
    (return-from post-span-hierarchy-to-honeycomb :*post-to-honeycomb*-false))
  (check-type (span-dataset span) string)
  (check-type (span-api-key span) string)
  (mp:enqueue *post-span-queue* span))

(defun do-post-span-hierarchy-to-honeycomb (span)
  (let ((url (util.string:string+ "https://api.honeycomb.io/1/batch/" (span-dataset span)))
        (headers `(("X-Honeycomb-Team" . ,(span-api-key span))))
        (spans-todo (list span)))
    (loop while spans-todo
        do (let ((body (with-output-to-string (*standard-output*)
                         (write-string "[")
                         (let ((is-first t))
                           (loop while spans-todo
                               for count from 0 below 100
                               for span = (pop spans-todo)
                               when (not (span-sent-to-honeycomb-p span))
                               do ;; Mark sent, to avoid being blocked on errors
                                 (setf (span-sent-to-honeycomb-p span) t)
                                 (excl:if* is-first
                                    then (setf is-first nil)
                                    else (write-char #\,)
                                         (write-char #\Newline))
                                 (write-string "{\"time\":\"")
                                 (format-high-res-time (span-start-time span) t)
                                 (write-string "\", \"data\": ")
                                 (format-span-as-json span t)
                                 (write-string "}")
                                 (dolist (c (span-child-spans span))
                                   (push c spans-todo))))
                         (write-string "]"))))
             (multiple-value-bind (response-string status)
                 (net.aserve.client:do-http-request url
                   :method :post
                   :headers headers
                   :content-type "application/json"
                   :content body)
               (when (not (<= 200 status 299))
                 (warn "Posting to ~a failed with status ~d: ~a"
                       url status response-string)
                 (return-from do-post-span-hierarchy-to-honeycomb
                   (values nil status response-string)))))))
  t)

(defun format-span-as-json (span stream)
  (when (eq stream t)
    (setf stream *standard-output*))
  ;; Every trace span should have a name and service-name, but check anyway.
  (with-slots (name service-name span-id parent-id trace-id start-time end-time key-values)
      span
    (let ((alist `(("name" . ,(or name "(no name)"))
                   ("service_name" . ,(or service-name "(no service)"))
                   ("trace.span_id" . ,span-id)
                   ,@(when parent-id `(("trace.parent_id" . ,parent-id)))
                   ("trace.trace_id" . ,trace-id)
                   ,@(when end-time `(("duration_ms" . ,(high-res-time-duration start-time end-time))))))
          (extra-key-values (loop for (k v) on key-values by #'cddr
                                do (assert (stringp k))
                                   (assert (stringp v))
                                collect (cons k v))))
      ;; Keep the standard key/values at the front of the alist to help debugging.
      (setf alist (nconc alist extra-key-values))
      (st-json:write-json (st-json::make-jso :alist alist) stream))))

(defun format-high-res-time (hrt stream)
  ;; Returns e.g. "2019-04-10T04:03:22.810Z" for hrt (3763883002 . 810)
  (destructuring-bind (ut . fsecs) hrt
    (excl:locale-print-time ut :fmt "%Y-%m-%dT%T" :stream stream)
    (format stream ".~3,'0d" fsecs)
    (let ((timezone (- excl::*time-zone*)))
      (when (excl::sy-daylight-savings-in-effect (get-universal-time))
        (incf timezone))
      (if* (zerop timezone)
         then (format stream "Z")
         else (write-char (if (plusp timezone) #\+ #\-))
              (format stream "~2,'0d:00" (abs timezone))))))

(defun high-res-time-duration (start-hrt end-hrt)
  ;; Get the duration from START to END, both high-resolution time.
  ;; The return value is the duration in mulliseconds.
  (destructuring-bind (ut1 . fsecs1)
      start-hrt
    (destructuring-bind (ut2 . fsecs2)
        end-hrt
      (if* (or (< ut1 ut2)
               (and (= ut1 ut2)
                    (<= fsecs1 fsecs2)))
         then (+ (* (- ut2 ut1) 1000) ;; delta in seconds
                 (- fsecs2 fsecs1)) ;; delta in milliseconds
         else 0)))) ;; system time went backwards?

#|
Some tests that show the usage.
Assign *global-api-key*, or bind *local-api-key* outside fo the test function.
Visualize the results in honeycomb and check the parent is linked to the children.

(defun test-1 ()
  (let (context)
    (with-span ("parent" "test-1")
      (with-saved-serialized-context (str)
        (setf context str))
      (with-span ("child" "a")))
  (with-restored-serialized-context (context)
    (with-span ("child" "b")))))

(defun test-2 ()
  (let (f)
    (with-span ("parent" "test-2")
      (with-span ("child" "a"))
      (with-saved-context ()
        (setf f (lambda ()
                  (with-restored-context ()
                    (with-span ("child" "c")))))
        )
      (with-span ("child" "b")))
    (funcall f)))

(defun test-3 ()
  (with-span ("parent" "p" :key-1 "val-1" :key-2 222)
    (add-span-attributes :key-3 "val-3" :key-4 "val-4")
    (with-span ("child" "" :ch-key-1 "v1")
      (add-span-attributes :ch-key-2 "v2"))
    (add-span-attributes :key-5 "val-5")))

;; Try this one after (setf *global-dataset* nil)
;; binding *local-api-key* around it
(defun test-4 ()
  (let (f)
    (let ((*local-dataset* "cl-honeycomb"))
      (let ((ht (make-hash-table)))
        (with-span ("parent" "p" :ht-0 ht)
          (setf (gethash 1 ht) 1)
          (with-span ("child 1" "" :ht-1 ht)
            (with-saved-context ()
              (setf f (lambda ()
                        (with-restored-context ()
                          (with-span ("lambda inside child-1" "")))))))
          (setf (gethash 2 ht) 2)
          (with-span ("child 2" "" :ht-2 ht)))))
    (funcall f)))
|#
