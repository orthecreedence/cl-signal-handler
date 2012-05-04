;;; This is a simple library for creating signal handlers for unix signals in
;;; Common Lisp. Its only dependency is CFFI, and it is only tested on *NIX
;;; systems. Chances are, windows won't work ='[.
;;;
;;; Any implmentation that supports CFFI and is running on a posix system
;;; should be able to use this library.
;;;
;;; Although I'm no expert in C and cannot recommend best practices for this
;;; library (sorry!!), I can point you to an excellent resource:
;;;
;;;   http://www.gnu.org/software/libc/manual/html_node/Defining-Handlers.html#Defining-Handlers
;;;
;;; See full documentation at
;;;
;;;   https://github.com/orthecreedence/cl-signal-handler
;;;
;;; Please enjoy irresponsibly =].
;;;
;;; Andrew Lyon <orthecreedence@gmail.com>

(defpackage :cl-signal-handler
  (:export :set-signal-handler
           :with-signal-handler)
  (:nicknames :ush))
(in-package :cl-signal-handler)

(defmacro set-signal-handler (signo &body body)
  "Replace the current handler for the signal number under signo, and return a
  pointer to the handler that is being replaced."
  (let ((handler (gensym "HANDLER")))
    `(progn
       (cffi:defcallback ,handler :void ((signo :int))
         (declare (ignore signo))
         ,@body)
       (cffi:foreign-funcall "signal" :int ,signo :pointer (cffi:callback ,handler) :pointer))))

(defmacro with-signal-handler (signo handler-fn &body body)
  "Allows you to execute a block of code using the signal handling function
  specified by handler-fn. Once the body is finished, the original signal
  handler will always be restored."
  `(let ((default-handler (set-signal-handler ,signo (funcall ,handler-fn))))
     (unwind-protect 
       (progn ,@body)
       (cffi:foreign-funcall "signal" :int ,signo :pointer default-handler :pointer))))
