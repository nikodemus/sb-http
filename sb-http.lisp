;;;; Minimal HTTP Client.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is in the public domain and is provided with absolutely no
;;;; warranty. See the COPYING and CREDITS files for more information.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-bsd-sockets))

(defpackage :sb-http
  (:use :cl :sb-ext :sb-int :sb-bsd-sockets)
  (:shadow #:get)
  (:export
   "GET"
   "*PROXY*"))

(in-package :sb-http)

(defvar *proxy* (posix-getenv "http_proxy")
  "Default HTTP proxy to use. Initial value comes from the environment variable
http_proxy at the time SB-HTTP is loaded.")

(defvar *user-agent* "SB-HTTP/1.0"
  "Default User-Agent. Defaults to SB-HTTP/1.0.")

(defun check-http-url (url)
  (unless (or (> 7 (length url)) (string-equal url "http://" :end1 7))
    (error "Not an HTTP URL: ~S" url)))

(defun url-host (url)
  (check-http-url url)
  (let* ((port-start (position #\: url :start 7))
         (host-end (min (or (position #\/ url :start 7) (length url))
                        (or port-start (length url)))))
    (subseq url 7 host-end)))

(defun url-port (url)
  (check-http-url url)
  (let ((port-start (position #\: url :start 7)))
    (if port-start
        (values (parse-integer url :start (1+ port-start) :junk-allowed t))
        80)))

(defun request-uri (url proxy)
  (check-http-url url)
  (if proxy
      url
      (let ((path-start (position #\/ url :start 7)))
        (if path-start
            (subseq url path-start)
            "/"))))

(defun read-http-line (stream)
  ;; This piece of cruft uses CODE-CHAR and READ-BYTE to get CRLF terminated
  ;; status and headers from the stream: the external format need not be
  ;; iso-8859-1, but that's what the headers use.
  (let (last tmp)
    (flet ((stop (char)
             (cond ((and (eql char #\newline)
                         (eql last #\return))
                    t)
                   (t
                    (setf last char)
                    nil))))
      (let ((line (with-output-to-string (line)
                    (loop for byte = (read-byte stream nil nil)
                          for char = (when byte (code-char byte))
                          until (or (not char) (stop char))
                          do (when tmp
                               (write-char tmp line)
                               (setf tmp nil))
                          (if (eql #\return last)
                              (setf tmp last)
                              (write-char last line))))))
        (unless (zerop (length line))
          line)))))

(defun note (verbose control &rest arguments)
  (when verbose
    (fresh-line *trace-output*)
    (pprint-logical-block (*trace-output* nil :per-line-prefix "; ")
      (apply #'format *trace-output* control arguments))
    (terpri *trace-output*)
    (finish-output *trace-output*)))

(defun connect-http-socket (verbose socket addresses port)
  (tagbody
   :connect
     (let ((addr (pop addresses)))
       (note verbose "Connecting to ~{~S~^.~}" (coerce addr 'list))
       (handler-bind ((error (lambda (e)
                               (note verbose "Could not connect~% ~
                                              Error: ~A.~% ~
                                              ~:[No other addresses to try~
                                              ~;Trying next address~]"
                                     e
                                     addresses)
                               (when addresses
                                 (go :connect)))))
         (socket-connect socket addr port)))))

(defglobal **crlf** (concatenate 'string (string #\return) (string #\linefeed)))

(defmacro send-http-request (verbose verbose-headers socket headers)
  (with-unique-names (s b h r)
    (once-only ((verbose-headers-n verbose-headers))
      `(progn
         (note ,verbose "Sending request")
         (let* ((,s ,socket)
                (,b (with-output-to-string (,h)
                      ,@(mapcar (lambda (header)
                                  (destructuring-bind (test control &rest arguments) header
                                    `(when ,test
                                       (let ((header-string (format nil ,control ,@arguments)))
                                         (note ,verbose-headers-n " => ~A" header-string)
                                         (write-string header-string ,h))
                                       (write-string **crlf** ,h))))
                                headers)
                      (write-string **crlf** ,h)))
                (,r
                  (progn
                    (socket-send ,s ,b nil :external-format :iso-8859-1))))
           (unless (eql ,r (length ,b))
             (error "Problem sending headers, only ~A out of ~A bytes transmitted."
                    ,r (length ,b))))))))

(defun read-http-status (stream)
  (let* ((status (read-http-line stream))
         (length (length status))
         (end -1))
    (flet ((slice ()
             (when (>= end length)
               (error "Malformed HTTP status line: ~S" status))
             (let ((start (1+ end)))
               (setf end (position #\space status :start start))
               (subseq status start end))))
      (let ((http-version (slice))
            (status-code (slice))
            (reason-phrase (slice)))
        (values http-version
                (parse-integer status-code :radix 10)
                reason-phrase)))))

(defun trim (string &optional (start 0) (end nil))
  (string-trim '(#\space) (subseq string start end)))

(defvar *header-parsers* nil)

(defun parse-header (header)
  (let* ((colon (position #\: header))
         (key (when colon (intern (string-upcase (trim header 0 colon)) :keyword)))
         (value (if (and colon (< (1+ colon) (length header)))
                    (trim header (1+ colon))
                    (trim header))))
    (cons key
          (or (ignore-errors (parse-integer value))
              value))))

(defun read-http-headers (verbose verbose-headers stream)
  (note verbose "Reading headers")
  (multiple-value-bind (http-version status reason) (read-http-status stream)
    (values http-version status reason
            (loop for header = (read-http-line stream)
                  while header
                  do  (note verbose-headers " <= ~A" header)
                  collect (parse-header header)))))

(defun get (url &key (external-format :default)
                     (proxy *proxy*)
                     (redirection-limit 128)
                     (redirect t)
                     (verbose t)
                     (verbose-headers nil)
                     (user-agent *user-agent*))
  "Sends a HTTP/1.0 GET request to URL.

Returns three values: a stream, an association list of headers
received, and the HTTP protocol version as a string.

The stream returned is bivalent, and uses the specified
EXTERNAL-FORMAT.

Header names are upcased and interned into the KEYWORD package. Header
values are returned as strings, with the exception of those composed
of digits only, which are parsed into integers.

The request is sent using PROXY, defaulting to *PROXY*.

When REDIRECT is true (the default), automatically handles status 301
and 302 by redirecting to the location specified in headers. When
REDIRECT is false, signals a continuable error instead.

When VERBOSE is true (the default), status messages are sent to
*TRACE-OUTPUT*.

When VERBOSE-HEADERS is true (false by default), headers sent and
received are echoed to *TRACE-OUTPUT*.

The default USER-AGENT is *USER-AGENT*. If USER-AGENT is NIL, no
User-Agent header is sent.

Signals an error if cannot resolve to a status 200.

Experimental: interface subject to change."
  (let ((seen (make-hash-table :test 'equal))
        (redirection-count -1))
    (note verbose "Opening ~A~@[, using proxy ~A~]" url proxy)
    (tagbody
     :redirect
       (when (>= (incf redirection-count) redirection-limit)
         (error "Too many redirections: ~A" redirection-count))
       (if (gethash url seen)
           (error "Circular redirection to ~A" url)
           (setf (gethash url seen) t))
       (let* ((host (url-host url))
              (connect-url (or proxy url))
              (addresses (host-ent-addresses (get-host-by-name (url-host connect-url))))
              (port (url-port connect-url))
              (socket (make-instance 'inet-socket :type :stream :protocol :tcp))
              (ok nil))
         (unwind-protect
              (progn
                (connect-http-socket verbose socket addresses port)
                (send-http-request verbose verbose-headers socket
                                   ((t "GET ~A HTTP/1.0" (request-uri url proxy))
                                    (t "Host: ~A" host)
                                    (user-agent "User-Agent: ~A" user-agent)))
                (let ((stream (socket-make-stream socket
                                                  :input t :output t :buffering :full
                                                  :element-type :default
                                                  :external-format external-format)))
                  (multiple-value-bind (http-version status reason headers)
                      (read-http-headers verbose verbose-headers stream)
                    (when (member status '(301 302))
                      (let* ((text (ecase status
                                     (301 "Moved Permanently")
                                     (302 "Moved Temporarily")))
                             (location (or (cdr (assoc :location headers))
                                           (error "Got ~A ~A, but no Location in headers."
                                                  status text))))
                        (unless redirect
                          (cerror "Allow redirection"
                                  "~A ~A~@[ (~A)~], but redirection forbidden: ~A"
                                  status text
                                  (unless (string-equal text reason)
                                    reason)
                                  location))
                        (note verbose "~A~@[ (~A)~], redirecting to ~A"
                              text
                              (unless (string-equal text reason)
                                reason)
                              location)
                        (close stream)
                        (setf url location)
                        (go :redirect)))
                    (if (eql status 200)
                        (note verbose "~A" reason)
                        (cerror "Continue" "Got status ~S (~A)" status reason))
                    (setf ok t)
                    (return-from get (values stream headers http-version)))))
           (when (and (not ok) (socket-open-p socket))
             (socket-close socket :abort t)))))))
