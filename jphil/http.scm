(define-module jphil.http
  (use rfc.uri)
  (use rfc.http)
  (use srfi-11)
  (export-all))
(select-module jphil.http)

(define-class <http-client-request> ()
  ((server :init-keyword :server)
   (port   :init-keyword :port :init-value 80)
   (query  :init-keyword :query :init-value "/")
   (method :init-keyword :method :init-value 'get)
   (file   :init-keyword :file :init-value #f)
   ))

(define-class <http-server-reply> ()
  ((code :init-keyword :code)
   (head :init-keyword :head)
   (data :init-keyword :data)))

(define-method send-query ((self <http-client-request>))
  (define http-fetch
    (case (~ self 'method)
      ((get) http-get)
      ((post) http-post)
      ((head) http-head)
      (else (error (format #f "Unknown http method: ~s\n" (~ self 'method))))))
  (define url (format #f "~a:~a" (~ self 'server)(~ self 'port)))
  (let-values (((code head data)
		(if (~ self 'file)
		    (call-with-output-file (~ self 'file)
		      (lambda (p)
			(http-fetch url (~ self 'query) :sink p :flusher (lambda (out head) (format #f "Saved in ~a\n" (~ self 'file))))))
		    (http-fetch url (~ self 'query)))))
    (make <http-server-reply> :code code :head head :data data)))

(define-method object-apply ((self <http-client-request>))
  (send-query self))

(define-method object-apply ((self <http-server-reply>))
  
  (~ self 'data))

;; <http-client-request> self evaluate to it's server response
;; ((make <http-client-request> ...)) => a <http-server-reply> object
;;
;; <http-server-reply> self evaluate to it's data
;; (<http-server-reply>)  => The data part of the reply
;;
;; so in short : (((make <http-client-request> ...))) => server reply data

