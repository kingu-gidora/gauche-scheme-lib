#!/usr/local/bin/gosh 
(add-load-path "/home/jphil/.gauche/gauche-scheme-lib/")
(use gauche.test)
(test-start "content-type")
(load "jphil/content-type")
(import jphil.content-type)

(test-module 'jphil.content-type)
(test-section "feature : db-file")

(test "db-file existence"
      #t
      (lambda () (file-exists? (content-type-db-file))))

(test "result"
      '("text" "plain")
      (lambda () (content-type-of "/etc/passwd")))
      
