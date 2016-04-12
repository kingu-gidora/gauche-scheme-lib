(define-module jphil.imdb
  (use jphil.http)
  (use gauche.parameter)
  (use sxml.ssax)
  (use sxml.sxpath)
  (use file.util)
  (use jphil.csv)
  (use jphil.file)

  (export-all))
(select-module jphil.imdb)

(define http-server (make-parameter "www.myapifilms.com"))

;; go to http://www.myapifilms.com to request a API token
(define api-token (make-parameter "---"))
;; Set this to a directory where to cache the xml files returned by myapifilms
;; Since myapifilm requests are limited, better set it to a persistant place (not tmp)
;; TODO: Compress them to take less space.
(define imdb:xml-directory (make-parameter "path-to-xml-cache"))

(define-class <movie> ()
  ((id :init-keyword :id)
   (title :init-keyword :title)
   (countries :init-keyword :countries)
   (actors :init-keyword :actors)
   (directors :init-keyword :directors)
   (type :init-keyword :type)
   (genres :init-keyword :genres)
   (ratings :init-keyword :ratings :init-value '())
   (xml :init-keyword :xml) ))

(define-class <person> ()
  ((id :init-keyword :id)
   (name :init-keyword :name)))

(define-class <director> (<person>)
  ())

(define-class <actor> (<person>)
  ())

(define-class <rating> ()
  ((viewer :init-keyword :viewer)
   (rating :init-keyword :rating)))

(define imdb:getXML  
  (lambda (id)
    (let* ((client-request (make <http-client-request>
			     :server (http-server)
			     :query `("/imdb/idIMDB" (idIMDB ,id) (token ,(api-token))
				      (format "xml") (language "en-us") (aka 1)
				      (business 0) (seasons 1) (seasonYear 0) (technical 0) (trailers 0)
				      (movieTrivia 0) (awards 0) (moviePhotos 0) (movieVideos 0) (actors 2)
				      (biography 0) (uniqueName 0) (filmography 0) (bornAndDead 0) (starSign 0) 
				      (actorActress 1) (actorTrivia 0) (similarMovies 1) (goofs 0) (quotes 0)
				      )))
	   (server-reply (client-request))
	   (XML (server-reply)))
      (string->file XML (string-append (imdb:xml-directory) id ".xml"))
      XML)))
      

(define imdb:xml->movie 
  (lambda (f)
    (define extract 
      (lambda (sxml x)  
	((sxpath (append '(results data movie) x)) sxml)))
    
    (define extract-str
      (lambda (sxml x)  
	(let ((result (extract sxml x)))
	  ;(print result)
	  (if (null? (cdar result))
	      #f
	      (cadar result)))))
    
    (define extract-actors
      (lambda (sxml)    
	(map (lambda (x) 
	       (let ((g (lambda (i) (cadr (assoc i (cdr x))))))
		 (make <actor>
		   :id (g 'actorId)
		   :name (g 'actorName))))
	     (extract sxml '(actors actor)))))
    
    (define extract-directors
      (lambda (sxml)    
	(map (lambda (x) 	       
	       (let ((g (lambda (i)
			  (let ((zz (assoc i (cdr x))))
			    (if zz (cadr zz) #f)))))
		 (make <director>
		   :id (or (g 'nameId) (g 'id))
		   :name (g 'name))))
	     (extract sxml '(directors director)))))
    
    (define extract-type
      (lambda (sxml)    
	(cadar (extract sxml '(type)))))
    
    (define extract-genres
      (lambda (sxml)    
	(map (lambda (x) (cadr x)) (extract sxml '(genres genre)))))
    
    (define extract-countries
      (lambda (sxml)    
	(map (lambda (x) (cadr x)) (extract sxml '(countries country)))))

    (let ((SXML (call-with-input-string 
		 (cond
		  [(file-exists? f) (file->string f)]
		  [(file-exists? (string-append (imdb:xml-directory) f ".xml"))
		   (file->string (string-append (imdb:xml-directory) f ".xml"))]
		  (else (error "file not found")))
		 (lambda (p)
		   (ssax:xml->sxml p '())))))
		 
      (make <movie>
	:id (extract-str SXML '(idIMDB))
	:title (or (extract-str SXML '(originalTitle)) 
		   (extract-str SXML '(title)))
	:actors (extract-actors SXML)
	:countries (extract-countries SXML)
	:directors (extract-directors SXML)
	:type (extract-type SXML)
	:genres (extract-genres SXML)
	:xml SXML)
      )))

(define imdb:id->movie
  (lambda (id)
    (let ((f (string-append (imdb:xml-directory) id ".xml")))
      (unless (file-exists? f) 
	      (imdb:getXML id))	      
      (imdb:xml->movie f))))
	  
(define imdb:csv-for-each  
  (lambda (func :key (before #f)(after #f)(skip-first-line? #f))
    (csv-for-each "/tmp/imdb.csv" func :before before :after after :skip-first-line? skip-first-line?)))
 	    
