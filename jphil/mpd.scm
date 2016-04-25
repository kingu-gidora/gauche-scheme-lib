(define-module jphil.mpd
  (use gauche.parameter)
  (use gauche.net)
  (use jphil.process)
  (use srfi-13)
  (export-all))

(select-module jphil.mpd)

(define mpd:port (make-parameter 6600))
(define mpd:host (make-parameter "localhost"))

(define mpd:process-exists? 
  (lambda ()
    (call-with-current-continuation
     (lambda (k)
       (for-each 
	(lambda (x) 
	  (when (#/^mpd/ x) 
		(k #t)))
	(process->list '(ps aux)))
       #f))))

(define mpd:connect
  (lambda ()
    (make-client-socket 'inet (mpd:host) (mpd:port))))

(define mpd-command
  (let ((*MPD* (mpd:connect)))
    (lambda (cmd)
      (dynamic-wind
	  (lambda ()
	    (unless (mpd:process-exists?) (error "MPD Daemon is not running"))
	    (when (eq? 'closed (socket-status *MPD*))
		  (set! *MPD* (mpd:connect))))
	  (lambda () (letrec ((buf '())
			      (loop (lambda (p) 
				      (let ((line (read-line p)))
					(when (#/^ACK/ line) (print line)(exit))
					(if (string=? "OK" line)
					    (reverse buf)
					    (begin					      
					      (unless (#/^OK MPD/ line)
						      (push! buf (let ((spl (string-split line #\:)))
								   (cons (string->symbol (car spl))
									 (string-trim-both (cadr spl))))))
					      (loop p)))))))
		       (call-with-client-socket *MPD*
						(lambda (in out)
						  (format out "~a\n" cmd)
						  (loop in)))))
	  (lambda () #f)))))

;; Querying status
(define mpd:clearerror (lambda () (mpd-command "clearerror")))
(define mpd:currentsong (lambda () (mpd-command "currentsong")))
(define mpd:status (lambda () (mpd-command "status")))
(define mpd:stats (lambda () (mpd-command "stats")))
(define mpd:idle-database (lambda () (mpd-command "idle database")))
(define mpd:idle-update (lambda () (mpd-command "idle update")))
(define mpd:idle-playlist (lambda () (mpd-command "idle playlist")))
(define mpd:idle-stored_playlist (lambda () (mpd-command "idle stored_playlist")))
(define mpd:idle-player (lambda () (mpd-command "idle player")))
(define mpd:idle-mixer (lambda () (mpd-command "idle mixer")))
(define mpd:idle-output (lambda () (mpd-command "idle output")))
(define mpd:idle-options (lambda () (mpd-command "idle options")))
(define mpd:idle-sticker (lambda () (mpd-command "idle sticker")))
(define mpd:idle-subscription (lambda () (mpd-command "idle subscription")))
(define mpd:idle-message (lambda () (mpd-command "idle message")))
(define mpd:idle-noidle (lambda () (mpd-command "noidle")))

;; Playback options
(define mpd:consume (lambda (state) (mpd-command (format #f "consume ~a" state))))
(define mpd:crossfade (lambda (seconds) (mpd-command (format #f "crossfade ~a" seconds))))
(define mpd:mixrampdb (lambda (decibels) (mpd-command (format #f "mixrampdb ~a" decibels))))
(define mpd:mixrampdelay (lambda (seconds) (mpd-command (format #f "mixrampdelay ~a" seconds))))
(define mpd:random (lambda (state) (mpd-command (format #f "random ~a" state))))
(define mpd:repeat (lambda (state) (mpd-command (format #f "repeat ~a" state))))
(define mpd:setvol (lambda (vol) (mpd-command (format #f "setvol ~a" vol))))
(define mpd:single (lambda (state) (mpd-command (format #f "single ~a" state))))
(define mpd:replay_gain_mode (lambda (mode) (mpd-command (format #f "replay_gain_mode ~a" mode))))
(define mpd:replay_gain_mode-off (lambda () (mpd:replay_gain_mode "off")))  
(define mpd:replay_gain_mode-track (lambda () (mpd:replay_gain_mode "track")))  
(define mpd:replay_gain_mode-album (lambda () (mpd:replay_gain_mode "album")))  
(define mpd:replay_gain_mode-auto (lambda () (mpd:replay_gain_mode "auto")))  
(define mpd:replay_gain_status (lambda () (mpd-command "replay_gain_status")))

;; Controlling playback
(define mpd:next (lambda () (mpd-command "next")))
(define mpd:previous (lambda () (mpd-command "previous")))
(define mpd:pause (lambda (state) (mpd-command (format #f "pause ~a" state))))
(define mpd:play (lambda () (mpd-command "play")))
(define mpd:play-at (lambda (pos) (mpd-command (format #f "play ~a" pos))))
(define mpd:playid (lambda (id) (mpd-command (format #f "playid ~a" id))))
(define mpd:stop (lambda () (mpd-command "stop")))

;; The current Playlist
(define mpd:clear-playlist (lambda () (mpd-command "clear")))
(define mpd:add-to-playlist (lambda (uri) (mpd-command (format #f "add ~s" uri))))
(define mpd:addid (lambda (uri) (mpd-command (format #f "addid ~s" uri))))
(define mpd:addid-at-position (lambda (uri pos) (mpd-command (format #f "addid ~s ~a" uri pos))))


;; The music database
(define mpd:update-all (lambda () (mpd-command "update")))
(define mpd:update (lambda (uri) (mpd-command (format #f "update ~s" uri))))
(define mpd:listall (lambda () (mpd-command "listall")))


