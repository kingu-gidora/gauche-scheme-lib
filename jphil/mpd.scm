(define-module jphil.mpd
  (use gauche.parameter)
  (use gauche.net)
  (use jphil.process)
  (use srfi-13)
  (export-all))

(select-module jphil.mpd)

(define mpd:port (make-parameter 6600))
(define mpd:host (make-parameter "localhost"))
(define mpd:on-ack #f)


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
	  (lambda () 
	    (call-with-current-continuation 
	     (lambda (k)
	       (letrec ((buf '())
			(loop (lambda (p) 
				(let ((line (read-line p)))
				  (when (#/^ACK/ line)
					(if mpd:on-ack 
					    (mpd:on-ack line)
					    (k #f)))
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
					    (loop in)))))))
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
(define mpd:shuffle (lambda () (mpd-command "shuffle")))

;; The music database
(define mpd:update-all (lambda () (mpd-command "update")))
(define mpd:update (lambda (uri) (mpd-command (format #f "update ~s" uri))))
(define mpd:listall (lambda () (mpd-command "listall")))
(define mpd:rescan  (lambda (uri) (mpd-command (format #f "rescan ~s" uri))))
(define mpd:rescan-all  (lambda () (mpd-command "rescan ~s")))

;; Mounts and neighbors

(define mpd:mount (lambda (path uri) (mpd-command (format #f "mount ~s ~s" path uri))))
(define mpd:unmount (lambda (path) (mpd-command (format #f "unmount ~s" path))))
(define mpd:listmounts (lambda () (mpd-command "listmounts")))
(define mpd:listneighbours (lambda () (mpd-command "listneighbours")))

;; Stickers

(define mpd:set-sticker (lambda (type uri name value) (mpd-command (format #f "sticker set ~s ~s ~s ~s" type uri name value))))
(define mpd:get-sticker (lambda (type uri name) (mpd-command (format #f "sticker get ~s ~s ~s" type uri name))))
(define mpd:delete-sticker (lambda (type uri name) (mpd-command (format #f "sticker delete ~s ~s ~s" type uri name))))
(define mpd:list-sticker (lambda (type uri) (mpd-command (format #f "sticker list ~s ~s" type uri))))
(define mpd:find-sticker (lambda (type uri name) (mpd-command (format #f "sticker find ~s ~s ~s" type uri name))))

;;Connection settings

(define mpd:close (lambda () (mpd-command "close")))
(define mpd:kill (lambda () (mpd-command "kill")))
(define mpd:ping (lambda () (mpd-command "ping")))
(define mpd:password (lambda (password) (mpd-command (format #f "password ~s" password))))

;; Audio output devices

(define mpd:outputs (lambda () (mpd-command "outputs")))
(define mpd:disableoutput (lambda (id) (mpd-command (format #f "disableoutput ~s" id))))
(define mpd:enableoutput (lambda (id) (mpd-command (format #f "enableoutput ~s" id))))
(define mpd:toggleoutput (lambda (id) (mpd-command (format #f "toggleoutput ~s" id))))

;; Reflection

(define mpd:commands (lambda () (mpd-command "commands")))
(define mpd:notcommands (lambda () (mpd-command "notcommands")))
(define mpd:tagtypes (lambda () (mpd-command "tagtypes")))
(define mpd:urlhandlers (lambda () (mpd-command "urlhandlers")))
(define mpd:decoders (lambda () (mpd-command "decoders")))

;; Client to client

(define mpd:subscribe (lambda (channel) (mpd-command (format #f "subscribe ~s" channel))))
(define mpd:unsubscribe (lambda (channel) (mpd-command (format #f "unsubscribe ~s" channel))))
(define mpd:channels (lambda () (mpd-command "channels")))
(define mpd:readmessage (lambda () (mpd-command "readmessage")))
(define mpd:sendmessage (lambda (channel text) (mpd-command (format #f "sendmessage ~s ~s" channel text))))
