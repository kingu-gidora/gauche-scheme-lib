(define-module jphil.ansi-term
  (export position up down forward backward save restore clear erase mode))

(select-module jphil.ansi-term)

(define codes
  '((reset        "\x1b[0m")
    (bold         "\x1b[1m")
    (underline    "\x1b[4m")
    (blink        "\x1b[5m")
    (reverse      "\x1b[7m")
    (invisible    "\x1b[8m")
    (black        "\x1b[0;30m")
    (red          "\x1b[0;31m")
    (green        "\x1b[0;32m")
    (brown        "\x1b[0;33m")
    (blue         "\x1b[0;34m")
    (purple       "\x1b[0;35m")
    (cyan         "\x1b[1;36m")
    (grey         "\x1b[0;37m")
    (bgblack      "\x1b[40m")
    (bgred        "\x1b[41m")
    (bggreen      "\x1b[42m")
    (bgyellow     "\x1b[43m")
    (bgblue       "\x1b[44m")
    (bgmagenta    "\x1b[45m")
    (bgcyan       "\x1b[46m")
    (bgwhite      "\x1b[47m")
    (darkgrey     "\x1b[1;30m")
    (lightred     "\x1b[1;31m")
    (lightgreen   "\x1b[1;32m")
    (yellow       "\x1b[1;33m")
    (lightblue    "\x1b[1;34m")
    (magenta      "\x1b[1;35m")
    (lightcyan    "\x1b[1;36m")
    (white        "\x1b[1;37m")))


(define (position line column)
  (string-append "\x1b[" (number->string line) ";" (number->string column) "H"))

(define (up lines)
  (string-append "\x1b[" (number->string lines) "A"))

(define (down lines)
  (string-append "\x1b[" (number->string lines) "B"))

(define (forward columns)
  (string-append "\x1b[" (number->string columns) "C"))

(define (backward columns)
  (string-append "\x1b[" (number->string columns) "D"))

(define (save)
  (string-append "\x1b[s"))

(define (restore)
  (string-append "\x1b[u"))

(define (clear)
  (string-append "\x1b[2J"))

(define (erase)
  (string-append "\x1b[K"))

(define (mode name)
  (cadr (assq name codes)))
