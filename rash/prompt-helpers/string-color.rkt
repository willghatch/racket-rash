#lang rash

(require racket/list
         racket/class
         racket/set)


(provide create-colored-string
         create-styled-struct
         styled-struct->string)


; hash for 4 bit colors
; (color-name . '(foreground-color-code background-color-code))
(define color-hash
  #hash(("black" . (30 40))
        ("red" . (31 41))
        ("green" . (32 42))
        ("yellow" . (33 43))
        ("blue" . (34 44))
        ("magenta" . (35 45))
        ("cyan" . (36 46))
        ("white" . (37 47))
        ("bright black" . (90 100))
        ("bright red" . (91 101))
        ("bright green" . (92 102))
        ("bright yellow" . (93 103))
        ("bright blue" . (94 104))
        ("bright magenta" . (95 105))
        ("bright cyan" . (96 106))
        ("bright white" . (97 107))))


; if you give a string as a color, it will treat it as a name for a 4 bit
; color number that it will get from `color-hash`
; unless it starts with a "#", then it will be treated as a hex color
; if it is a hex that is 3 long, it will be expanded into a normal size hex:
; #rgb -> #rrggbb
; if you give a `color%` object it will treat it as rgb and ignore alpha
; if you give a number it will treat it at an 8bit/256color number
; if you give a list, it will treat it as a 24 bit/rgb color
; #f means no color and is default
; #:fg "green" -> treated as 4 bit
; #:fg 41 -> treated as 8 bit springgreen3 color
; #:fg '(r g b) -> treated as a 24 bit color
(define (create-colored-string to-color
                               #:fg [foreground #f]
                               #:bg [background #f]
                               #:bold? [bold? #f]
                               #:italic? [italic? #f]
                               #:underlined? [underlined? #f]
                               #:reset-before? [reset-before? #t] ; reset all attributes before setting your own
                               #:reset-after? [reset-after? #t]
                               #:other-commands [other-commands ""]) ; string with normal ansi escape commands

  ; if user inputs a color% object, change it to a normal r g b list, ignoring opacity
  (when (and (object? foreground) (subset? '(red green blue) (interface->method-names (object-interface foreground))))
    (set! foreground (list (send foreground red)
                           (send foreground green)
                           (send foreground blue))))

  (when (and (object? background) (subset? '(red green blue) (interface->method-names (object-interface background))))
    (set! background (list (send background red)
                           (send background green)
                           (send background blue))))

  ; if user inputs a string starting with, "#", convert it to an rgb list
  (when (and (string? foreground) (regexp-match "^#" foreground))
    (when (= 4 (string-length foreground)) ; change #rgb to #rrggbb
      (set! foreground (let ([r (string-ref foreground 1)]
                             [g (string-ref foreground 2)]
                             [b (string-ref foreground 3)])
                         (string-append "#" (string r r g g b b)))))
    (set! foreground
          (map string->number (list (string-append "#x" (substring foreground 1 3))
                                    (string-append "#x" (substring foreground 3 5))
                                    (string-append "#x" (substring foreground 5 7))))))

  (when (and (string? background) (regexp-match "^#" background))
    (when (= 4 (string-length background)) ; change #rgb to #rrggbb
      (set! background (let ([r (string-ref background 1)]
                             [g (string-ref background 2)]
                             [b (string-ref background 3)])
                         (string-append "#" (string r r g g b b)))))
    (set! background
          (map string->number (list (string-append "#x" (substring background 1 3))
                                    (string-append "#x" (substring background 3 5))
                                    (string-append "#x" (substring background 5 7))))))

  (define foreground-command
    (if foreground
        (cond
          [(string? foreground) (format "\033[~am" (first (hash-ref color-hash foreground)))]
          [(number? foreground) (format "\033[38;5;~am" foreground)]
          [(list? foreground) (format "\033[38;2;~a;~a;~am"
                                      (first foreground)
                                      (second foreground)
                                      (third foreground))])
        ""))

  (define background-command
    (if background
        (cond
          [(string? background) (format "\033[~am" (second (hash-ref color-hash background)))]
          [(number? background) (format "\033[48;5;~am" background)]
          [(list? background) (format "\033[48;2;~a;~a;~am"
                                      (first background)
                                      (second background)
                                      (third background))])
        ""))

  (define-values (bold-str ital-str und-str resetbf-str resetaft-str)
    (values (if bold? "\033[1m" "")
            (if italic? "\033[3m" "")
            (if underlined? "\033[4m" "")
            (if reset-before? "\033[0m" "")
            (if reset-after? "\033[0m" "")))

  (string-append resetbf-str
                 foreground-command
                 background-command
                 bold-str
                 ital-str
                 und-str
                 other-commands
                 to-color
                 resetaft-str))


; body is a list of stings and other structs
; style is a string with the style
(struct styled-string (body style))


; takes mostly same arguments as `create-colored-string`
; creates a struct with the style generated by `create-colored-string`
(define (create-styled-struct #:fg [foreground #f]
                              #:bg [background #f]
                              #:bold? [bold? #f]
                              #:italic? [italic? #f]
                              #:underlined? [underlined? #f]
                              #:reset-before? [reset-before? #f] ; #f because we want nested structs to inherit style
                              #:other-commands [other-commands ""]
                              . to-color)
  (styled-string to-color (create-colored-string ""
                                                 #:fg foreground ; is there a more efficient way to pass these to `create-colored-string`?
                                                 #:bg background
                                                 #:bold? bold?
                                                 #:italic? italic?
                                                 #:underlined? underlined?
                                                 #:reset-before? reset-before?
                                                 #:reset-after? #f ; because we're going to be appending the text to the style so we do want to reset it right before
                                                 #:other-commands other-commands)))


; recursively converts a styled struct into a string
(define (styled-struct->string ss [outer-style "\033[0m"]) ; no outer style means it is the outermost struct, so we want to clear the style just in case
  (define this-style (string-append outer-style (styled-string-style ss))) ; inherit from outer structs
  (string-append (foldl (lambda (element result)
                          (if (string? element)
                              (string-append result (string-append this-style element)) ; just apply the current style
                              (string-append result (styled-struct->string element this-style))))
                        ""
                        (styled-string-body ss))
                 "\033[0m"))


; I don't wanna type out ansi escape sequences for tests, so here are some examples instead
; they should do what you think they do
(module+ main
  (displayln (create-colored-string "asdf123test hi there im a test string! bye!"
                                    #:fg '(1 161 82)
                                    #:bg "#0a5"
                                    #:bold? #t
                                    #:underlined? #t
                                    #:italic? #t)))


(module+ main
  (displayln (styled-struct->string (create-styled-struct "I'm green and hilighted blue"
                                                          (create-styled-struct " im still green but higighted yellow and underlined " #:bg "yellow" #:underlined? #t)
                                                          "I'm green and hilighted blue again (and not underlined)"
                                                          #:fg "green"
                                                          #:bg "blue"))))
