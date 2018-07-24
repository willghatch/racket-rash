#lang racket/base

(require racket/list
         racket/class
         racket/set
         racket/contract)


(provide create-styled-string
         create-styled-struct
         styled-struct->string
         color-value?)


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


(define (valid-hex? v)
  (and (string? v)
       (or (= (string-length v) 7) (= (string-length v) 4))
       (char=? (string-ref v 0) #\#)
       (subset? (rest (string->list v)) (string->list "0123456789abcdef"))))


(define (rgb-component? v)
  (and (exact-integer? v) (<= 0 v 255)))


(define (color-value? v)
  (cond
    [(string? v) (or (subset? (list v) (hash-keys color-hash)) (valid-hex? v))]
    [(exact-integer? v) (rgb-component? v)]
    [(list? v) (andmap rgb-component? v)]
    [(object? v) (and (subset? '(red green blue) (interface->method-names (object-interface v)))
                      (andmap rgb-component?
                              (list (send v red)
                                    (send v green)
                                    (send v blue))))]
    [else #f]))


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
(define/contract (create-styled-string [to-style ""]
                              #:fg [foreground #f]
                              #:bg [background #f]
                              #:bold? [bold? #f]
                              #:italic? [italic? #f]
                              #:underlined? [underlined? #f]
                              #:reset-before? [reset-before? #t] ; reset all attributes before setting your own
                              #:reset-after? [reset-after? #t]
                              #:custom-commands [custom-commands ""] ; string with normal ansi escape commands
                              #:create-function? [create-function? #f]) ; create a re-usable function that accepts a string (includes `to-color`)

  (() ; technically no arguments required
   (string?
   #:fg (or/c color-value? #f)
   #:bg (or/c color-value? #f)
   #:bold? boolean?
   #:italic? boolean?
   #:underlined? boolean?
   #:reset-before? boolean?
   #:reset-after? boolean?
   #:custom-commands string?
   #:create-function? boolean?)
   . ->* . (or/c string? (-> string? string?)))

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

  (if create-function?
      (λ (str) (string-append resetbf-str
                              foreground-command
                              background-command
                              bold-str
                              ital-str
                              und-str
                              custom-commands
                              to-style
                              str
                              resetaft-str))
      (string-append resetbf-str
                     foreground-command
                     background-command
                     bold-str
                     ital-str
                     und-str
                     custom-commands
                     to-style
                     resetaft-str)))


; body is a list of stings and other structs
; style is a string with the style
(struct styled-struct (body style))


(define default (string->uninterned-symbol "default"))


; takes mostly same arguments as `create-styled-string`
; creates a struct with the style generated by `create-styled-string`
; for style arguments `default` is treated as keep whatever outer struct style was,
; #f means get rid out outer style
(define/contract (create-styled-struct #:fg [foreground default]
                                       #:bg [background default]
                                       #:bold? [bold? default]
                                       #:italic? [italic? default]
                                       #:underlined? [underlined? default]
                                       #:reset-before? [reset-before? #f] ; this doesn't get passed to the actual
                                       ; create-styled-string function
                                       #:custom-commands [custom-commands ""] ; will get appended to outer customs
                                       #:reset-customs? [reset-customs? #f] ; unless this is set to `#t`
                                       . to-style)

  (()
   (#:fg (or/c color-value? default #f)
    #:bg (or/c color-value? default #f)
    #:bold? (or/c boolean? default)
    #:italic? (or/c boolean? default)
    #:underlined? (or/c boolean? default)
    #:reset-before? boolean?
    #:custom-commands string?
    #:reset-customs? boolean?)
   #:rest (listof (or/c string? styled-struct?))
   . ->* . styled-struct?)
  
  (define style-hash (make-hash))
  (hash-set! style-hash 'foreground foreground)
  (hash-set! style-hash 'background background)
  (hash-set! style-hash 'bold? bold?)
  (hash-set! style-hash 'italic? italic?)
  (hash-set! style-hash 'underlined? underlined?)
  (hash-set! style-hash 'reset-before? reset-before?)
  (hash-set! style-hash 'custom-commands custom-commands)
  (hash-set! style-hash 'reset-customs? reset-customs?)

  (styled-struct to-style style-hash))


; h2's styles override h1's styles
(define (create-combined-style-hash h1 h2)
  (define (decide-new hash-key)
    (cond
      [(equal? default (hash-ref h2 hash-key)) (if (hash-ref h2 'reset-before?) ; check if user expects no inherited
                                                   #f
                                                   (hash-ref h1 hash-key))]
      [else (hash-ref h2 hash-key)]))
  (define new-hash (make-hash))
  (hash-set! new-hash 'foreground (decide-new 'foreground))
  (hash-set! new-hash 'background (decide-new 'background))
  (hash-set! new-hash 'bold? (decide-new 'bold?))
  (hash-set! new-hash 'italic? (decide-new 'italic?))
  (hash-set! new-hash 'underlined? (decide-new 'underlined?))
  (hash-set! new-hash 'reset-before? (hash-ref h2 'reset-before?)) ; defaults to #f, not `default`
  (hash-set! new-hash 'custom-commands (if (or (hash-ref h2 'reset-customs?)
                                               (hash-ref h2 'reset-before?))
                                           (hash-ref h2 'custom-commands) ; only use new customs
                                           (string-append (hash-ref h1 'custom-commands)
                                                          (hash-ref h2 'custom-commands))))
  (hash-set! new-hash 'reset-customs? #f) ; doesn't matter what we put here since it's only used in this function
  ; and this hash will be h1 next time, so it wont be used
  new-hash)


; recursively converts a styled struct into a string
(define/contract (styled-struct->string ss [outer-style-hash #hash((foreground . #f) ; no outer means no styles
                                                                   (background . #f)
                                                                   (bold? . #f)
                                                                   (italic? . #f)
                                                                   (underlined? . #f)
                                                                   (reset-before? . #t)
                                                                   (custom-commands . "")
                                                                   (reset-customs? . #t))]) ; doesn't matter what is here because reset-before is #t

  ((styled-struct?) (hash?) . ->* . string?)

  (define this-style (create-combined-style-hash outer-style-hash
                                                 (styled-struct-style ss))) ; inherit from outer style

  (define this-style-raw (create-styled-string ""
                                               #:fg (hash-ref this-style 'foreground)
                                               #:bg (hash-ref this-style 'background)
                                               #:bold? (hash-ref this-style 'bold?)
                                               #:italic? (hash-ref this-style 'italic?)
                                               #:underlined? (hash-ref this-style 'underlined?)
                                               #:reset-before? #t ; this doesn't affect sub-struct's styles
                                               #:reset-after? #f ; we will be appending the string to the style
                                               #:custom-commands (hash-ref this-style 'custom-commands)))

  (string-append (foldl (lambda (element result)
                          (if (string? element)
                              (string-append result (string-append this-style-raw element)) ; just apply the current style
                              (string-append result (styled-struct->string element this-style))))
                        ""
                        (styled-struct-body ss))
                 "\033[0m"))


; I don't wanna type out ansi escape sequences for tests, so here are some examples instead
; they should do what you think they do
;(module+ main
;  (println (create-styled-string "asdf123test hi there im a test string! bye!"
;                                    #:fg '(1 161 82)
;                                    #:bg "#0a5"
;                                    #:bold? #t
;                                    #:underlined? #t
;                                    #:italic? #t)))


(module+ main
  ; should this go in a test module since it's technically a test?
  (displayln "\nRunning string-style main module\n\n")
  (displayln "Color string display tests:\n")
  (displayln (create-styled-string "My background should be red if the terminal supports 4 bit colors, which it definately should."
                                   #:bg "red"
                                   #:fg "black"))
  (displayln (create-styled-string "My background should be \"dark golden rod\", color code 136, if your terminal supports 8 bit (256 colors), which it probably should."
                                   #:bg 136
                                   #:fg "black"))
  (displayln (create-styled-string "My background should be a pale violet red color rgb(168,94,158) if your terminal supports true/24bit color.  Many modern terminals do, but 256 color is usually good enough."
                                   #:bg '(168 94 158)
                                   #:fg "black"))

  (displayln "\nOther string style tests:\n")
  (displayln (create-styled-string "I should look like normal text."))
  (displayln (create-styled-string "I should be bold." #:bold? #t))
  (displayln (create-styled-string "I should be italic." #:italic? #t))
  (displayln (create-styled-string "I should be underlined." #:underlined? #t))
  (displayln (create-styled-string "I should be bold, italic, and underlined." #:bold? #t #:italic? #t #:underlined? #t))
  (displayln (create-styled-string "If your terminal supports it, I should be faint, or have decreased intensity." #:custom-commands "\033[2m"))

  (displayln "\nStyled struct tests:\n")
  (displayln (styled-struct->string (create-styled-struct "I'm green and hilighted blue. "
                                                          (create-styled-struct "Im still green but higighted yellow and underlined." #:bg "yellow" #:underlined? #t)
                                                          " I'm green and hilighted blue again (and not underlined)"
                                                          #:fg "green"
                                                          #:bg "blue")))
  (displayln
   (styled-struct->string (create-styled-struct #:underlined? #t
                                                #:bg "blue"
                                                "underlined"
                                                (create-styled-struct #:underlined? #f
                                                                      " not underlined ")
                                                "underlined"
                                                (create-styled-struct #:reset-before? #t "\n")))))
