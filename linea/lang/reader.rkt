(module reader syntax/module-reader
  #:language (lambda (p) (read-syntax (object-name p) p))
  #:read-syntax linea-read-syntax
  #:read linea-read
  (require "../read.rkt"))
