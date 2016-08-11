#lang s-exp "lang.rkt"

ls -l \| wc %%read-newline-symbol
ls \| wc %%read-newline-symbol
      
