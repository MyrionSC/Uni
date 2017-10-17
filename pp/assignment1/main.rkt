#lang racket
(require "music-base.rkt")

;;; helper functions
(define (get-item lst n)
  (if (eqv? lst '()) (raise "Out of index" #t)
      (if (= n 0) (car lst)
          (get-item (cdr lst) (- n 1)))))
(define (! pred)
  (if (eqv? pred #t) #f #t))
(define head car)
(define tail cdr)
(define (valid-music-element-type type)
  (or (eqv? type 'note) (or (eqv? type 'pause) (or (eqv? type 'seq) (eqv? type 'par)))))

;; global variables
(define piano 1)
(define organ 2)
(define guitar 3)
(define violin 4)
(define flute 5)
(define trumpet 6)
(define helicopter 7)
(define telephone 8)

;;; 1: create constructors for music elements

;; create super element with parameter validation
(struct music-element (type els props)
  #:transparent
  #:guard (lambda (type els props type-name)
            (cond
              [(! (valid-music-element-type type)) (error type-name "bad type: ~e. Should be one of: 'note 'pause 'seq 'par" type)]
              [(eqv? type 'note)
               (match props
                 [(list pit dur ins)
                  (cond
                    [(or (! (number? pit)) (! (and (> pit 0) (< pit 127)))) (error type-name "bad pitch. Should be number between 0 and 127 (inclusive)" type)]
                    [(! (positive? dur)) (error type-name "bad duration. Should be positive number" type)]
                    [(or (< ins 0) (> ins 16)) (error type-name "bad instrument. Should be number between 1 and 16 (inclusive)" type)]
                    [else (values type els props)])])]
              [(eqv? type 'pause)
               (match props
                 [(list dur)
                  (cond
                    [(! (positive? dur)) (error type-name "bad duration. Should be positive number" type)]
                    [else (values type els props)])])]
              ;[(or (eqv? type 'seq) (eqv? type 'par)) (if (! (list? els)) (error type-name "bad input. Should be list of music elements" type)
              ;                      (values type els props))] ;no time: something is wrong with this check, but I am out of time
              [else (values type els props)])))

;; create specific elements
(define (note pit dur ins)
  (music-element 'note '() (list pit dur ins)))
(define (pause dur)
  (music-element 'pause '() (list dur)))
(define (seq els)
  (music-element 'seq els '()))
(define (par els)
  (music-element 'par els '()))

;;; 2 - element predicates
(define (music-element-pred? type)
  (lambda (me)
    (and (music-element? me) (eqv? (music-element-type me) type))))
(define note? (music-element-pred? 'note))
(define pause? (music-element-pred? 'pause))
(define seq? (music-element-pred? 'seq))
(define par? (music-element-pred? 'par))

;;; 3: selectors
(define (get-note-item index)
  (lambda (n)
    (if (note? n) (get-item (music-element-props n) index)
        (raise "type not note" #t))))
(define get-note-pit (get-note-item 0))
(define get-note-dur (get-note-item 1))
(define get-note-ins (get-note-item 2))
(define (get-pause-dur p)
    (if (pause? p) (get-item (music-element-props p) 0)
        (raise "type not pause" #t)))
(define (get-els me)
  (if (or (seq? me) (par? me)) (music-element-els me)
      (raise "element not sequence or paralel music element")))

;;; 4: scale, transpose and reinstrument functions
(define (scale me mult)
    (cond ((eqv? me '()) '())
          ((note? me) (list (note (get-note-pit me) (* (get-note-dur me) mult) (get-note-ins me))))
          ((pause? me) (list (pause (* (get-pause-dur me) mult))))
          ((seq? me) (seq (scale (get-els me) mult)))
          ((par? me) (par (scale (get-els me) mult)))
          ((or (seq? (head me)) (par (head me))) (append (list (scale (head me) mult)) (scale (tail me) mult)))
          ((append (scale (head me) mult) (scale (tail me) mult)))))

; no time: should check if the pitch goes below zero
(define (transpose me value)
    (cond ((eqv? me '()) '())
          ((note? me) (list (note (+ (get-note-pit me) value) (get-note-dur me) (get-note-ins me))))
          ((pause? me) (list (pause (get-pause-dur me))))
          ((seq? me) (seq (transpose (get-els me) value)))
          ((par? me) (par (transpose (get-els me) value)))
          ((or (seq? (head me)) (par (head me))) (append (list (transpose (head me) value)) (transpose (tail me) value)))
          ((append (transpose (head me) value) (transpose (tail me) value)))))

; no time: should check if valid instrument
(define (reinstrument me new-ins)
    (cond ((eqv? me '()) '())
          ((note? me) (list (note (get-note-pit me) (get-note-dur me) new-ins)))
          ((pause? me) (list (pause (get-pause-dur me))))
          ((seq? me) (seq (reinstrument (get-els me) new-ins)))
          ((par? me) (par (reinstrument (get-els me) new-ins)))
          ((or (seq? (head me)) (par (head me))) (append (list (reinstrument (head me) new-ins)) (reinstrument (tail me) new-ins)))
          ((append (reinstrument (head me) new-ins) (reinstrument (tail me) new-ins)))))

;;; 5: duration of music element function
(define (get-music-dur me)
  (cond ((eqv? me '()) 0)
        ((note? me) (get-note-dur me))
        ((pause? me) (get-pause-dur me))
        ((or (seq? me) (par? me)) (get-music-dur (get-els me)))
        ((if (par? (head me)) (max (get-music-dur (head me)) (get-music-dur (tail me)))
             (+ (get-music-dur (head me)) (get-music-dur (tail me)))))))
; drawback: if there is a long pause at the end of the song that is counted as well

;;; 6: is monophonic function

;not implemented

;;; 7: degree of polyphony function

;not implemented

;;; 8: transform to list of note-abs-time-with-duration

(define (note-to-natwd n abstime)
  (note-abs-time-with-duration abstime (get-note-ins n) (get-note-pit n) 80 (get-note-dur n)))

(define (transform-to-note-abs-time-with-duration me)
  (cond ((! (music-element? me)) (raise "input not music element" #t))
        ((transform-helper me 0))))

(define (transform-helper me abstime)
  (cond ((eqv? me '()) '())
        ((note? me) (list (note-to-natwd me abstime)))
        ((pause? me) '())
        ((or (seq? me) (par? me)) (transform-helper (get-els me) abstime))
        ((if (par? (head me)) (append (transform-helper (head me) abstime) (transform-helper (tail me) abstime))
             (append (transform-helper (head me) abstime) (transform-helper (tail me) (+ abstime (get-music-dur (head me)))))))))


;;; canon
(define mester-jakob (seq (list
                           (note 65 480 piano)
                           (note 67 480 piano)
                           (note 69 480 piano)
                           (note 65 480 piano)
                           (note 65 480 piano)
                           (note 67 480 piano)
                           (note 69 480 piano)
                           (note 65 480 piano))))
(define hvor-er-du (seq (list
                         (note 69 480 piano)
                         (note 70 480 piano)
                         (note 72 960 piano)
                         (note 69 480 piano)
                         (note 70 480 piano)
                         (note 72 960 piano))))
(define ringer-du-med-klokken (seq (list
                                    (note 72 240 piano)
                                    (note 74 240 piano)
                                    (note 72 240 piano)
                                    (note 70 240 piano)
                                    (note 69 480 piano)
                                    (note 65 480 piano)
                                    (note 72 240 piano)
                                    (note 74 240 piano)
                                    (note 72 240 piano)
                                    (note 70 240 piano)
                                    (note 69 480 piano)
                                    (note 65 480 piano))))
(define bim-bam-bum (seq (list
                          (note 65 480 piano)
                          (note 60 480 piano)
                          (note 65 960 piano)
                          (note 65 480 piano)
                          (note 60 480 piano)
                          (note 65 960 piano))))

(define song (seq (list mester-jakob hvor-er-du ringer-du-med-klokken bim-bam-bum)))
(define canon (seq (list
                    mester-jakob
                    (reinstrument (par (list song)) 2) ;sætter en ny paralel "tråd" igang som spiller hele sangen
                    hvor-er-du
                    (reinstrument (par (list song)) 3)
                    ringer-du-med-klokken
                    (reinstrument (par (list song)) 4)
                    bim-bam-bum)))
(define slow-higher-transposed-canon (scale canon 2))

(define abscanon (transform-to-note-abs-time-with-duration slow-higher-transposed-canon))
(transform-to-midi-file-and-write-to-file! abscanon "mester-jakob.mid")
