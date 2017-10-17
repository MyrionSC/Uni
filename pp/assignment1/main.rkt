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
(define (p val) ;prints a value
  (display val)
  (newline)
  val)
(define (valid-music-element-type type)
  (or (eqv? type 'note) (or (eqv? type 'pause) (or (eqv? type 'seq) (eqv? type 'par)))))

;;; global variables
(define piano 1)
(define organ 2)
(define guitar 3)
(define violin 4)
(define flute 5)
(define trumpet 6)
(define helicopter 7)
(define telephone 8)
(define sec 960)
(define eight (/ sec 8))
(define quarter (/ sec 4))
(define half (/ sec 2))
(define full sec)
(define velo 80)



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
              ;[(or (eqv? type 'seq) eqv? type 'par) (if (! (list? els)) (error type-name "bad input. Should be list of music elements" type)
              ;                      (values type els props))]
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
  ;(display me)
  ;(newline)
    (cond ((eqv? me '()) '())
          ((note? me) (list (note (get-note-pit me) (* (get-note-dur me) mult) (get-note-ins me))))
          ((pause? me) (list (pause (* (get-pause-dur me) mult))))
          ((seq? me) (seq (scale (get-els me) mult)))
          ((par? me) (par (scale (get-els me) mult)))
          ((or (seq? (head me)) (par (head me))) (append (list (scale (head me) mult)) (scale (tail me) mult)))
          ((append (scale (head me) mult) (scale (tail me) mult)))))
          ;((append (scale (head me) mult) (scale (tail me) mult)))))

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

;;; 7: degree of polyphony function

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

;;; test
(define n1 (note 1 2 3))
(define p1 (pause 123))
(define s1 (seq (list n1 p1)))
(define par1 (par (list s1)))
(define text "test")

(define t1 (note-to-natwd n1 1))
(define t2 (note-to-natwd n1 2))

(define mester-jakob (seq (list
                           (note 67 480 piano)
                           (seq (list
                                 (note 67 480 piano)
                                 (note 67 480 piano)
                                 (note 67 480 piano)))
                           (note 69 480 piano)
                           (note 65 480 piano))))

mester-jakob
(define mester-jakob-scaled (scale mester-jakob 1))
mester-jakob-scaled
(define abscanon (transform-to-note-abs-time-with-duration mester-jakob-scaled))
(transform-to-midi-file-and-write-to-file! abscanon "mester-jakob.mid")
