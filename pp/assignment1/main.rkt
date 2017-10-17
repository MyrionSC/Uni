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

;; create super element
(struct music-element (type els props))

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

;;; 5: duration of music element function
(define (get-music-dur me)
    (cond ((eqv? me '()) 0)
          ((note? me) (get-note-dur me))
          ((pause? me) (get-pause-dur me))
          ((seq? me) (get-music-dur (get-els me)))
          ((par? me) (get-music-dur (get-els me)))
          ((if (par? (head me)) (max (get-music-dur (head me)) (get-music-dur (tail me)))
               (+ (get-music-dur (head me)) (get-music-dur (tail me)))))))
; drawback: if there is a long pause at the end of the song that is counted as well



;;; 6: is monophonic function

;;; 7: degree of polyphony function

;;; 8: transform to list of note-abs-time-with-duration

(define (pause-to-natwd abstime)
  (display abstime)
  (newline)
  '())

(define (note-to-natwd n abstime)
  (display abstime) (newline)
  (note-abs-time-with-duration abstime (get-note-ins n) (get-note-pit n) 80 (get-note-dur n)))

(define (transform-to-note-abs-time-with-duration me)
  (cond ((! (music-element? me)) (raise "input not music element" #t))
        ((transform-helper me 0))))

(define (transform-helper me abstime)
  (cond ((eqv? me '()) '())
        ((note? me) (list (note-to-natwd me abstime)))
        ((pause? me) (pause-to-natwd abstime))
        ((or (seq? me) (par? me)) (transform-helper (get-els me) abstime))
        ((append (transform-helper (head me) abstime) (transform-helper (tail me) (+ abstime (get-music-dur (head me))))))))
        
        ;((if (seq? (head me)) (append (transform-helper (head me) abstime) (transform-helper (tail me) (+ abstime (get-music-dur (head me)))))
             ;(append (transform-helper (head me) abstime) (transform-helper (tail me) abstime))))))

;;; test
(define n1 (note 1 2 3))
(define p1 (pause 123))
(define s1 (seq (list n1 p1)))
(define par1 (par (list s1)))
(define text "test")

(define t1 (note-to-natwd n1 1))
(define t2 (note-to-natwd n1 2))
(define l1 (list t1 t1))
;t1
;l1
(append l1 (list t2))

(define testelement (seq (list (note 60 half piano) (pause half) (note 60 half piano))))
(define testseq (seq (list testelement (par (list testelement)) (pause 100) testelement)))

"how it looks now"
(transform-to-note-abs-time-with-duration testelement)


"how it should look"
;(define notelist (list (note-abs-time-with-duration 0 1 60 80 1920) (note-abs-time-with-duration 1920 1 64 80 960) (note-abs-time-with-duration 2880 1 62 80 1920) (note-abs-time-with-duration 4800 1 64 80 960) (note-abs-time-with-duration 5760 1 60 80 1920) (note-abs-time-with-duration 7680 1 59 80 960) (note-abs-time-with-duration 8640 1 57 80 1920) (note-abs-time-with-duration 10560 1 60 80 960) (note-abs-time-with-duration 11520 1 64 80 1920) (note-abs-time-with-duration 13440 1 64 80 960) (note-abs-time-with-duration 14400 1 64 80 1920) (note-abs-time-with-duration 16320 1 60 80 960) (note-abs-time-with-duration 17280 1 57 80 3840)))
(define notelist (list (note-abs-time-with-duration 0 1 60 80 1920) (note-abs-time-with-duration 1920 1 64 80 960) (note-abs-time-with-duration 2880 1 62 80 1920)))
notelist
;(transform-to-midi-file-and-write-to-file! notelist "generated-music.mid")








;;; canon
(define my-melody (seq  (list
                           (note 65 480 piano)
                           (note 67 480 piano)
                           (note 69 480 piano)
                           (note 65 480 piano)
                           (note 65 480 piano)
                           (note 67 480 piano)
                           (note 69 480 piano)
                           (note 65 480 piano)
                           (note 69 480 piano)
                           (note 70 480 piano)
                           (note 72 960 piano)
                           (note 69 480 piano)
                           (note 70 480 piano)
                           (note 72 960 piano)
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
                           (note 65 480 piano)
                           (note 65 480 piano)
                           (note 60 480 piano)
                           (note 65 960 piano)
                           (note 65 480 piano)
                           (note 60 480 piano)
                           (note 65 960 piano))))
;my-melody
;(define mester-jakob (transform-to-note-abs-time-with-duration my-melody))
;mester-jakob
;(transform-to-midi-file-and-write-to-file! mester-jakob "mester-jakob.mid")

