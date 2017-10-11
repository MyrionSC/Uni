#lang racket

(require "music-base.rkt")
;test data
(define notelist (list (note-abs-time-with-duration 0 1 60 80 1920) (note-abs-time-with-duration 1920 1 64 80 960) (note-abs-time-with-duration 2880 1 62 80 1920) (note-abs-time-with-duration 4800 1 64 80 960) (note-abs-time-with-duration 5760 1 60 80 1920) (note-abs-time-with-duration 7680 1 59 80 960) (note-abs-time-with-duration 8640 1 57 80 1920) (note-abs-time-with-duration 10560 1 60 80 960) (note-abs-time-with-duration 11520 1 64 80 1920) (note-abs-time-with-duration 13440 1 64 80 960) (note-abs-time-with-duration 14400 1 64 80 1920) (note-abs-time-with-duration 16320 1 60 80 960) (note-abs-time-with-duration 17280 1 57 80 3840)))


;;; helper functions
(define (get-item lst n)
  (if (eqv? lst '()) (raise "Out of index" #t)
      (if (= n 0) (car lst)
          (get-item (cdr lst) (- n 1)))))

;;; model

;; music element
;;   - Note (pitchvalue, duration, instrument)
;;   - pause (duration)
;;   - sequentialMusicElement (musicElements)
;;   - parallelMusicElement (musicElements)

;; pitchvalue: int between 0 and 127
;; duration: timeunit where 960 is a second
;; instruments: Piano, Organ, Guitar, Violin, Flute, Trumpet, Helicopter, Telephone

;;; 1: create constructors for music elements

;;; create super element
(struct music-element (type els props))

;;; create specific elements
(define (note pit dur ins)
  (music-element 'note '() (list pit dur ins)))
(define (pause dur)
  (music-element 'pause '() (list dur)))
(define (seq els)
  (music-element 'seq els '()))
(define (par els)
  (music-element 'par els '()))


;;; test inits
(define n1 (note 1 2 3))
(define p1 (pause 123))
(define s1 (seq (list n1 p1)))
(define par1 (par (list s1)))
(define text "test")


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

;;; 6: is monophonic function

;;; 7: degree of polyphony function

;;; 8: transform to list of note-abs-time-with-duration



;(struct note (pitch duration instrument))

;(define n (note 0 0 0))
;(note-pitch n)


;(define-values (struct:a make-a a? a-ref a-set!)
;  (make-struct-type 'a #f 2 1 'uninitialized))

;(define an-a (make-a 'x 'y))
;(define a-first (make-struct-field-accessor a-ref 0))

;(define-values (struct:b make-b b? b-ref b-set!)
;  (make-struct-type 'b struct:a 1 2 'b-uninitialized))
;(define a-b (make-b 'x 'y 'z))










(transform-to-midi-file-and-write-to-file! notelist "generated-music.mid")
