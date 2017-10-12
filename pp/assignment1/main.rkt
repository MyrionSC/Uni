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
(define piano 0)
(define organ 1)
(define guitar 2)
(define violin 3)
(define flute 4)
(define trumpet 5)
(define helicopter 6)
(define telephone 7)
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
          ((note? me) (get-note-dur me)) ;transform to note-abs
          ((pause? me) (get-pause-dur me)) ;get duration somehow
          ((seq? me) (get-music-dur (get-els me))) ;call recursively on all elements
          ((par? me) (get-music-dur (get-els me)))
          ((if (par? (head me)) (max (get-music-dur (head me)) (get-music-dur (tail me)))
               (+ (get-music-dur (head me)) (get-music-dur (tail me)))))))

;;; 6: is monophonic function

;;; 7: degree of polyphony function

;;; 8: transform to list of note-abs-time-with-duration

(define (transform-to-note-abs-time-with-duration me)
  (cond ((! (music-element? me)) (raise "input not music element" #t))
        ((cdr (transform-helper me 0)))))


(define (transform-helper me dur)
  (cond ((note? me) (cons (+ dur (get-note-dur me)) '())) ;transform to note-abs
        ((pause? me) "pause") ;get duration somehow
        ((seq? me) "seq")  ;call recursively on all elements
        ((par? me) "par")
        ("maybe list?")))
;call recursively on all elements


;; should be in helper
        ;((note? me) "note") ;transform to note-abs
        ;((pause? me) "pause") ;ignore
        ;((seq? me) "seq") ;call recursively on all elements
        ;("par"))) ;call recursively on all elements

;;; test
(define n3 (note 60 half piano))
(define tel (par (list (note 60 half piano) (pause half) (note 60 half piano) (seq (list (note 60 full piano) (pause quarter))) (par (list (note 60 full piano) (note 60 full piano) (pause quarter))))))
;(get-music-dur tel)


(define n1 (note 1 2 3))
(define p1 (pause 123))
(define s1 (seq (list n1 p1)))
(define par1 (par (list s1)))
(define text "test")

;; music element
;;   - Note (pitchvalue, duration, instrument)
;;   - pause (duration)
;;   - sequentialMusicElement (musicElements)
;;   - parallelMusicElement (musicElements)

;; pitchvalue: int between 0 and 127
;; duration: timeunit where 960 is a second
;; instruments: Piano, Organ, Guitar, Violin, Flute, Trumpet, Helicopter, Telephone

;;; canon
(define test (note 60 half piano))
(define testelement (seq (list (note 60 half piano) (pause half) (note 60 half piano))))
(define peter-jakob (seq (list (note 60 half piano) (pause half) (note 60 half piano) (pause half) (note 60 half piano) (pause half) (note 60 half piano) (pause half) (note 60 half piano))))
;(transform-to-note-abs-time-with-duration test)

;abs-time:	A non-negative integer, in time ticks. The absolute start time of the note.
;channel:	An integer between 1 and 16. The MIDI channel allocated to the requested instrument. Se the miniproject formulation for more details.
;note-number:	An integer between 0 and 127. The MIDI note number.
;velocity:	An integer between 0 and 127. The velocity (strength) of the note. Not controllable in this exercise. It can be a constant, such as 80.
;duration:	A non-negative integer. The duration of this note (a number of time ticks).
 

;(define notelist (list (note-abs-time-with-duration 0 1 60 80 1920) (note-abs-time-with-duration 1920 1 64 80 960) (note-abs-time-with-duration 2880 1 62 80 1920) (note-abs-time-with-duration 4800 1 64 80 960) (note-abs-time-with-duration 5760 1 60 80 1920) (note-abs-time-with-duration 7680 1 59 80 960) (note-abs-time-with-duration 8640 1 57 80 1920) (note-abs-time-with-duration 10560 1 60 80 960) (note-abs-time-with-duration 11520 1 64 80 1920) (note-abs-time-with-duration 13440 1 64 80 960) (note-abs-time-with-duration 14400 1 64 80 1920) (note-abs-time-with-duration 16320 1 60 80 960) (note-abs-time-with-duration 17280 1 57 80 3840)))
;(transform-to-midi-file-and-write-to-file! notelist "generated-music.mid")
