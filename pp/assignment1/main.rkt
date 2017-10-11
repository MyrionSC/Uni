#lang racket

(require "music-base.rkt")
;test data
(define notelist (list (note-abs-time-with-duration 0 1 60 80 1920) (note-abs-time-with-duration 1920 1 64 80 960) (note-abs-time-with-duration 2880 1 62 80 1920) (note-abs-time-with-duration 4800 1 64 80 960) (note-abs-time-with-duration 5760 1 60 80 1920) (note-abs-time-with-duration 7680 1 59 80 960) (note-abs-time-with-duration 8640 1 57 80 1920) (note-abs-time-with-duration 10560 1 60 80 960) (note-abs-time-with-duration 11520 1 64 80 1920) (note-abs-time-with-duration 13440 1 64 80 960) (note-abs-time-with-duration 14400 1 64 80 1920) (note-abs-time-with-duration 16320 1 60 80 960) (note-abs-time-with-duration 17280 1 57 80 3840)))


;;; helper functions





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
(struct music-element (type children props))

;;; create specific elements
(define (note pit dur ins)
  (music-element 'note '() (list pit dur ins)))



(define (seq children)
  (music-element 'seq children '()))



;;; test inits
(define m1 (music-element 0 0 0))
(define n1 (note 0 0 0))
(define text "test")
(music-element-type m1)

;;; element predicates
(define (note? n)
  (if (and (music-element? n) (eqv? (music-element-type n) 'note)) #t
      #f))

;;; selectors






;;; scale, transpose and reinstrument functions

;;; duration of music element function

;;; is monophonic function

;;; degree of polyphony function

;;; transform to list of note-abs-time-with-duration



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
