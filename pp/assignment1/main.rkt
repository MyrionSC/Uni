#lang racket

(require "music-base.rkt")
;test data
(define notelist (list (note-abs-time-with-duration 0 1 60 80 1920) (note-abs-time-with-duration 1920 1 64 80 960) (note-abs-time-with-duration 2880 1 62 80 1920) (note-abs-time-with-duration 4800 1 64 80 960) (note-abs-time-with-duration 5760 1 60 80 1920) (note-abs-time-with-duration 7680 1 59 80 960) (note-abs-time-with-duration 8640 1 57 80 1920) (note-abs-time-with-duration 10560 1 60 80 960) (note-abs-time-with-duration 11520 1 64 80 1920) (note-abs-time-with-duration 13440 1 64 80 960) (note-abs-time-with-duration 14400 1 64 80 1920) (note-abs-time-with-duration 16320 1 60 80 960) (note-abs-time-with-duration 17280 1 57 80 3840)))










(transform-to-midi-file-and-write-to-file! notelist "generated-music.mid")
