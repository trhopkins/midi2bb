#lang racket/base

(require midi-readwrite) ; library for reading MIDI file itself
(require racket/runtime-path) ; find way to get rid of this?
(require racket/format) ; find simpler version of this?

;; basic definitions. Write a macro for these?

(define min-note 0) ; lowest possible note value
(define max-note 127) ; highest possible note value
(define high-threshold 72) ; higher octave
(define low-threshold 48) ; lower octave
(define high-prefix "j.") ; holding up. Try 8?
(define low-prefix "c.") ; holding down. Try 2?
(define show-octave #f) ; if #t, C5 instead of C for instance
(define include-milliseconds #f) ; eg. 2:38.350 instead of 2:38
(define-runtime-path midi-file "c:\\Users\\th8795\\Documents\\MIDI_sample.mid") ; input. Generalize?
(define parsed (midi-file-parse (build-path midi-file))) ; parse directly, note-for-note
#;(define song (MIDIFile->notelist parsed)) ; mushes all tracks together
(define all-tracks (MIDIFile-tracks parsed)) ; extract info about all tracks (max 16 total)
(define bass (cadr all-tracks)) ; sample pull of a single track to parse, not general
(define piano (caddr all-tracks)) ; another track to parse, not general

;; note-specific functions

(define note-list #(c c# d d# e f f# g g# a a# b)) ; 12 notes per octave

(define notes #hasheq((c  . "HP") ; match notes to inputs
                      (c# . "LP+MP+HP")
                      (d  . "LP+HP")
                      (d# . "MP+HP")
                      (e  . "LP+MP")
                      (f  . "LP")
                      (f# . "MP")
                      (g  . "HK")
                      (g# . "MK+HK")
                      (a  . "LK+MK")
                      (a# . "LK")
                      (b  . "MK")))

(struct note (pitch time)) ; convert ChannelMessages with 'note-on to these

(define (note-char note) ; find note's corresponding letter in octave
  (vector-ref note-list (remainder (note-pitch note) 12)))

(define (note-name note) ; combine the note and octave
  (~a (note-char note) (note-octave note)))

(define (note-octave note) ; specific octave a pitch is in
  (quotient (note-pitch note) 12))

(define (bb-note note) ; given note, return SkullGirls inputs to perform it
  (let* ([pitch (note-pitch note)]
         [bb-input (hash-ref notes (vector-ref note-list (remainder pitch 12)))]) ; find cleaner way?
    (cond [(> pitch high-threshold) ; jumping
           (string-append high-prefix bb-input)]
          [(< pitch low-threshold) ; crouching
           (string-append low-prefix bb-input)]
          [else bb-input]))) ; standing

;; track-specific functions

(define (timestamp time) ; get min:sec.ms from note's "
  (let ([milliseconds (remainder time 1000)]
        [seconds (remainder (quotient time 1000) 60)]
        [minutes (quotient time 60000)])
    (~a minutes #\: ; can we macro these internal ~a thingies?
        (~a seconds #:min-width 2 #:align 'right #:left-pad-string "0") #\.
        (~a milliseconds #:min-width 3 #:align 'right #:left-pad-string "0"))))

(define (track-instrument track) ; get instrument from track. Fails for (car all-tracks)
  (bytes->string/utf-8
   (cadr (MetaMessage-content (cadar track))))) ; convert bytestring return value

(define (track-note-pitches track) ; get all note-on messages from a track
  (let ([notes (filter (lambda (msg)
                         (and (ChannelMessage? (cadr msg))
                              (eq? (ChannelMessage-kind (cadr msg)) 'note-on)))
                       track)])
    (map (lambda (n)
           (car (ChannelMessage-operands (cadr n)))) notes)))

(define (rising-note-edges track) ; get all note-on messages from a track
  (filter rising-note-edge?
          track))

(define (trackevent->note trackevent) ; assumes msg is a note-on
  (note (car (ChannelMessage-operands (cadr trackevent)))
        (car trackevent)))

(define (rising-note-edge? track-event) ; check if a track event is a note
  (and (ChannelMessage? (cadr track-event))
       (eq? (ChannelMessage-kind (cadr track-event)) 'note-on)))

(define (show-track-inputs track) ; put it all together
  (let ([notes (map trackevent->note
                    (filter rising-note-edge? bass))])
    (for-each (lambda (n)
                (display (~a #:separator " "
                             (~a #:min-width 3 (note-name n))
                             (~a #:min-width 7 (bb-note n))
                             (timestamp (note-time n))
                             "\n")))
              notes)))

;; running code here

(show-track-inputs bass) ; run it!
