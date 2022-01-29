#lang racket/base

;; Travis Hopkins 2022-10-28

(require midi-readwrite) ; library for reading MIDI file itself
(require racket/runtime-path) ; find way to get rid of this?
(require racket/format) ; find simpler version of this?

(define note-list #(c c# d d# e f f# g g# a a# b)) ; 12 notes total

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

(define min-note 0) ; lowest possible note value
(define max-note 127) ; highest possible note value
(define high-threshold 72) ; higher octave
(define low-threshold 48) ; lower octave
(define high-prefix "j.") ; holding up. Try 8?
(define low-prefix "c.") ; holding down. Try 2?
(define show-octave #f) ; if #t, C5 instead of C for instance
(define include-milliseconds #f) ; eg. 2:38.5023 instead of 2:38

(define-runtime-path midi-file "c:\\Users\\th8795\\Documents\\MIDI_sample.mid") ; input

(define parsed (midi-file-parse (build-path midi-file))) ; parse directly, note-for-note

(define song (MIDIFile->notelist parsed))
;(display song) ; show list of note structs

#;(display (note-pitch (car song))) ; get pitch of first note

(define (octave pitch) ; specific octave a pitch is in
  (quotient pitch 12))

(define (timestamp note) ; return the time a note begins
  (let* ([time (note-time note)]
         [milliseconds (remainder time 1000)]
         [seconds (remainder (quotient time 1000) 60)]
         [minutes (quotient time 60000)])
    (~a minutes #\: ; can we macro these internal ~a thingies?
        (~a seconds #:min-width 2 #:align 'right #:left-pad-string "0") #\.
        (~a milliseconds #:min-width 3 #:align 'right #:left-pad-string "0") "\n")))

#;(for-each (lambda (i) ; show all timestamps as a test
            (display (timestamp i)))
          song)

(define all-tracks (MIDIFile-tracks parsed)) ; extract info about all tracks (max 16 total)

(define (track-instrument track) ; get instrument from track. Fails for (car all-tracks)
  (bytes->string/utf-8 (cadr (MetaMessage-content (cadar track))))) ; bytestring return value

(define (note-char note) ; match name of pitch to note number (0-127)
  (vector-ref note-list (remainder (note-pitch note) 12)))

(define (note-name note) ; combine the note and octave
  (~a (note-char note) (octave (note-pitch note))))

#;(for-each ; proof of work so far goes here
 (lambda (note)
   (display (~a (~a (note-name note) #:min-width 3) " " (timestamp note)))) ; there must be a better way of doing this...
 song)

(define bass (cadr all-tracks)) ; sample pull of a single track to parse
(define piano (caddr all-tracks)) ; another track to parse

#;(filter (lambda (msg) ; get all note-on messages from a track
            (and (ChannelMessage? (cadr msg))
                 (eq? (ChannelMessage-kind (cadr msg)) 'note-on)))
          bass)

(define (track-note-pitches track)
  (let ([notes (filter (lambda (msg) ; get all note-on messages from a track
            (and (ChannelMessage? (cadr msg))
                 (eq? (ChannelMessage-kind (cadr msg)) 'note-on)))
          track)])
    (map (lambda (n)
           (car (ChannelMessage-operands (cadr n)))) notes)))

(map (lambda (n) ; show all notes from the bass track
       (let ([bb-input (hash-ref notes (vector-ref note-list (remainder n 12)))])
         (cond
           [(>= n high-threshold)
            (string-append high-prefix bb-input)]
           [(<= n low-threshold)
            (string-append low-prefix bb-input)]
           [else bb-input])))
     (track-note-pitches bass))
