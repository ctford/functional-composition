;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functional Composition                       ;;
;;                                              ;;
;; Chris Ford (ThoughtWorks)                    ;;
;; http://github.com/ctford/goldberg            ;;
;; http://github.com/overtone/overtone          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns goldberg.variations.canone-alla-quarta
  (:use
    [clojure.repl]
    [overtone.live :exclude [midi->hz sharp flat scale run pitch shift]]))













;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sine waves                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(definst tone [frequency 440] (sin-osc frequency))
(definst beep [frequency 440 duration 1.0]
  (let [envelope (env-gen (line 1.0 0 duration FREE))]
          (* envelope (sin-osc frequency))))

;(tone)
;(beep)

; harmonics
(definst bell [frequency 300 duration 10.0 h0 1.0 h1 0.5 h2 0.4 h3 0.25 h4 0.20 h5 0.125]
  (let [harmonic-decay [h0 h1 h2 h3 h4 h5]
        proportional-partial
         (fn [harmonic proportion]
           (let [envelope (env-gen (perc 0.01 (* duration proportion)))
                 overtone (* (inc harmonic) frequency)]
             (* envelope proportion (sin-osc overtone))))
        partials (map-indexed proportional-partial harmonic-decay)
        whole (mix partials)]
    (detect-silence whole :action FREE)
    whole))

;(bell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Psycho-acoustics                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(bell 600 20.0)
;(bell 500 20.0 0.0)
;(bell 400 20.0 0.0 0.0)
;(stop)

















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Equal temperament                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn midi->hz [note]
    (*
      8.1757989156 ; midi zero
      (java.lang.Math/pow 2.0 (/ note 12.0))))

;(midi->hz 69)

(defn play [notes] 
  (let [start (now)
        play-at (fn [[ms midi]]
                  (at
                    (+ ms start)
                    (bell (midi->hz midi) 3.5 0.1 0.6 0.0 0.1 0.0 0.0)))]
    (->> notes (map play-at) dorun)
    notes))

(defn even-melody [pitches]
  (let [times (reductions + (repeat 400))
        notes (map vector times pitches)]
    (play notes)))

;(even-melody (range 70 81))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scale                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defs [names values]
  `(do ~@(map
           (fn [name value] `(def ~name ~value))
           names (eval values))))

(defn start-from [base] (partial + base))
(defs [sharp flat] [inc dec])
(defn sum-n [series n] (reduce + (take n series)))

(defn scale [intervals]
  #(if-not (neg? %)
     (sum-n (cycle intervals) %)
     ((comp - (scale (reverse intervals)) -) %)))

(def major (scale [2 2 1 2 2 2 1]))

(defs [C D E F G A B]
  (map
    (comp start-from (start-from 60) major)
    (range)))

; alternative scales
(def blues (scale [3 2 1 1 3 2]))
(def pentatonic (scale [3 2 2 3 2]))
(def chromatic (scale [1]))

;(even-melody
;  (map
;    (comp C major)
;    (range 15))
;)

; modes
(defn mode [scale n]
  (comp
    #(- % (scale n))
    scale
    (start-from n)))

(defs
  [ionian dorian phrygian lydian mixolydian aeolian locrian]
  (map (partial mode major) (range)))

(def minor aeolian)

;(even-melody
;  (let [_ -100]
;    (map (comp D major) [0 1 2 0, 0 1 2 0, 2 3 4 _, 2 3 4 _]))
;)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Melody                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def row-row-row-your-boat
  (let [named-pitches
         [:C4 :C4 :C4 :D4 :E4
          ; Row, row, row your boat,
          :E4 :D4 :E4 :F4 :G4
          ; Gently down the stream,
          :C5 :C5 :C5 :G4 :G4 :G4 :E4 :E4 :E4 :C4 :C4 :C4
          ; (take 4 (repeat "merrily"))
          :G4 :F4 :E4 :D4 :C4]
          ; Life is but a dream!
        durations
         [1 1 2/3 1/3 1
          2/3 1/3 2/3 1/3 2
          1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3
          2/3 1/3 2/3 1/3 2]
        midi-pitches (map note named-pitches)
        times (reductions + 0 durations)]
      (map vector times midi-pitches)))

(defn bpm [beats] (fn [beat] (-> beat (/ beats) (* 60) (* 1000))))
;((bpm 120) 3)

;row-row-row-your-boat
;(play
;  (map
;    (fn [[beat midi]] [((bpm 90) beat) midi])
;    row-row-row-your-boat)
;)

(defn run [[from & tos]]
  (if-let [to (first tos)]
    (let [up-or-down (if (<= from to)
                       (range from to)
                       (reverse (range (inc to) (inc from))))]
      (concat up-or-down (run tos)))
    [from]))

;(even-melody (map (comp G major)
;            (run [0 4 -1 0 1 0])
;            ))

(defn accumulate [series] (map (partial sum-n series) (range (count series))))
(def repeats (partial mapcat #(apply repeat %)))
(def runs (partial mapcat run))

(def melody 
  (let [call
          [(repeats [[2 1/4] [1 1/2] [14 1/4] [1 3/2]])
          (runs [[0 -1 3 0] [4] [1 8]])]
        response
          [(repeats [[10 1/4] [1 1/2] [2 1/4] [1 9/4]])
          (runs [[7 -1 0] [0 -3]])]
        development
          [(repeats [[1 3/4] [12 1/4] [1 1/2] [1 1] [1 1/2] [12 1/4] [1 3]])
          (runs [[4] [4] [2 -3] [-1 -2] [0] [3 5] [1] [1] [1 2] [-1 1 -1] [5 0]])]
        [durations pitches] (map concat call response development)
        timings (map (partial + 1/2) (accumulate durations))]
    (map vector timings pitches)))

(def bass
  (let [triples (partial mapcat #(repeat 3 %))]
    (map vector
       (accumulate (repeats [[21 1] [13 1/4]]))
       (concat (triples (runs [[-7 -10] [-12 -10]])) (run [5 -7])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Canon                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn canon [f] (fn [notes] (concat notes (f notes))))

(defs [timing pitch] [0 1])
(defn skew [k f] (fn [points] (map #(update-in % [k] f) points))) 
(defn shift [point] (fn [points] (map #(->> % (map + point) vec) points)))
(defn in-time [tempo] (skew timing tempo))
(defn in-key [scale] (skew pitch scale))

; flavours of canon
(defn simple [wait] (shift [wait 0]))
(defn interval [interval] (shift [0 interval]))
(def mirror (skew pitch -))
(def crab (skew timing -))
(def table (comp mirror crab))

(defn => [value & fs] (reduce #(%2 %1) value fs))

; round
;(=> row-row-row-your-boat
;    (in-time (bpm 90))
;    (canon (simple 4))
;    play)

; canone alla quarta, by johann sebastian bach
(def canone-alla-quarta (canon (comp (interval -3) mirror (simple 3))))

;(=> melody
;    canone-alla-quarta
;    #(concat bass %)
;    (in-key (comp G major))
;    (in-time (bpm 90))
;    play)
