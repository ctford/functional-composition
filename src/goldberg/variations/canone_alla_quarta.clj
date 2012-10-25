;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functional Composition                          ;;
;; by Chris Ford - @ctford                         ;;
;; (ThoughtWorks)                                  ;;
;;                                                 ;;
;; http://github.com/ctford/functional-composition ;;
;; http://github.com/ctford/goldberg               ;;
;; http://github.com/overtone/overtone             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns goldberg.variations.canone-alla-quarta
  (:use [overtone.live
         :only [definst sin-osc mix at now line stop
                FREE env-gen perc detect-silence]]
        [quil.core :exclude [scale line]]))











;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sine waves                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(definst tone [frequency 440] (sin-osc frequency))
(definst doubletone [freq1 440 freq2 440]
  (+
    (sin-osc freq1)
    (sin-osc freq2)))

(definst beep [frequency 440 duration 1]
  (let [envelope (line 1 0 duration :action FREE)]
          (* envelope (sin-osc frequency))))

;(tone 300)
;(doubletone 300 300)
;(beep)
;(stop)

; harmonics
(definst bell [frequency 440 duration 10
               h0 1 h1 0.6 h2 0.4 h3 0.25 h4 0.2 h5 0.15]
  (let [harmonics ; [ 1  2  3  4   5   6  ]
                  [ 1  2  3  4.2 5.4 6.8] ; more realistic timbre
        proportions [h0 h1 h2 h3 h4 h5]
        proportional-partial
         (fn [harmonic proportion]
           (let [envelope (env-gen (perc 0.01 (* proportion duration)))
                 overtone (* harmonic frequency)]
             (* 1/2 proportion envelope (sin-osc overtone))))
        partials (map proportional-partial harmonics proportions)
        whole (mix partials)]
    (detect-silence whole :action FREE)
    whole))

;(bell)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Psycho-acoustics                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(bell 600 10.0)
;(bell 500 10.0 0.0)
;(bell 400 10.0 0.0 0.0)


















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Equal temperament                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn midi->hz [pitch]
    (*
      8.1757989156 ; midi zero
      (java.lang.Math/pow 2 (/ note 12))))

;(midi->hz 69)

(defn ding! [midi] (bell (midi->hz midi) 3))

(defn play! [notes] 
  (doseq [{ms :time midi :pitch} notes]
    (at ms (ding! midi)))
  notes)

(defn where [k f notes] (->> notes (map #(update-in % [k] f)))) 
(defn even-melody [pitches]
  (let [times (reductions + (repeat 400))
        notes (map #(zipmap [:time :pitch] [%1 %2]) times pitches)]
    (->> notes (where :time #(+ (now) %)) play!)))

;(even-melody (range 70 81))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scale                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defs [names values]
  `(do ~@(map
           (fn [name value] `(def ~name ~value))
           names (eval values))))

(defn start-from [base] (partial + base))
(defn sum-n [series n] (reduce + (take n series)))

(defn scale [intervals]
  (fn [degree]
    (if-not (neg? degree)
      (sum-n (cycle intervals) degree)
      ((comp - (scale (reverse intervals)) -) degree))))

(def major (scale [2 2 1 2 2 2 1]))

(def C (start-from 60))
(defs [D E F G A B]
  (map
    (comp start-from C major inc)
    (range)))

(defs [sharp flat] [inc dec])

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Melody                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def row-row-row-your-boat
  (let [pitches
         [0 0 0 1 2
          ; Row, row, row your boat,
          2 1 2 3 4 
          ; Gently down the stream,
          7 7 7 4 4 4 2 2 2 0 0 0 
          ; (take 4 (repeat "merrily"))
          4 3 2 1 0]
          ; Life is but a dream!
        durations
         [1 1 2/3 1/3 1
          2/3 1/3 2/3 1/3 2
          1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3
          2/3 1/3 2/3 1/3 2]
        times (reductions + 0 durations)]
      (map #(zipmap [:time :pitch] [%1 %2]) times pitches)))

;row-row-row-your-boat

(defn bpm [beats] (fn [beat] (-> beat (/ beats) (* 60) (* 1000))))
;((bpm 120) 3)

;(play!
;  (map
;    (fn [[beat degree]]
;      [((comp (start-from (now)) (bpm 90)) beat) ((comp C major) degree)])
;    row-row-row-your-boat))

(defn run [[from & tos]]
  (if-let [to (first tos)]
    (let [up-or-down (if (<= from to)
                       (range from to)
                       (reverse (range (inc to) (inc from))))]
      (concat up-or-down (run tos)))
    [from]))

;(even-melody (map (comp G major)
;            (run [0 4 -1 1 0])
;            ))

(defn accumulate [series]
  (map (partial sum-n series) (range (count series))))
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
          [(repeats [[1 3/4] [12 1/4] [1 1/2] [1 1] [1 1/2]
                 [12 1/4] [1 3]])
          (runs [[4] [4] [2 -3] [-1 -2] [0] [3 5] [1] [1] [1 2]
                 [-1 1 -1] [5 0]])]
        [durations pitches] (map concat call response development)
        timings (map (partial + 1/2) (accumulate durations))]
    (map #(zipmap [:time :pitch] [%1 %2]) timings pitches)))

(def bass
  (let [triples (partial mapcat #(repeat 3 %))]
    (map #(zipmap [:time :pitch] [%1 %2]) 
       (accumulate (repeats [[21 1] [13 1/4]]))
       (concat
         (triples (runs [[-7 -10] [-12 -10]]))
         (runs [[5 0] [6 0]])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Canon                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn canon [f notes] (concat notes (f notes)))

(defn in-time [tempo notes]
  (->> notes
    (where :time tempo)
    (where :time (start-from (now)))))

(defn in-key [scale notes] (->> notes (where :pitch scale)))

; flavours of canon
(defn simple [wait notes] (->> notes (where :time (start-from wait))))
(defn interval [interval notes] (->> notes (where :pitch (start-from interval))))
(defn mirror [notes] (->> notes (where :pitch -)))
(defn crab [notes] (->> notes (where :time -)))
(def table (comp mirror crab))

; round
;(->> row-row-row-your-boat
;  (canon (partial simple 4))
;  (in-key (comp C major))
;  (in-time (bpm 90))
;  (graph! "Row row row your boat")
;  play!)

; canone alla quarta, by johann sebastian bach
(defn canone-alla-quarta [notes]
  (canon
    #(->> % (simple 3) mirror (interval -3))
    notes))

(defn graph! [title points]
  (let [
      start (now)
      most (fn [member comparison] (->> points (map #(% member)) (reduce comparison)))
      max-x (most :time max)
      min-x (most :time min)
      max-y (most :pitch max)
      min-y (most :pitch min)
      past (fn [end points] (filter #(< (:time %) end) points))
      normalise (fn [points] (->> points
        (where :time (start-from (- min-x)))
        (where :pitch (start-from (- min-y)))
        (where :time #(/ % (- max-x min-x)))
        (where :pitch #(/ % (- max-y min-y)))))]
                               
    (sketch 
      :title title
      :setup (fn []
               (smooth)
               (frame-rate 6)
               (background 200))  
      :draw  (fn []
               (stroke 100)
               (stroke-weight 5)
               (fill 50)
               (doseq [[x y] (->> points (past (now)) normalise)]
                 (ellipse (* (width) x) (- (* 2/3 (height)) (* (/ (height) 3) y)) 10 10))) 
      :size [800 600])
    points))

;(->> melody
;  canone-alla-quarta
;  (concat bass)
;  (in-key (comp G major))
;  (in-time (bpm 90))
;  (graph! "Time vs pitch")
  ;play!
;  )
