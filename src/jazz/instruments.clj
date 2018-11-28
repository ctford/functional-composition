(ns jazz.instruments
  (:use [overtone.core]
        [overtone.samples.piano :only [index-buffer]]))

(definst hat []
  (let [buffer (load-sample (freesound-path 802))]
    (play-buf 1 buffer :action FREE)))

(definst piano
  [note 60 level 1 rate 1 loop? 0 duration 8
   attack 0 decay 1 sustain 1 release 0.1 curve -4 gate 1]
  (let [buf (index:kr (:id index-buffer) note)
        env (env-gen (adsr attack decay sustain release level curve)
                     (line:kr 1 0 duration)
                     :action FREE)]
    (* env (scaled-play-buf 2 buf :rate rate :level level :loop loop? :action FREE))))
