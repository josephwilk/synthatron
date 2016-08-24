(ns synthatron.core
  (:use [overtone.live]))

(def waves [sin-osc
            pulse
            square
            ;;ouch  blip
            saw

            lf-saw
            lf-tri
            lf-par
            lf-cub
            lf-tri
            lf-gauss
            lf-pulse])

(def envs [perc])

(def effect [g-verb
             pitch-shift
             comb-n
             pluck ])

(def delay-ugens
  [delay1 delay-n delay-l delay-c comb-n comb-l
   comb-c allpass-n allpass-l allpass-c])

(defn synth-builder []
  (let [n :blah
        wave-count (+ 1 (rand-int (dec (count waves))))
        waves (repeatedly wave-count #(choose waves))
        envelope (choose envs)
        effect (choose delay-ugens)

        attack (rand)
        release (rand)]
    (println (map :name waves))
    (do
      (definst n [note 40]
        (let [freq (midicps note)
              all-waves (map (fn [w] (w freq)) waves)
              src (mix all-waves)
              src (effect src)
              env (env-gen (envelope attack release) :action FREE)]
          (* src env))))))

(do
  (def new-synth (synth-builder))
  (new-synth :note (note :F#3)))
;;(kill n)
