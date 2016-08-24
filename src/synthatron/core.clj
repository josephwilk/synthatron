(ns synthatron.core
  (:use [overtone.live]))

(def waves [sin-osc
            lf-saw
            lf-cub
            lf-par
            pulse
            square
            lf-tri])

(defn synth-builder []
  (let [n :blah
        wave (choose waves)]
    (do
      (definst n [note 40]
        (let [freq (midicps note)
              src (wave freq)
              env (env-gen (env-perc))]
          (* src env))))))

(do
  (def new-synth (synth-builder))
  (new-synth :note (note :F#3)))
