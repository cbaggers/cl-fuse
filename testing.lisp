(in-package #:fuse)
(in-readtable :fn.reader)

;; Requirements for all JS code
(def-js-requires ("FuseJS/Observable" observable))


;; make a component
(def-ux-component big-header () ()
  (text (:font-size 54)))



(def-js-var some-text "This is still working")
(def-js-var some-var (observable 30))

(def-fuse-app ()
  (stack-panel ()
    (text (:color :blue) "Hi there. This is a dumb project")
    (text (:color :red) "test")
    (text (:color :red :value some-text))
    (slider (:value some-var :minimum 0 :maximum 100))
    (text (:color :Red :value some-var))
    (my-button (:bg-color "#ff0"))
    (big-header () "Hi!")))


(def-js-func some-func (x y)
  (+ x y))


(def-ux-component my-button ((bg-color "#f0f" float4)) ()
  (panel ()
    (text (:alignment :center) "SUBMIT")
    (rectangle (:layer :background :color bg-color :corner-radius 5))))
