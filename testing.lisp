(in-package #:whatevs)
(in-readtable :fn.reader)

;; Requirements for all JS code
(def-js-requires ("FuseJS/Observable" observable))


;; make a component
(def-ux-component big-header () ()
  (text (:font-size 50)))



(def-js-var some-text "This is working")
(def-js-var some-var (observable 120))

(def-fuse-app ()
  (stack-panel ()
    (text (:color :blue) "Hi there. This is a dumb project")
    (text (:color :red) "test")
    (text (:color :red :value some-text))
    (slider (:value some-var :minimum 0 :maximum 100))
    (text (:color :blue :value some-var))
    (big-header () "Hi!")))


(def-js-func some-func (x y)
  (+ x y))
