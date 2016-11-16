(in-package #:whatevs)
(in-readtable :fn.reader)

;; Requirements for all JS code
(def-js-requires ("FuseJS/Observable" observable))



(def-js-var some-var 120)

;; make a component
(debug-ux)
(def-ux-component big-header () ()
  (text (:font-size 50)))



;; make a var we use as the text below
(debug-ux)
(def-js-var some-text (observable "This is dumb"))

(debug-ux)
(def-fuse-app ()
  (stack-panel ()
    (text (:color :blue) "Hi there. This is a dumb project")
    (text (:color :red) "test")
    (text (:color :red :value some-text))
    (big-header () "Hi!")))



(debug-ux
  (def-ux-component my-comp () ()
    (panel (:color "Yellow")
      (while-pressed ()
        (scale (:factor 2 :duration 0.3 :easing :back-out))))))


(def-js-func some-func (x y)
  (+ x y))
