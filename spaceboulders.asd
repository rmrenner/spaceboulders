(defsystem :spaceboulders
  :name "spaceboulders"
  :description "An Asteroids clone"
  :license "MIT"
  :depends-on (:lispbuilder-sdl
               :lispbuilder-sdl-gfx
               :lispbuilder-sdl-mixer
               :cl-opengl)
  :components ((:file "spaceboulders")))
