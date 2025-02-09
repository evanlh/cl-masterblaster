(defsystem "masterblaster"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:sdl2 (:feature :cl-portaudio :portaudio) :portmidi :series :bordeaux-threads :alexandria)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "font")
                 (:file "arrays")
                 (:file "midi")
                 (:file "euclidean" :depends-on ("arrays"))
                 (:file "notes")
                 (:file "track" :depends-on ("arrays" "euclidean" "notes"))
                 (:file "sound" :depends-on ("track"))
                 (:file "main" :depends-on ("font" "sound" "midi")))))
  :description ""
  :entry-point "masterblaster::init-track"
  :in-order-to ((test-op (test-op "masterblaster/tests"))))

(defsystem "masterblaster/tests"
  :author ""
  :license ""
  :depends-on ("masterblaster"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for masterblaster"
  :perform (test-op (op c) (symbol-call :rove :run c)))
