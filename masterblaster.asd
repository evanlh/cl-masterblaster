(require 'asdf)
(asdf:defsystem "masterblaster"
  :description "masterblaster: a MIDI sequencer with tracker influences"
  :version "0.0.1"
  :author "Evan Lawrence-Hurt <evanlh@gmail.com>"
  :licence "MIT"
  :depends-on ("sdl2" "cl-portaudio" "portmidi" "series" "bordeaux-threads" "alexandria")
  :components ((:file "package")
               (:file "font")
               (:file "arrays")
               (:file "midi")
               (:file "euclidean" :depends-on ("arrays"))
               (:file "notes")
               (:file "track" :depends-on ("arrays" "euclidean" "notes"))
               (:file "sound" :depends-on ("track"))
               (:file "main" :depends-on ("font" "sound" "midi"))))
