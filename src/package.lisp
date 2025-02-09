(defpackage #:masterblaster
  (:use #:cl
        ;; #:sdl2
        ;; #:portaudio
        ;; #:portmidi
        ;; #:series
        ;; #:bordeaux-threads
        ;; #:alexandria

        )
  (:import-from #:alexandria #:iota)
  (:import-from #:portmidi :count-devices :list-devices :get-default-output-device-id :get-default-input-device-id :close-midi :open-output :note-on :note-off :write-short-midi)
  (:export
   ;; main
   #:launch #:plot #:init-track))
