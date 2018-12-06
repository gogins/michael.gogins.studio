(require :asdf)
(asdf:load-system :nudruz)
(load "/home/mkg/csound-extended/nudruz/sources/all-in-one-orc.lisp")
(in-package :cm)

(set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc)

(defparameter csound-seq (midifile-to-seq "/home/mkg/Downloads/Pange_Lingua_More_Hispano.mid"))
(list-objects csound-seq)

;(defparameter output "dac")
(defparameter output "Pange-Lingua.wav")
(render-with-orc csound-seq all-in-one-orc :output output :channel-offset 1 :velocity-scale 140 :csd-filename "Pange-Lingua.csd")
(unless (equal output "dac")    
    (print "Post-processing...")
    (uiop:run-program '("python" "../post-process.py" "Pange-Lingua.wav") :output nil)
)
(quit)



