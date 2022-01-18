(import ./utf8-decode :as u)

(defn main
  [& args]
  (def infile
    (get args 1))
  (def outdir
    (get args 2))
  (unless infile
    (eprint "please specify file name for list of words")
    (os/exit 1))
  (unless (= :directory
             (os/stat outdir :mode))
    (eprint "please specify existing output directory for results")
    (os/exit 1))
  #
  (unless (u/process-with-constraint infile outdir u/katakana?)
    (eprintf "failed to process: %p" infile)
    (os/exit 1)))
