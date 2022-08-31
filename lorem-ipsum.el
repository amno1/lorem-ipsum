;;; lorem-ipsum.el   Elisp version of lorem-ipsum.lisp  -*- lexical-binding: t; -*-

(require 'cl-lib)

(defgroup lorem-ipsum
  :prefix "lorem-ipsum-")

(defvar lorem-ipsum--prologue
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit")

(defvar lorem-ipsum--table
  ["do" "eiusmod" "incididunt" "labore" "dolore" "aliqua" "erat" "bibendum"
    "venenatis" "condimentum" "nisi" "natoque" "penatibus" "magnis" "dis"
    "parturient" "montes" "aenean" "nam" "ante" "metus" "tempor" "nullam"
    "suscipit" "justo" "posuere" "eleifend" "vulputate" "luctus" "accumsan"
    "lacus" "dictum" "fusce" "euismod" "placerat" "elementum" "pharetra"
    "maecenas" "ultricies" "suspendisse" "potenti" "pulvinar" "gravida"
    "hendrerit" "interdum" "laoreet" "magna" "urna" "porttitor" "rhoncus"
    "dolor" "non" "praesent" "nec" "pretium" "fringilla" "est" "nulla"
    "facilisi" "etiam" "dignissim" "tincidunt" "lobortis" "vivamus" "augue"
    "velit" "ligula" "ullamcorper" "malesuada" "a" "duis" "diam" "quam" "mattis"
    "libero" "ornare" "arcu" "elit" "pellentesque" "habitant" "tristique"
    "senectus" "netus" "ut" "sem" "eget" "viverra" "integer" "feugiat"
    "scelerisque" "varius" "mollis" "consectetur" "lorem" "donec" "sapien"
    "molestie" "semper" "auctor" "neque" "vitae" "tempus" "nisl" "ipsum"
    "faucibus" "cras" "adipiscing" "enim" "eu" "turpis" "volutpat" "consequat"
    "nunc" "congue" "leo" "vel" "porta" "fermentum" "et" "sollicitudin" "ac"
    "orci" "phasellus" "egestas" "tellus" "rutrum" "mauris" "amet" "massa"
    "nibh" "tortor" "id" "aliquet" "lectus" "proin" "aliquam" "vestibulum"
    "blandit" "risus" "at" "ultrices" "mi" "facilisis" "sed" "morbi" "quis"
    "commodo" "odio" "cursus" "in" "hac" "habitasse" "platea" "dictumst"
    "quisque" "sagittis" "purus" "sit"]
  "A table of words to use wehn producing the text.")

(defun lorem-ipsum-word ()
  "Pick a random word from `lorem-ipsum--table'"
  (aref lorem-ipsum--table (random (length lorem-ipsum--table))))

(defun lorem-ipsum-words (n)
  (let (words)
    (dotimes (_i n) (push (lorem-ipsum-word) words))
    words))

(defun lorem-ipsum--paragraph (&optional word-count prologue)
  (with-output-to-string
    (with-current-buffer standard-output
      (let ((words-remaining (or word-count 50))
            (words-in-sentence (+ 2 (random 8)))
            (capitalize t)
            (prologue (or prologue t)))
        (cl-tagbody
         :start
         (cond (prologue
                (princ lorem-ipsum--prologue)
                (cl-decf words-in-sentence 3)
                (setf capitalize nil))
               (t
                (go :output-word)))
         :end-paragraph
         (when (= 0 words-remaining)
           (princ ".")
           (go :end))
         :end-sentence
         (cond ((= 0 words-in-sentence)
                (princ ". ")
                (setf capitalize t
                      words-in-sentence (+ 2 (random 8))))
               (t
                (if (= 0 (random 8))
                    (princ ", ")
                  (princ " "))))
         :output-word
         (princ (if capitalize (capitalize (lorem-ipsum-word)) (lorem-ipsum-word)))
         (setf capitalize nil)
         (cl-decf words-remaining)
         (cl-decf words-in-sentence)
         :loop
         (go :end-paragraph)
         :end)))))

(defun lorem-ipsum-paragraphs (n &optional word-count prologue)
  (let ((text (lorem-ipsum--paragraph word-count prologue)))
    (dotimes (_i (1- n))
      (setq text (concat text "\n\n" (lorem-ipsum--paragraph word-count))))
    text))

(defun lorem-ipsum-paragraph ()
  "Insert one paragraph of lorem ipsum words at point."
  (interactive)
  (let ((beg (point)))
    (insert (lorem-ipsum-paragraphs 1 50 t))
    (indent-region beg (point))
    (fill-region beg (point) t)))

(provide 'lorem-ipsum)
