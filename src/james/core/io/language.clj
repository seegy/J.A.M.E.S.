(ns james.core.io.language)

(use 'clojure.pprint) ; just for this documentation
(use 'opennlp.nlp)
(use 'opennlp.treebank) ; treebank chunking, parsing and linking lives here


(def get-sentences (make-sentence-detector "resources/lang/de-sent.bin"))
(def tokenize (make-tokenizer "resources/lang/de-token.bin"))
(def pos-tag (make-pos-tagger "resources/lang/de-pos-maxent.bin"))
(def name-find (make-name-finder "resources/lang/en-ner-person.bin"))
;(def chunker (make-treebank-chunker "resources/lang/en-chunker.bin"))


(def satz "Hallo James, bist du da?")

(get-sentences satz)

;(chunker (pos-tag (tokenize satz)))

(name-find (tokenize satz))
