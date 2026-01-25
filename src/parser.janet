#!/usr/local/bin/janet

(use judge)

(import ./tokenizer)
(import ./expr)
(use ./utils)


(defn main [ & args ]
	(def t (tokenizer/make-tokenizer-from-args (rest args)))
	#currently only parses first symbol
	(pp (expr/parseExp t))
)
