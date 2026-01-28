#!/usr/local/bin/janet

(use judge)

(import ./parsers/statement)
(import ./parsers/expr)
(import ./parsers/class)
#(use ./parsers)
(import ./tokenizer)

#(use ./parser/parserTools)
(use ./utils)


(defn main [ & args ]
	(def t (tokenizer/make-tokenizer-from-args (rest args)))
	#currently only parses first symbol
	(pp (expr/parseExp t))
)
