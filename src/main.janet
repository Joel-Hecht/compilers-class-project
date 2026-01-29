#!/usr/local/bin/janet

(import ./tokenizer)
(import ./parser)
(import ./CFG)
(use ./utils)

#takes in program as a string
(defn doMain [program]
	(def AST (parser/parseProgram program))
	(CFG/convertFromAST AST)
)

(defn main [& args]
	(doMain (joinArgs (rest args)))
)
