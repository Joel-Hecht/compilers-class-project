#!/usr/local/bin/janet

(import ./tokenizer)

(defn errorAndExit [msg]
	(print (string "comp: " msg) )
	(exit 1)
)	

#where tokens is output of (tokenizer args)
(defn parseExp [tokens]
	

)

(defn main [& args] 
			
)
