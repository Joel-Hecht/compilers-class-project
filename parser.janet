#!/usr/local/bin/janet


(defn rest [l] (array/slice l 1 ))

#refactor with match first & rest
(defn value-in? [val list]
	(if (empty? list) false
		(if (= val (first list )) 
			true
			(value-in? val (rest list ) )
		)
	)
)

(defn specifyOperators [t]
	#reinitialize struct, poor practice, blah blah blah whatever
	(if (value-in? (t :tok) [:+ :- :* :/ ] ) 
		{:tok (t :tok) :op true}	
		t
	)
)

(defn extractNumberToken [this after]
	()	
)

(defn charToKeyword [c] 
	(def vals @[])
	(put vals 0 :sym)
	(put vals 1 
		(case c
			"(" :lp
			")" :rp
			"{" :lb
			"}" :rb
			":" :colon
			"!" :not
			"@" :at
			"^" :caret
			"&" :amp
			"." :dot
			"," :comma
			(do
				(put vals 0 :op)
				(case c

					"+" :+
					"-" :-
					"*" :*
					"/" :/
					#(do
					#	(put vals 0 :num)
					#	(case c
					#
					#
					#	)
					#)
					[:INVALID nil]
				)
			)
		)
	)
	{:type (get vals 0) :tok (get vals 1)}
)

#tokenize a string into an array of tokens
(defn tokenizeSingleString [s]
	(if (= s "")
		@[]	
		(array/insert 
			(tokenizeSingleString (string/slice s 1))
			0
			(charToKeyword (string/slice s 0 1) ) 
		)	
	)
)

#tokenize all argument strings
(defn tokenize [ args ]
	(if (empty? args)
		@[]
		(array/insert 
			(tokenize (rest args ) )
			0
			(tokenizeSingleString (first args))
		)
	)
)

(defn destructureLevels [ s n f ]
	(if (= 0 n)
		(f s)
		(each i s
			( destructureLevels i (- n 1) f)
		)
	)
)

(defn main [& args] 
	#remove first arg, since this is the name of the program
	(def tokens (tokenize (rest args)))
	(destructureLevels tokens 3 print)	
	#(case (get args 0)
	#	"tokenize" (tokenize (array/slice args 1 ) )
	#	"parseExpr" (parseExpr (array/slice (args 1 ) ) )
	#	(print (string "Unsupported Subcommand " (get args 0) ) )
	#)

)
