#!/usr/local/bin/janet

(use ./utils)

(defn specifyOperators [t]
	#reinitialize struct, poor practice, blah blah blah whatever
	(if (value-in? (t :tok) [:+ :- :* :/ ] ) 
		{:tok (t :tok) :op true}	
		t
	)
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
			"\n" :nl
			(do
				(put vals 0 :op)
				(case c

					"+" :+
					"-" :-
					"*" :*
					"/" :/
					"=" :=
					(do
						(put vals 0 :num)
						(if (peg/match charIsNumber c)
							c
							(do
								(if (peg/match charIsLetter c)
									(do
										(put vals 0 :id)
										c
									)
									(do
										(put vals 0 :INVALID)
										:INVALID
									)
								)
							)
						)						
					)
				)
			)
		)
	)
	(if (= (get vals 0) :sym)
		{:type (get vals 1) }
		{:type (get vals 0) :tok (get vals 1)}
	)
)

(defn typesNeededFor [t]
	(if (= t :num)
		[:num]
		(if (= t :id)
			[:num :id]
			[]
		)
	)
)

(defn insertMultiCharToken [arr tok]
	(def newtok
		{ :type
			(case (tok :tok)
				"if" :iff
				"ifonly" :ifonly
			  "while" :w
				"return" :ret
				"print" :print
				"this" :th
				(tok :type)
			)	
			:tok (tok :tok) #messy but I'm just gonna leave in the token value even if it is identical to the type

		}
	)
	(array/insert arr 0 newtok)	
)

(defn advanceToken [remaining needsTypes prev ] 
	(if (= remaining "")
		(if prev
			@[prev]
			@[]
		)	
		(do
			#advnace spaces	
			(def thischar (string/slice remaining 0 1) )
			(def newremaining (string/slice remaining 1) )
			(if (= thischar " ")
				(do 
					(def toks (advanceToken newremaining [] false) )
					(if (empty? needsTypes )
						toks
						(insertMultiCharToken toks prev)
					) #if ongoing token
				) #if whitespace
				(do
					(def tok (charToKeyword thischar ))
					(if (value-in? (tok :type) needsTypes)
						(advanceToken newremaining needsTypes {:type (prev :type) :tok (string (prev :tok) (tok :tok)) } )
						(do
							(defn getNextRecursion [] 
								(do
									(if (or (= (tok :type) :id) (= (tok :type) :num))
										(advanceToken newremaining (typesNeededFor (tok :type)) tok)
										(array/insert (advanceToken newremaining [] false) 0 tok)
									)# if num or id
								)
							) #defn
							(if (empty? needsTypes)
								(getNextRecursion)
								(insertMultiCharToken (getNextRecursion) prev )
							)#if no types needed
						)
					)	#if in needstypes
				)
			) #if space
		)
	)	
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
	(def joined (joinArgs args) )
	(advanceToken joined [] false ) 
)

(defn main [& args] 
	#remove first arg, since this is the name of the program
	(def tokens (tokenize (rest args) ) )
	(destructureLevels tokens 2 print)	
	#(case (get args 0)
	#	"tokenize" (tokenize (array/slice args 1 ) )
	#	"parseExpr" (parseExpr (array/slice (args 1 ) ) )
	#	(print (string "Unsupported Subcommand " (get args 0) ) )
	#)

)
