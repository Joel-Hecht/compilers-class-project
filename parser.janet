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

(def charIsNumber 
	(peg/compile
		'(range "09")
	)
)

(def charIsLetter
	(peg/compile 
		'(range "az" "AZ")
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
			(do
				(put vals 0 :op)
				(case c

					"+" :+
					"-" :-
					"*" :*
					"/" :/
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
		{:type (get vals 0) }
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

(defn advanceToken [remaining needsTypes prev ] 
	(if (= remaining "")
		@[]	
		(do
			#advnace spaces	
			(def thischar (string/slice remaining 0 1) )
			(def newremaining (string/slice remaining 1) )
			(if (= thischar " ")
				(do 
					(def toks (advanceToken newremaining [] false) )
					(if (empty? needsTypes )
						toks
						(array/insert toks 0 prev)	
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
								(array/insert (getNextRecursion) 0 prev) #insert prev in addition to whatever else we are inserting here
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
	(if (empty? args)
		@[]
		(array/insert 
			(tokenize (rest args ) )
			0
			(advanceToken (first args) [] false ) 
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
