#!/usr/local/bin/janet
# parse source args/string into tokens
# implemented as a generator

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

(defn insertMultiCharToken [tok ]
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
	(yield newtok)
)

(defn advanceToken [remaining needsTypes prev ] 
	(if (= remaining "")
		(do
			(when prev
				(yield prev)
			)
			(yield {:type :eof})
		)
		(do
			#advnace spaces	
			(def thischar (string/slice remaining 0 1) )
			(def newremaining (string/slice remaining 1) )
			(if (= thischar " ")
				(do 
					(when (not (empty? needsTypes ))
						(insertMultiCharToken prev )
					) #if ongoing token
					(advanceToken newremaining [] false ) 
				) #if whitespace
				(do
					(def tok (charToKeyword thischar ))
					(if (value-in? (tok :type) needsTypes)
						(advanceToken newremaining needsTypes {:type (prev :type) :tok (string (prev :tok) (tok :tok)) }  )
						(do
							(defn getNextRecursion [] 
								(do
									(if (or (= (tok :type) :id) (= (tok :type) :num))
										(advanceToken newremaining (typesNeededFor (tok :type)) tok )
										(do
											(yield tok )
											(advanceToken newremaining [] false )
										)
									)# if num or id
								)
							) #defn
							(when (not (empty? needsTypes))
								(insertMultiCharToken prev )
							) #if has needed types
							(getNextRecursion)
						)
					)	#if in needstypes
				)
			) #if space
		)
	)	
)

(defn getNextToken [f]
	(:getNext f)
)

(defn peekNextToken [f]
	(:peek f)
)

(defn make-tokenizer [str]
	(make-fiber-with-cache (fiber/new (fn [] (advanceToken str [] false ))))

)

(defn make-tokenizer-from-args [args]	
	(def str (joinArgs args))
	(make-tokenizer str)
)

(defn print-all-tokens [t]
	(var k {:type :placeholder})
	(forever 
		(set k (getNextToken t))
		(prin "{ ")
		(each tok k
			(prin (string tok ", "))
		)
		(print "}")
		(when (= (k :type) :eof)
			(break)
		)
	)
)

(defn main [& args] 
	#remove arg, since that is the name of the program
	(def t (make-tokenizer-from-args (rest args)))
	(print-all-tokens t)
)
