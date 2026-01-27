#!/usr/local/bin/janet

(use ./utils)
(import ./tokenizer)

(defn nextt [t]
	(tokenizer/getNextToken t)
)

(defn peekt [t]
	(tokenizer/peekNextToken t)
)

(defn peekcheck [t eq]
	(= ((peekt t) :type) eq)
)

(defn assertType [x exp &opt specifier]
	(var errexp specifier)
	(if (nil? errexp)
		(set errexp exp)
	)
	(when (not= (x :type) exp)
		(wantedGotErr errexp (x :type))
	)
)

#comma list, terminating when we get token @term instead of comma, and
#adding parseTermFunction t to @return every time we pass a comma
#DOES NOT ACTUAL CONSUME TOKEN of type term
(defn parseCommaList [t term parseTermFunction]
	(def l @[])	
	(forever
		(if (not= ((peekt t) :type) term)
			(do
				#add to args
				(array/push l (parseTermFunction t))
				(if (peekcheck t :comma)
					(nextt t) #throw away comma
					#if its not a comma, it had better be a terminator
					(assertType (peekt t) term)
				)
			) #~do
			(break)
		) #~if
	) #~forever
	l
)

(defn newline [t]
	(assertType (nextt t) :nl)
)

#termfunction will run on (peekt t), if it is true we will TERMINATE
#statementparser param is to avoid circular dependancy becuase I am really lazy adn am not gonna do metaprogramming
(defn parseSeveralStatements [t statementParser term-function]
	(var gotStatement false)
	(def body @[])
	#(assertType (nextt t) :lb)
	(forever 
		(newline t)
		(when (term-function (peekt t))#(peekcheck t term)
			(when (not gotStatement)
				(error "Expected at least one statement before rb")
			)
			#(nextt t) #consume terminator 
			(break)
		)
		(array/push body (statementParser t) )
		(set gotStatement true)
	)
	body
)
