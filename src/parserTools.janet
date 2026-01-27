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
