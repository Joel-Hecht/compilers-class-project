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

(defn destructureLevels [ s n f ]
	(if (= 0 n)
		(f s)
		(each i s
			( destructureLevels i (- n 1) f)
		)
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

(defn joinArgs [ args]
	(if (empty? args)
		""
		(string (first args) " " (joinArgs (rest args )) )
	)	
)

# because of the way fibers work ... this class is somehow a singleton
# thats fine i dont even care
# this causes massive issues with judge because, even though only one instance of this class exists in the program as it would run generally, judge is try/accepting and then trying to make a new class
# and then of course this goes sour becuase it is still using the old class.  And so it instantly feeds unexpected input into whatever it is making and everyone dies
(def fiber-with-cache
 @{
 	:fiber nil
	:cached nil
	:construct (fn [self fiber]
		(set (self :fiber) fiber)	
	)
	:getNext (fn [self] 
		(if (nil? (self :cached))
			(resume (self :fiber))
			(do
				(def temp (self :cached))
				(set (self :cached) nil)
				temp	
			)
		)
	)
	:peek (fn [self]
		(when (nil? (self :cached))
			(set (self :cached) (resume (self :fiber)))
		)	
		(self :cached)
	)

	:flush
	(fn [self endCondition]
		#loop until a specified "end of generator" function is made
		(while (not (endCondition (:getNext self ))))
	)
 }
)

(defn make-fiber-with-cache [fiber]
	(def f fiber-with-cache)
	(:construct f fiber)
	f
)

(defn wantedGotErr [exp act]
	(error (string "Expected " exp " but found " act " instead"))
)

#in janet, only arrays can be passed by reference
(defn deref [ref]
	(get ref 0)
)

#returns current value and increments
(defn ref+ [ref]
	(def old (get ref 0))
	(put ref 0 (+ old 1))
	old
)
