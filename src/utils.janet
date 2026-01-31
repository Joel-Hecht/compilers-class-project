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

(defn tab+ [tab key]
	(def old (get tab key))
	(put tab key (+ old 1))
	old
)
(defn dereftab [tab key]
	(get tab key)
)
(defn getNextID [tab key]
	(if (nil? (get tab key))
		(do
			(put tab key 0)
			(string key 0)
		)	
		(string key (tab+ tab key))	
	)
)

(defn invertList [l]
	(def m @{})
	(eachp p l
		(put m (get p 1) (get p 0))
	)
	m
)

#make update a single array that keeps references from a map
#where m is a map of keys to ints
#and l is a list, where l[m[key]] should equal val
#add a new key/value, chceckign first if they exist, and if not appending them
(defn updateMapIfNeeded [m l key newval]
			(def mapval (get m key))
			(if (nil? mapval)
				(do
					#tell future l that we already have an index for that name
					(def newMapIndex (length l))
					(array/push l newval) #this will be that index
					(put m key newMapIndex)

				)
				(put l mapval newval )
			)
)

(defn startingCommas [l]
	(def k @[])
	(each i l
		(array/push k (string "," i))
	)
	(string/join k)
)

(defn fillArrays [l ind len]
	(each i l
		(def diff (- len (length (get i ind) )))
		(def extras (array/new-filled diff 0))
		(array/join (get i ind) extras)
		
	)	

)
