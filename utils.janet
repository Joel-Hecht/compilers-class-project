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

(defn joinArgs [args]
	(if (empty? args)
		""
		(string (first args) " " (joinArgs (rest args )) )
	)	
)
