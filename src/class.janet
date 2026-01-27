#!/usr/local/bin/janet

(import ./tokenizer)
(use ./parserTools)
(use ./utils)

(defn graceful-checkIDName [tok name]
	(var out false)
	(when (= (tok :type) :id)
		(when (= (tok :tok) name)
			(set out true)
		)
	)
	out
)

(defn checkIDName [tok name]
	(assertType tok :id)
	(when (not= (tok :tok) name)
		(error (string "Error: Unexpected identifier \"" (tok :tok) "\". expected \"" name "\""  ) )
	)
)

#matches one or more capital letters (empty string will fail)
(def all-caps 
	(peg/compile 
		'(some
			(range "AZ")
		)
	)
)


#parse a class, or, if there is no class, return false
(defn parseClass [t]
	(when (not (graceful-checkIDName (peekt t) "class"))
		(return false)
	)	

	#throw out 'class' token
	(nextt t)

	(def name (nextt t))
	(when (not (peg/match all-caps name)) 
		(error (string "class name \"" name "\" was not all caps"))	
	)
	
	(assertType (nextt t) :lsb)
	(assertType (nextt t) :nl)

	#this assumes it is required to have a fields section, even if you have 0 fields
	(checkIDName (nextt t) "fields")
	
	(forever
		
	)
)

(defn parseOnce [t]
	#body
)

(defn main [ & args]
	(def t (tokenizer/make-tokenizer-from-args (rest args)))
	(parseOnce t)
)
