#!/usr/local/bin/janet

(use ../utils)
(use ../CFGClasses)

(defn tempvar [name]
	{:name name :temp true :type :variable}
)
(defn realvar [name]
	{:name name :temp false :type :variable}
)

#inherits a temp number from a parent call
(defn expandStatements [statements tempNumber]
	(def body @[])
	(each statem statements
		(array/join body (:expand statem tempNumber))
	)
	body
)

#where all are statements except for the last one, which is the now simple version
#of whatever expression we asked for
#all created statements are assignment
#needs assignmentf to avoid circular dependancy
(defn getTempList [expr tempNumber assignmentF &opt finalExpansion]

	(def out @[])
	(var tnumSnapShot (deref tempNumber))
	(def tempsList @[])
	(def expanded (:expand expr tempsList tempNumber) )
	(each t tempsList 
		(array/push out 
			(assignmentF (tempvar tnumSnapShot) t)
		)
		(set tnumSnapShot (+ tnumSnapShot 1))
	)	

	# if we are expanding for a vlalue on the left, e.g. an object call
	# we will want to wrap it up nicely one last time in a temp var
	# if it is not already a var
	(if (and finalExpansion (not (expanded :atomic)))
		(do 
			(array/push out (assignmentF (deref tempNumber) expanded ))
			(array/push out (tempvar (ref+ tempNumber)))
		)
		(array/push out expanded)
	)
	out
)

# statement 'class' definitions
(defn assignment [name value]
	{	:type :assignment :var name :expr value
		:expand (fn [this tempNumber]
			(def l (getTempList (this :expr) tempNumber assignment))	
			(array/push l
				(assignment (realvar (this :var)) (array/pop l))
			)
			
			l	
		)
	}
)

(defn fieldSet [obj field-name new]
	{ :type :fieldSet :obj obj :field field-name :expr new
		:expand (fn [this tempNumber]
			(def l (getTempList (this :obj) tempNumber assignment true))	
			(def lexp (array/pop l))
			(array/join l (getTempList (this :expr) tempNumber assignment))	
			(def rexp (array/pop l))
			(array/push l (fieldSet lexp (this :field) rexp))
			
			l	
		)
	}
)
(defn run [e]
	{:type :run :expr e

		:expand (fn [this tempNumber]
			(def l (getTempList (this :expr) tempNumber assignment))	
			(array/push l
				#run with temp variable 0, which is reserved for these no-assignment runs
				(assignment (tempvar "0" ) (array/pop l))
			)
			
			l	
		)
	}
)
(defn ret [e]
	{ :type :return :expr e 
		:expand (fn [this tempNumber]
			(def l (getTempList (this :expr) tempNumber assignment true))	
			(array/push l
				#run with temp variable 0, which is reserved for these no-assignment runs
				(ret (array/pop l))
			)
			
			l	
		)
	}
)
(defn pr [e]
	{:type :print :expr e
		:expand (fn [this tempNumber]
			(def l (getTempList (this :expr) tempNumber assignment true))	
			(array/push l
				#run with temp variable 0, which is reserved for these no-assignment runs
				(pr (array/pop l))
			)
			
			l	
		)
	}
)

(defn ifelse [condition ifbody elsebody]
	{:type :ifelse :cond condition :if ifbody :else elsebody
		:expand (fn [this tempNumber]
			(def l (getTempList (this :cond) tempNumber assignment true))
			(def i (expandStatements (this :if) tempNumber))
			(def e (expandStatements (this :else) tempNumber))
			(array/push l
				(ifelse (array/pop l) i e)	
			)	
		)
	}
)
(defn ifonly [condition body]
	{:type :ifonly :cond condition :body body
		:expand (fn [this tempNumber]
			(def l (getTempList (this :cond) tempNumber assignment true))
			(def b (expandStatements (this :body) tempNumber))
			(array/push l
				(ifonly (array/pop l) b )	
			)	
		)
	}
)

#control flow might get weird here, since temp variables will need to be reassigned every iteration (but will be declared before the if)
(defn whileloop [condition body]
	{:type :while :cond condition :body body
		:expand (fn [this tempNumber]
			(def l (getTempList (this :cond) tempNumber assignment true))
			(def b (expandStatements (this :body) tempNumber))
			(array/push l
				(whileloop (array/pop l) b )	
			)	
		)
	}
)
