#!/usr/local/bin/janet
# Parse a tokenized string into an AST

(import ./tokenizer)
(use ./utils)

(defn assertType [x exp &opt specifier]
	(var errexp specifier)
	(if (nil? errexp)
		(set errexp exp)
	)
	(when (not= (x :type) exp)
		(wantedGotErr errexp (x :type))
	)
)

(defn print-parsed-internal [p s]
	(def s-next (string s "\t"))

)

(defn print-parsed [p]
	(print-parsed-internal p "")
)

(defn nextt [t]
	(tokenizer/getNextToken t)
)

(defn peekt [t]
	(tokenizer/peekNextToken t)
)

(defn thisExpr []
	{:type :thisExpr }
)
(defn constant [num] 
	{ :type :constant :value num }
)
(defn variable [name]
	{ :type :variable :name name}
)
(defn binop [lhs op rhs]
	{:type :binop :lhs lhs :op op :rhs rhs}
)
#base : expression, methodname : str, args : array
(defn methodCall [base methodname args]
	{:type :methodCall :base base :methodName methodname :args args}	
)
(defn fieldRead [base field]
	(:type :fieldRead :base base :field field
	)
)
(defn classRef [name]
	(:type :classRef :classname name)
)

#where t is a tokenizer object
# tokens have keys :type and :tok
(defn parseExp [t]
	(def z (nextt t))
	(def tok (z :tok))
	(case (z :type)
		:eof (errorAndExit "parser got eof")
		:this (thisExpr)
		:num (constant tok)
		:id (variable tok)
		:lp (do
					(def lhs (parseExp t))
					(def op (nextt t))
					(if (not= (op :type) :op)
						(wantedGotErr :op (op :type) )
						(do
							(def rhs (parseExp t))
							(def rp (nextt t))
							(if (not= (rp :type) :rp)
								(wantedGotErr :rp (rp :type))
								(binop lhs (op :tok) rhs)
							) #~if bad right paren
						) #~do
					) #~if bad op
				) #~do
		:amp	(do
						(def base (parseExp t))
						(def dot (nextt t))
						(if (not= (dot :type) :dot)
							(wantedGotErr :dot (dot :type))
							(do
								(def fname (nextt t))
								(if (not= (fname :type) :id)
									(wantedGotErr :id (fname :type))
									(fieldRead base (fname :tok))
								)
							)
						)
					)	
		:caret 	(do #this guy is our method call
							(def mbase (parseExp t))
							(def mdot (nextt t))
							(assertType mdot :dot)
							(def mname (nextt t))	
							(assertType mname :id "method")
							(def lp (nextt t))
							(assertType lp :lp)
							(def args @[])
							(forever
								(if (not= ((peekt t) :type) :rp)
									(do
										#add to args
										(array/push args (parseExp t))
										#throw away comma
										(when (= ((peekt t) :type) :comma)
											(nextt t)
										)
									) #~do
									(break)
								) #~if
							) #~forever
							(methodCall mbase (mname :tok) args)	
						)
		:at	(do
					(def cname (nextt t))
					(assertType cname :id "valid class name")
					(classRef (cname :tok))
				)
		(errorAndExit (string "Token " (z :type) " is not part of a valid expression"))
	)	
)

(defn main [& args] 
	(def t (tokenizer/make-tokenizer-from-args (rest args)))
	(parseExp t)
)
