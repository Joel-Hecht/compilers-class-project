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
	{:type :fieldRead :base base :field field
	}
)
(defn classRef [name]
	{:type :classRef :classname name}
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
					(assertType op :op)
					(def rhs (parseExp t))
					(def rp (nextt t))
					(assertType rp :rp)
					(binop lhs (op :tok) rhs)
				) 
		:amp	(do
						(def base (parseExp t))
						(def dot (nextt t))
						(assertType dot :dot)
						(def fname (nextt t))
						(assertType fname :id "valid field name")
						(fieldRead base (fname :tok))
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
	(pp (parseExp t))
)
