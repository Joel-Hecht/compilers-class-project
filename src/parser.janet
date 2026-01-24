#!/usr/local/bin/janet
# Parse a tokenized string into an AST

(use judge)

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
		:eof (error "parser got eof")
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
										(if (= ((peekt t) :type) :comma)
											(nextt t)
											#if its not a comma, it had better be a rp
											(assertType (peekt t) :rp)
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
		(error (string "Token " (z :type) " is not part of a valid expression"))
	)	
)

(defn parseString [s]
	(parseExp (tokenizer/make-tokenizer s))
)

(defn main [& args] 
	(def t (tokenizer/make-tokenizer-from-args (rest args)))
	#currently only parses first symbol
	(pp (parseExp t))
)

(test-error (parseString "") "parser got eof")
(test (parseString "a+b") {:name "a" :type :variable}) #should probabyl error but I don't have that behavior yet.As of right now it should just parse to a variable
(test (parseString "(a+b)")
  {:lhs {:name "a" :type :variable}
   :op :+
   :rhs {:name "b" :type :variable}
   :type :binop})
(test (parseString "((a+b) * c)")
  {:lhs {:lhs {:name "a" :type :variable}
         :op :+
         :rhs {:name "b" :type :variable}
         :type :binop}
   :op :*
   :rhs {:name "c" :type :variable}
   :type :binop})
(test (parseString "((a+b) * (c - b))")
  {:lhs {:lhs {:name "a" :type :variable}
         :op :+
         :rhs {:name "b" :type :variable}
         :type :binop}
   :op :*
   :rhs {:lhs {:name "c" :type :variable}
         :op :-
         :rhs {:name "b" :type :variable}
         :type :binop}
   :type :binop})
(test-error (parseString "(a + b") "Expected rp but found eof instead")
(test (parseString "a") {:name "a" :type :variable})
(test (parseString "&(a * b).c")
  {:base {:lhs {:name "a" :type :variable}
          :op :*
          :rhs {:name "b" :type :variable}
          :type :binop}
   :field "c"
   :type :fieldRead})
(test (parseString "&a.c")
  {:base {:name "a" :type :variable}
   :field "c"
   :type :fieldRead})
(test-error (parseString "&.c") "Token dot is not part of a valid expression")
(test-error (parseString "&(a+c)") "Expected dot but found eof instead")
(test-error (parseString "&a^c") "Expected dot but found caret instead")
(test (parseString "@c") {:classname "c" :type :classRef})
(test-error (parseString "@4") "Expected valid class name but found num instead")
(test-error (parseString "@(a + b)") "Expected valid class name but found lp instead")
(test (parseString "^(a / b).sd(&(p*c).xr)")
  {:args @[{:base {:lhs {:name "p" :type :variable}
                   :op :*
                   :rhs {:name "c" :type :variable}
                   :type :binop}
            :field "xr"
            :type :fieldRead}]
   :base {:lhs {:name "a" :type :variable}
          :op :/
          :rhs {:name "b" :type :variable}
          :type :binop}
   :methodName "sd"
   :type :methodCall})
(test-error (parseString "^(a / b)sd(&(p*c).xr)") "Token rp is not part of a valid expression")
(test (parseString "^(a / b).sd()")
  {:args @[]
   :base {:lhs {:name "a" :type :variable}
          :op :/
          :rhs {:name "b" :type :variable}
          :type :binop}
   :methodName "sd"
   :type :methodCall})
(test-error (parseString "^(a / b).sd((p*c).xr)") "Token rp is not part of a valid expression")
(test (parseString "^(a / b).sd()&(p*c).xr")
  {:args @[]
   :base {:lhs {:name "a" :type :variable}
          :op :/
          :rhs {:name "b" :type :variable}
          :type :binop}
   :methodName "sd"
   :type :methodCall}) #shouln't error because it reaches the end of the expression - maybe we change this later
(test-error (parseString "^(a / b).sd()&(p*c).xr") "Token rp is not part of a valid expression")
(test-error (parseString "^(a / b).sd&xr") "Expected lp but found amp instead")
(test-error (parseString "^(a / b).sd(a + b)") "Expected rp but found op instead") # should require nested parens
(test-error (parseString "^(a / b).sd(&(p*c).xr(a + b))") "Token op is not part of a valid expression") #no comma
(test (parseString "^(a / b).sd(&(p*c).xr,a,(a + b) )")
  {:args @[{:base {:lhs {:name "p" :type :variable}
                   :op :*
                   :rhs {:name "c" :type :variable}
                   :type :binop}
            :field "xr"
            :type :fieldRead}
           {:name "a" :type :variable}
           {:lhs {:name "a" :type :variable}
            :op :+
            :rhs {:name "b" :type :variable}
            :type :binop}]
   :base {:lhs {:name "a" :type :variable}
          :op :/
          :rhs {:name "b" :type :variable}
          :type :binop}
   :methodName "sd"
   :type :methodCall})
(test-error (parseString "^a.df(") "Token rp is not part of a valid expression")
