#!/usr/local/bin/janet
#Parse a tokenized string into expressions

(use judge)

(import ./tokenizer)
(use ./utils)
(use ./parserTools)

#expression 'class' definitions
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
							(def args (parseCommaList t :rp parseExp))
							(assertType (nextt t) :rp )
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

(defn parseOnce [s]
	(parseExp (tokenizer/make-tokenizer s))
)

(defn parseMultiple [s]
	(def t (tokenizer/make-tokenizer s))
	(def outlist @[])
	(forever
		(if (peekcheck t :eof)
			(break)
			(array/push outlist (parseExp t))
		)
	)
	outlist
)

(defn main [& args] 
	(def t (tokenizer/make-tokenizer-from-args (rest args)))
	#currently only parses first symbol
	(pp (parseExp t))
)

#inline judge tests that kind of work sometimes
(test-error (parseOnce "") "parser got eof")
(test (parseOnce "a+b") {:name "a" :type :variable}) #should probabyl error but I don't have that behavior yet.As of right now it should just parse to a variable
(test (parseOnce "(a+b)")
  {:lhs {:name "a" :type :variable}
   :op :+
   :rhs {:name "b" :type :variable}
   :type :binop})
(test (parseOnce "((a+b) * c)")
  {:lhs {:lhs {:name "a" :type :variable}
         :op :+
         :rhs {:name "b" :type :variable}
         :type :binop}
   :op :*
   :rhs {:name "c" :type :variable}
   :type :binop})
(test (parseOnce "((a+b) * (c - b))")
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
(test-error (parseOnce "(a + b") "Expected rp but found eof instead")
(test (parseOnce "a") {:name "a" :type :variable})
(test (parseOnce "&(a * b).c")
  {:base {:lhs {:name "a" :type :variable}
          :op :*
          :rhs {:name "b" :type :variable}
          :type :binop}
   :field "c"
   :type :fieldRead})
(test (parseOnce "&a.c")
  {:base {:name "a" :type :variable}
   :field "c"
   :type :fieldRead})
(test-error (parseOnce "&.c") "Token dot is not part of a valid expression")
(test-error (parseOnce "&(a+c)") "Expected dot but found eof instead")
(test-error (parseOnce "&a^c") "Expected dot but found caret instead")
(test (parseOnce "@c") {:classname "c" :type :classRef})
(test-error (parseOnce "@4") "Expected valid class name but found num instead")
(test-error (parseOnce "@(a + b)") "Expected valid class name but found lp instead")
(test (parseOnce "^(a / b).sd(&(p*c).xr)")
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
(test-error (parseOnce "^(a / b)sd(&(p*c).xr)") "Expected dot but found id instead")
(test (parseOnce "^(a / b).sd()")
  {:args @[]
   :base {:lhs {:name "a" :type :variable}
          :op :/
          :rhs {:name "b" :type :variable}
          :type :binop}
   :methodName "sd"
   :type :methodCall})

#there is some residual affect after this test that makes the subsequent test fail in judge
# i have no idea man i just work here
#(test-error (parseOnce "^(a / b).sd((p*c).xr)") "Expected rp but found dot instead")

#(test-error (parseOnce "^(a / b).sd&xr") "Expected lp but found amp instead")
(test-error (parseOnce "^a.df(") "parser got eof")
(test-error (parseOnce "^(a / b).sd(&(p*c).xr(a + b))") "Expected rp but found lp instead")
#(test (parseOnce "^(a / b).sd()&(p*c).xr") )
#(test (parseOnce "^(a / b).sd(&(p*c).xr,a,(a + b) )") )

(test-error (parseOnce "^(a / b).sd(a + b)") "Expected rp but found op instead") # should require nested parens

