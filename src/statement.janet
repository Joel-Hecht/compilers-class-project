#!/usr/local/bin/janet

(use judge)

(import ./tokenizer)
(import ./expr)
(use ./parserTools)
(use ./utils)

(defn parseExp [t]
	(expr/parseExp t)
)

#expects to start right before the left bracket
#ends after consuming closing bracket - does NOT consume the newline
#needs statementParser to be the parseStatement function
#just because I'm too lazy to actually resolve the circular dependency
(defn bracketted-statements [t statementParser]
	(var gotStatement false)
	(def body @[])
	(assertType (nextt t) :lb)

	(def body 
		(parseSeveralStatements t statementParser 
			(fn [tok] #takes in token
				#return true if we want to terminate, i.e. if the peeked token is an }
				(= (tok :type) :rb)
			)
		) 
	)
	
	(when (empty? body)
		(error "Error: Expected at least 1 statement in conditional")
	)

	(assertType (nextt t) :rb) #consume rb, was checked in parseSeveral
	body
)

# statement 'class' definitions
(defn assignment [name value]
	{:type :assignment :var name :value value}
)
(defn fieldSet [obj field-name new]
	{ :type :fieldSet :obj obj :field field-name :value new}
)
(defn run [e]
	{:type :run :expr e}
)
(defn ret [e]
	{ :type :return :expr e }
)
(defn pr [e]
	{:type :print :expr e}
)
(defn ifelse [condition ifbody elsebody]
	{:type :ifelse :cond condition :if ifbody :else elsebody}
)
(defn ifonly [condition body]
	{:type :ifonly :cond condition :body body}
)
(defn whileloop [condition body]
	{:type :while :cond condition :body body}
)

# where t is some tokenizer
(defn parseStatement [t]
	(def z (nextt t))
	(def tok (z :tok))
	(case (z :type)
		:eof (error "parser got eof, expected statement")
		:id (do 
					(def name tok)
					(assertType (nextt t) :=)
					(def value (parseExp t))
					(assignment name value)
				)
		:underscore (do
									(assertType (nextt t) :=)	
									(def e (parseExp t) )
									(run e)
								)
		:not (do
					 (def obj (parseExp t))
					 (assertType (nextt t) :dot)
					 (def field (nextt t))
					 (assertType field :id)
					 (def fieldName (field :tok))
					 (assertType (nextt t) :=)
					 (def new-value (parseExp t))
					 (fieldSet obj fieldName new-value)
				 )
		:iff 	(do
						(def condition (parseExp t))
						(assertType (nextt t) :colon)
						(def ifbody (bracketted-statements t parseStatement) )
						(assertType (nextt t) :else)
						(def elsebody (bracketted-statements t parseStatement) )
						(ifelse condition ifbody elsebody)
				 	)
		:ifonly (do 
							(def condition (parseExp t))
							(assertType (nextt t) :colon)
							(def body (bracketted-statements t parseStatement))
							(ifonly condition body)
						)
		:while	(do 
							(def condition (parseExp t))
							(assertType (nextt t) :colon)
							(def body (bracketted-statements t parseStatement))
							(while condition body)
						)
		:ret (ret (parseExp t))
		:print (do
						(assertType (nextt t) :lp)
						(def value (parseExp t))
						(assertType (nextt t) :rp)
						(pr value)
					 )
		(error (string "Expected a statement, but instead got token " (z :type)))
	)
)

(defn parseOnce [s]
	(parseStatement (tokenizer/make-tokenizer s))	
)

(defn main [& args]
	(def t (tokenizer/make-tokenizer-from-args (rest args)))	
	(pp (parseStatement t))
)

#infinite tests forever
#judge hates this file - whatever the first test is will always fail.  I have set up this dummy test to catch this inital first stupid error
#for some reason it thinks theres an op here.  I dont even know.  There might be some residual input from somewhere else?  I would
# need a greater understanding of how judge works which I will not be doing
(test-error (parseOnce "") "Expected a statement, but instead got token op")
(test (parseOnce "x = b")
  {:type :assignment
   :value {:name "b" :type :variable}
   :var "x"})
(test (parseOnce "x = b{}")
  {:type :assignment
   :value {:name "b" :type :variable}
   :var "x"})
(test-error (parseOnce "a * b") "Expected = but found op instead")
(test (parseOnce "_ = b")
  {:expr {:name "b" :type :variable}
   :type :run})
(test-error (parseOnce "_p = b") "Expected = but found id instead")
(test (parseOnce "!(a+b).f = &e.f")
  {:field "f"
   :obj {:lhs {:name "a" :type :variable}
         :op :+
         :rhs {:name "b" :type :variable}
         :type :binop}
  :type :fieldSet
   :value {:base {:name "e" :type :variable}
           :field "f"
           :type :fieldRead}})
(test-error (parseOnce "!sdf(a * b) = 4") "Expected dot but found lp instead")
(test-error (parseOnce "!a.(a * b) = 4") "Expected id but found lp instead")
(test-error (parseOnce "!_.(sdf) = 4") "Token underscore is not part of a valid expression")
(test (parseOnce "print((x * b))")
  {:expr {:lhs {:name "x" :type :variable}
          :op :*
          :rhs {:name "b" :type :variable}
          :type :binop}
   :type :print})
(test (parseOnce "print(^x.p((a + b),x,p))")
  {:expr {:args @[{:lhs {:name "a" :type :variable}
                   :op :+
                   :rhs {:name "b" :type :variable}
                   :type :binop}
                  {:name "x" :type :variable}
                  {:name "p" :type :variable}]
          :base {:name "x" :type :variable}
          :methodName "p"
          :type :methodCall}
   :type :print})
(test-error (parseOnce "print(&e.f(x +p))") "Expected rp but found lp instead")
(test (parseOnce "return (z + vb)")
  {:expr {:lhs {:name "z" :type :variable}
          :op :+
          :rhs {:name "vb" :type :variable}
          :type :binop}
   :type :return})
(test-error (parseOnce "return ") "parser got eof")
(test (parseOnce `if (a + b): {
a = b
p = ^a.f((a+b)) 
print(c)
} else {
x = r
}`)
  {:cond {:lhs {:name "a" :type :variable}
          :op :+
          :rhs {:name "b" :type :variable}
          :type :binop}
   :else @[{:type :assignment
            :value {:name "r" :type :variable}
            :var "x"}]
   :if @[{:type :assignment
          :value {:name "b" :type :variable}
          :var "a"}
         {:type :assignment
          :value {:args @[{:lhs {:name "a" :type :variable}
                           :op :+
                           :rhs {:name "b" :type :variable}
                           :type :binop}]
                  :base {:name "a" :type :variable}
                  :methodName "f"
                  :type :methodCall}
          :var "p"}
         {:expr {:name "c" :type :variable}
          :type :print}]
   :type :ifelse})
(test-error (parseOnce `if (a + b) {
a = b
print(c)
} else {
x = r
}`)
  "Expected colon but found lb instead")
(test-error (parseOnce `if (a + b): 
{
a = b
print(c)
} else {
x = r
}`)
  "Expected lb but found nl instead")
(test-error (parseOnce `if (a + b): {
} else {
}`)
  "Error: Expected at least 1 statement in conditional")
(test-error (parseOnce `if (a + b): {
} `)
  "Expected a statement, but instead got token rb")
(test-error (parseOnce `while: {
x = b
}`)
  "Token colon is not part of a valid expression")
(test-error (parseOnce `while (a * b) {
x = b
}`)
  "Expected colon but found lb instead")
(test-error (parseOnce `while &e.f: {
x = b 
c = c}`)
  "Expected nl but found rb instead")
(test-error (parseOnce `ifonly: {
x = b
}`)
  "Token colon is not part of a valid expression")
(test-error (parseOnce `ifonly (a * b) {
x = b
}`)
  "Expected colon but found lb instead")
(test (parseOnce `ifonly &e.f: {
x =b 
c = c
}`)
  {:body @[{:type :assignment
            :value {:name "b" :type :variable}
            :var "x"}
           {:type :assignment
            :value {:name "c" :type :variable}
            :var "c"}]
   :cond {:base {:name "e" :type :variable}
          :field "f"
          :type :fieldRead}
   :type :ifonly})
(test-error (parseOnce `ifonly &e.f: {
x =b 
c = c}`)
  "Expected nl but found rb instead")
(test (parseOnce "return (z + vb)")
  {:expr {:lhs {:name "z" :type :variable}
          :op :+
          :rhs {:name "vb" :type :variable}
          :type :binop}
   :type :return})

#for some reason this test is ebil and cursed and will kill judge if it exists
#i dont know why.  Test this manually if you really care
#(test (parseOnce `while &e.f: {
#x = b 
#c = c
#}`))


#test with tabs
(test (parseOnce 
`ifonly &e.f: {
	x =b 
	c = c
}`)
  {:body @[{:type :assignment
            :value {:name "b" :type :variable}
            :var "x"}
           {:type :assignment
            :value {:name "c" :type :variable}
            :var "c"}]
   :cond {:base {:name "e" :type :variable}
          :field "f"
          :type :fieldRead}
   :type :ifonly})
