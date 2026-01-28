#!/usr/local/bin/janet

(use judge)

(import ./tokenizer)
(import ./statement)
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

(defn checkNextID [t name]
	(def tok (nextt t))
	(assertType tok :id (string "'" name "' keyword"))
	(when (not= (tok :tok) name)
		(flusherror t (string "Error: Unexpected identifier '" (tok :tok) "'. expected '" name "'"  ) )
	)
)

#takes in a tokenizer
(defn getIDName [t]
	(def tok (nextt t))
	(assertType tok :id)
	(tok :tok)
)


#should be metaprogrammed but im lazy
(defn new-method [args locals body]
	{:type :method :args args :locals locals :body body}
)

(defn new-class [name fields methods]
	{:type :class :name name :fields fields :methods methods}
)


#parse a class, or, if there is no class, return false
(defn parseClass [t]
	#(pp (peekt t))
	(when (not (graceful-checkIDName (peekt t) "class"))
		(flusherror t "Error : theres no class here to parse!")
		#(print "was class!")
	)	

	#throw out 'class' token
	(nextt t)

	#matches one or more capital letters (empty string will fail)
	(def all-caps 
		(peg/compile 
			'(sequence 
				(some
					(range "AZ")
				)
				-1 #end of string
			)
		)
	)

	(def name ((nextt t) :tok))
	(when (nil? (peg/match all-caps name)) 
		(flusherror t (string "class name '" name "' was not all caps"))	
	)
	
	(assertType (nextt t) :lsb)
	(assertType (nextt t) :nl)

	#this assumes it is required to have a fields section, even if you have 0 fields
	(checkNextID t "fields")
	
	# get a list of the string names of comma seperated fields
	(def fields 
		(parseCommaList t :nl getIDName)
	)

	#consume newline
	(assertType (nextt t) :nl)
	
	#get methods
	(def methods @{})
	#if we dont get a "method" declaration, we assume there are no more methods
	(while (graceful-checkIDName (peekt t) "method")
		(nextt t) #throw away "method"
		(def mnametok (nextt t))
		(assertType mnametok :id)
		(def methodName (mnametok :tok))
		(assertType (nextt t) :lp)

		#get comma listed args until right paren
		(def args (parseCommaList t :rp getIDName))
	
		#consume right paren
		(assertType (nextt t) :rp)

		#assert that we have 0-6 arguments
		(when (> (length args) 6)
			(flusherror t (string "Error: method " methodName " has more than 6 args (got " (length args) ")") )
		)

		(checkNextID t "with")
		(checkNextID t "locals")

		(def locals (parseCommaList t :colon getIDName))

		#consume that colon
		(assertType (nextt t) :colon)

		(def body 
			(parseSeveralStatements t statement/parseStatement 
				(fn [tok]
					#return true if we want to terminate the list
					#i.e. if there is another method or the end of the class
					(or
						(graceful-checkIDName tok "method")
						(= (tok :type) :rsb)	
					)
				)
			)
		)
		
		(when (empty? body)
			(flusherror t (string "Error: expected at least one statement in body for method " methodName))
			(os/exit 1)
			(print "unreachable")
		)
	
		(when (not (nil? (get methods methodName)))
			(flusherror t (string "Error: Redeclaration of method " methodName "() in class " name ))
		)

		(put methods methodName (new-method args locals body) )
	)
	
	#get class closer
	(assertType (nextt t) :rsb)

	(new-class name fields methods)
)

(defn parseOnce [s]
	(parseClass (tokenizer/make-tokenizer s))
)

(defn main [ & args]
	(def t (tokenizer/make-tokenizer-from-args (rest args)))
	(pp (parseClass t))
)

#infinite tests forever
#example in spec
(test (parseOnce 
`class FOO [
	fields x, y, z
	method m(a,b, c) with locals q, r, s:
		a = (q+r)
		r = ^z.s(b,c)
	method m2(ase,bse,cse ) with locals t, u, v:
		!e.f=(ase + bse)
]`
)
  {:fields @["x" "y" "z"]
   :methods @{"m" {:args @["a" "b" "c"]
                   :body @[{:type :assignment
                            :value {:lhs {:name "q" :type :variable}
                                    :op :+
                                    :rhs {:name "r" :type :variable}
                                    :type :binop}
                            :var "a"}
                           {:type :assignment
                            :value {:args @[{:name "b" :type :variable}
                                            {:name "c" :type :variable}]
                                    :base {:name "z" :type :variable}
                                    :methodName "s"
                                    :type :methodCall}
                            :var "r"}]
                   :locals @["q" "r" "s"]
                   :type :method}
              "m2" {:args @["ase" "bse" "cse"]
                    :body @[{:field "f"
                             :obj {:name "e" :type :variable}
                             :type :fieldSet
                             :value {:lhs {:name "ase" :type :variable}
                                     :op :+
                                     :rhs {:name "bse" :type :variable}
                                     :type :binop}}]
                    :locals @["t" "u" "v"]
                    :type :method}}
   :name "FOO"
   :type :class})
(test (parseOnce 
`class FOO [
	fields x, y, z
]`
)
  {:fields @["x" "y" "z"]
   :methods @{}
   :name "FOO"
   :type :class})
(test-error (parseOnce 
`class FOO [
]`
)
  "Expected 'fields' keyword but found rsb instead")
(test-error (parseOnce 
`class FOo [
	fields x, y, z
	method m(a,b, c) with locals q, r, s:
		a = (q+r)
		r = ^z.s(b,c)
	method m2(ase,bse,cse ) with locals t, u, v:
		!e.f=(ase + bse)
]`
)
  "class name 'FOo' was not all caps")

#check for brace absence
(test-error (parseOnce 
`class FOO
	fields x, y, z
	method m(a,b, c) with locals q, r, s:
		a = (q+r)
		r = ^z.s(b,c)
	method m2(ase,bse,cse ) with locals t, u, v:
		!e.f=(ase + bse)
]`
)
  "Expected lsb but found nl instead")
(test-error (parseOnce 
`class FOO [
	fields x, y, z
	method m(a,b, c) with locals q, r, s:
		a = (q+r)
		r = ^z.s(b,c)
	method m2(ase,bse,cse ) with locals t, u, v:
		!e.f=(ase + bse)
`
)
  "Expected nl but found eof instead")



#check no args okay
(test (parseOnce 
`class FOO [
	fields x, y, z
	method m() with locals :
		a = (q+r)
		r = ^z.s(b,c)
]`
)
  {:fields @["x" "y" "z"]
   :methods @{"m" {:args @[]
                   :body @[{:type :assignment
                            :value {:lhs {:name "q" :type :variable}
                                    :op :+
                                    :rhs {:name "r" :type :variable}
                                    :type :binop}
                            :var "a"}
                           {:type :assignment
                            :value {:args @[{:name "b" :type :variable}
                                            {:name "c" :type :variable}]
                                    :base {:name "z" :type :variable}
                                    :methodName "s"
                                    :type :methodCall}
                            :var "r"}]
                   :locals @[]
                   :type :method}}
   :name "FOO"
   :type :class})

#check not 7 args allowed
(test-error (parseOnce 
`class FOO [
	fields x, y, z
	method m(a,b, c, a, f, g, sd) with locals q, r, s:
		a = (q+r)
		r = ^z.s(b,c)
	method m2(ase,bse,cse ) with locals t, u, v:
		!e.f=(ase + bse)
]`
)
  "Error: method m has more than 6 args (got 7)")

#comma before end of list 
(test-error (parseOnce 
`class FOO [
	fields x, y, z,
	method m(a,b, c) with locals q, r, s:
		a = (q+r)
		r = ^z.s(b,c)
]`
)
  "Expected NOT to get a(n) nl but actually got a(n) nl")

#stuff that should be on two lines is on one
(test-error (parseOnce 
`class FOO [
	fields x, y, z 
	method m(a,b, c) with locals q, r, s: a = (q+r)
	method m2(ase,bse,cse ) with locals t, u, v:
		!e.f=(ase + bse)
]`
)
  "Expected nl but found id instead")


#fields, args, etc are not variable names
(test-error (parseOnce 
`class FOO [
	fields x, :, z
	method m(:,), c) with locals q, r, s:
		a = (q+r)
		r = ^z.s(b,c)
	method m2(ase,bse,cse ) with locals t, u, v:
		!e.f=(ase + bse)
]`
)
  "Expected id but found colon instead")

#keywords are wrong
(test-error (parseOnce 
`class FOO [
	feilds x, y, z
	method m(a,b, c) with locals q, r, s:
		a = (q+r)
		r = ^z.s(b,c)
	method m2(ase,bse,cse ) with locals t, u, v:
		!e.f=(ase + bse)
]`
)
  "Error: Unexpected identifier 'feilds'. expected 'fields'")
(test-error (parseOnce 
`clss FOO [
	fields x, y, z
	method m(a,b, c) with locals q, r, s:
		a = (q+r)
		r = ^z.s(b,c)
	method m2(ase,bse,cse ) with locals t, u, v:
		!e.f=(ase + bse)
]`
)
  "Error : theres no class here to parse!")
(test-error (parseOnce 
`class FOO [
	fields x, y, z
	method m(a,b, c) locals q, r, s:
		a = (q+r)
		r = ^z.s(b,c)
	method m2(ase,bse,cse ) with locals t, u, v:
		!e.f=(ase + bse)
]`
)
  "Error: Unexpected identifier 'locals'. expected 'with'")
(test-error (parseOnce 
`class FOO [
	fields x, y, z
	mthod method m(a,b, c) with locals q, r, s:
		a = (q+r)
		r = ^z.s(b,c)
	method m2(ase,bse,cse ) with locals t, u, v:
		!e.f=(ase + bse)
]`
)
  "Expected rsb but found id instead")
(test-error (parseOnce 
`class FOO [
	fields x, y, z
	mthod m(a,b, c) with locals q, r, s:
		a = (q+r)
		r = ^z.s(b,c)
	method m2(ase,bse,cse ) with locals t, u, v:
		!e.f=(ase + bse)
]`
)
  "Expected rsb but found id instead")

#problematic test
#check redundant method name
(test-error (parseOnce 
`class FOO [
	fields x, y, z
	method m(a,b, c) with locals q, r, s:
		a = (q+r)
		r = ^z.s(b,c)
	method m(ase,bse,cse ) with locals t, u, v:
		!e.f=(ase + bse)
]`
)
  "Error: Redeclaration of method m() in class FOO")


#this is also an evil test 
#check fails if no statements
(test-error (parseOnce 
`class FOO [
	fields x, y, z
	method m(a,b, c) with locals q, r, s:
	method m2(ase,bse,cse ) with locals t, u, v:
		!e.f=(ase + bse)
]`
)
  "Error: expected at least one statement in body for method m")

#method has no statements
(test-error (parseOnce 
`class FOO [
	fields x, y, z
	method m(a,b, c) with locals q, r, s:
		a = (q+r)
		r = ^z.s(b,c)
	method m2(ase,bse,cse ) with locals t, u, v:
]`
)
  "Error: expected at least one statement in body for method m2")
