#!/usr/local/bin/janet

(use judge)

(import ./tokenizer)
(import ./expr)
(use ./parserTools)
(use ./utils)

(defn parseExp [t]
	(expr/parseExp t)
)

(defn newline [t]
	(assertType (nextt t) :nl)
)

#expects to start right before the left bracket
#ends after consuming closing bracket - does NOT consume the newline
#needs statementParser to be the parseStatement function
#just because I'm too lazy to actually resolve the circular dependency
(defn bracketted-statements [t statementParser]
	(var gotStatement false)
	(def body @[])
	(assertType (nextt t) :lb)
	(forever 
		(newline t)
		(when (peekcheck t :rb)
			(when (not gotStatement)
				(error "Expected at least one statement before rb")
			)
			(nextt t) #consume rb
			(break)
		)
		(array/push body (statementParser t) )
		(set gotStatement true)
	)
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
(test (parseOnce "x = b"))
(test (parseOnce "x = b{}")
  {:type :assignment
   :value {:name "b" :type :variable}
   :var "x"})
(test-error (parseOnce "a * b") )
(test (parseOnce "_ = b"))
(test-error (parseOnce "_p = b"))
(test (parseOnce "!(a+b).f = &e.f"))
(test-error (parseOnce "!sdf(a * b) = 4"))
(test-error (parseOnce "!a.(a * b) = 4"))
(test-error (parseOnce "!_.(sdf) = 4"))
(test (parseOnce "print((x * b))"))
(test (parseOnce "print(^x.p((a + b),x,p))"))
(test-error (parseOnce "print(&e.f(x +p))"))
(test (parseOnce "return (z + vb)"))
(test-error (parseOnce "return "))
(test (parseOnce `if (a + b): {
a = b
p = ^a.f((a+b)) 
print(c)
} else {
x = r
}`))
(test-error (parseOnce `if (a + b) {
a = b
print(c)
} else {
x = r
}`))
(test-error (parseOnce `if (a + b): 
{
a = b
print(c)
} else {
x = r
}`))
(test-error (parseOnce `if (a + b): {
} else {
}`))
(test-error (parseOnce `if (a + b): {
} `))
(test-error (parseOnce `while: {
x = b
}`))
(test-error (parseOnce `while (a = b) {
x = b
}`))
(test (parseOnce `while &e.f: {
x =b 
c = c
}`))
(test-error (parseOnce `while &e.f: {
x =b 
c = c}`))

(test-error (parseOnce `ifonly: {
x = b
}`))
(test-error (parseOnce `ifonly (a = b) {
x = b
}`))
(test (parseOnce `ifonly &e.f: {
x =b 
c = c
}`))
(test-error (parseOnce `ifonly &e.f: {
x =b 
c = c}`))


