#!/usr/local/bin/janet

(use judge)

(import ./parsers/statement)
(import ./parsers/expr)
(import ./parsers/class)
(import ./tokenizer)

(use ./parsers/parserTools)
(use ./utils)

(defn make-program [classes vars body]
	{:type :program :classes classes :vars vars :body body}
)

#pass any amount of newlines zero inclusive
(defn newlines [t]
	(while (= ((peekt t) :type) :nl)
		(nextt t)
	)
)

#takes in a tokenizer t
(defn parseProgram [t]
	(def classes @{})
	(while (graceful-checkIDName (peekt t) "class")
		(def temp-class (class/parseClass t))

		(when (not (nil? (get classes (temp-class :name))))
			(flusherror t (string "Error: Redeclaration of class " (temp-class :name)))
		)

		(put classes (temp-class :name) temp-class)

		#pass a newline until the next class or main
		(assertType (nextt t) :nl)
	)
	
	(newlines t)
	(checkNextID t "main")
	(checkNextID t "with")

	(def vars 
		(parseCommaList t :colon getIDName )
	)

	#consume that pesky colon
	(assertType (nextt t) :colon)

	#parse statements until eof
	(def body 
		(parseSeveralStatements 
			t
			statement/parseStatement	
			(fn [tok]
				(= (tok :type) :eof)
			)
			true
		)
	) 
	
	#consume eof
	(assertType (nextt t) :eof)

	(when (empty? body)
		(error "Error: there must be at least one main body statement to compile")
	)

	(make-program classes vars body)
)

(defn parseFromString [s]
	(parseProgram (tokenizer/make-tokenizer s))
)

(defn main [ & args ]
	(def t (tokenizer/make-tokenizer-from-args (rest args)))
	#currently only parses first symbol
	(pp (parseProgram t))
)

(test (parseFromString
`main with x:
x = 3`
)
  {:body @[{:type :assignment
            :value {:type :constant :value "3"}
            :var "x"}]
   :classes @{}
   :type :program
   :vars @["x"]})

(test (parseFromString
`
class FOO [
	fields y
	method k(a) with locals p:
		y = a
]
main with x:
x = 3`
)
  {:body @[{:type :assignment
            :value {:type :constant :value "3"}
            :var "x"}]
   :classes @{"FOO" {:fields @["y"]
                     :methods @{"k" {:args @["a"]
                                     :body @[{:type :assignment
                                              :value {:name "a" :type :variable}
                                              :var "y"}]
                                     :locals @["p"]
                                     :type :method}}
                     :name "FOO"
                     :type :class}}
   :type :program
   :vars @["x"]})

(test (parseFromString
`
class FOO [
	fields y
	method k(a) with locals p:
		y = a
]

main with x:
x = 3`
)
  {:body @[{:type :assignment
            :value {:type :constant :value "3"}
            :var "x"}]
   :classes @{"FOO" {:fields @["y"]
                     :methods @{"k" {:args @["a"]
                                     :body @[{:type :assignment
                                              :value {:name "a" :type :variable}
                                              :var "y"}]
                                     :locals @["p"]
                                     :type :method}}
                     :name "FOO"
                     :type :class}}
   :type :program
   :vars @["x"]})


(test-error (parseFromString
#keywords goofed
`
class FOO [
	fields y
	method k(a) with locals p:
		y = a
]

min with x:
x = 3`
)
  "Error: Unexpected identifier 'min'. expected 'main'")


(test (parseFromString
#multiple classes
`
class FOO [
	fields y
	method k(a) with locals p:
		y = a
]
class BAR [
	fields y
	method k(a) with locals p:
		y = a
]

main with x:
x = 3`
)
  {:body @[{:type :assignment
            :value {:type :constant :value "3"}
            :var "x"}]
   :classes @{"BAR" {:fields @["y"]
                     :methods @{"k" {:args @["a"]
                                     :body @[{:type :assignment
                                              :value {:name "a" :type :variable}
                                              :var "y"}]
                                     :locals @["p"]
                                     :type :method}}
                     :name "BAR"
                     :type :class}
              "FOO" {:fields @["y"]
                     :methods @{"k" {:args @["a"]
                                     :body @[{:type :assignment
                                              :value {:name "a" :type :variable}
                                              :var "y"}]
                                     :locals @["p"]
                                     :type :method}}
                     :name "FOO"
                     :type :class}}
   :type :program
   :vars @["x"]})

(test (parseFromString
#multiple body statements
`
class FOO [
	fields y
	method k(a) with locals p:
		y = a
]

main with x:
x = 3
x = 206`
)
  {:body @[{:type :assignment
            :value {:type :constant :value "3"}
            :var "x"}
           {:type :assignment
            :value {:type :constant :value "206"}
            :var "x"}]
   :classes @{"FOO" {:fields @["y"]
                     :methods @{"k" {:args @["a"]
                                     :body @[{:type :assignment
                                              :value {:name "a" :type :variable}
                                              :var "y"}]
                                     :locals @["p"]
                                     :type :method}}
                     :name "FOO"
                     :type :class}}
   :type :program
   :vars @["x"]})

(test-error (parseFromString
#no body
`
class FOO [
	fields y
	method k(a) with locals p:
		y = a
]

main with x:
`)
  "Error: there must be at least one main body statement to compile")

#no newline
(test-error (parseFromString
`
class FOO [
	fields y
	method k(a) with locals p:
		y = a
]

main with x: x = 3`
)
  "Expected nl but found id instead")

#no main
(test-error (parseFromString
`
class FOO [
	fields y
	method k(a) with locals p:
		y = a
]

x = 3`
)
  "Error: Unexpected identifier 'x'. expected 'main'")
(test-error (parseFromString
`
class FOO [
	fields y
	method k(a) with locals p:
		y = a
]
`
)
  "Expected nl but found eof instead")
