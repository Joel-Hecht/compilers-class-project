#!/usr/local/bin/janet

(use judge)
#
##define cfg node 'classes'
(import ./parsers/statement)
(import ./parser)


#case for type
(defn expandExpr [e]
)

#iterate through statements
#takes in a list of statements
(defn expand [& sts]

)

(defn expand-statement-group []


)


##takes in an ast, should expand the 
(defn expandCompounds [ast]
	#start at 1 because 0 reserved for throwaway commands
	(def tempNumber @[1])
	(statement/expandStatements (ast :body) tempNumber)
)	

#should get macroed up using that <<- thing or whatever
(defn convertFromAST [ast]
	(expandCompounds ast)
)

(defn cfgFromString [s]
	(convertFromAST (parser/parseFromString s))
)

(test (cfgFromString 
`
main with x, y, z:
x = ((1 + 2) * (x + (y  + 2)))
`
)
  @[{:expand "<function 0x1>"
     :expr {:atomic false
            :expand "<function 0x2>"
            :lhs {:atomic true
                  :expand @ident
                  :type :constant
                  :value "1"}
            :op :+
            :rhs {:atomic true
                  :expand @ident
                  :type :constant
                  :value "2"}
            :type :binop}
     :type :assignment
     :var {:name 1 :temp true}}
    {:expand "<function 0x3>"
     :expr {:atomic false
            :expand "<function 0x4>"
            :lhs {:atomic true
                  :expand @ident
                  :name "y"
                  :type :variable}
            :op :+
            :rhs {:atomic true
                  :expand @ident
                  :type :constant
                  :value "2"}
            :type :binop}
     :type :assignment
     :var {:name 2 :temp true}}
    {:expand "<function 0x5>"
     :expr {:atomic false
            :expand "<function 0x6>"
            :lhs {:atomic true
                  :expand @ident
                  :name "x"
                  :type :variable}
            :op :+
            :rhs {:atomic true
                  :expand @ident
                  :num 2
                  :type :temp}
            :type :binop}
     :type :assignment
     :var {:name 3 :temp true}}
    {:expand "<function 0x7>"
     :expr {:atomic false
            :expand "<function 0x8>"
            :lhs {:atomic true
                  :expand @ident
                  :num 1
                  :type :temp}
            :op :*
            :rhs {:atomic true
                  :expand @ident
                  :num 3
                  :type :temp}
            :type :binop}
     :type :assignment
     :var {:name "x" :temp false}}])

(test (cfgFromString 
`
main with x, y, z:
x = x
y = 1
`
)
  @[{:expand "<function 0x1>"
     :expr {:atomic true
            :expand @ident
            :name "x"
            :type :variable}
     :type :assignment
     :var {:name "x" :temp false}}
    {:expand "<function 0x2>"
     :expr {:atomic true
            :expand @ident
            :type :constant
            :value "1"}
     :type :assignment
     :var {:name "y" :temp false}}])

(test (cfgFromString
`
main with x, y, z:
	!(a+b).p = &(p+q).r
`
)
  @[{:expand "<function 0x1>"
     :expr {:atomic false
            :expand "<function 0x2>"
            :lhs {:atomic true
                  :expand @ident
                  :name "a"
                  :type :variable}
            :op :+
            :rhs {:atomic true
                  :expand @ident
                  :name "b"
                  :type :variable}
            :type :binop}
     :type :assignment
     :var 1}
    {:expand "<function 0x3>"
     :expr {:atomic false
            :expand "<function 0x4>"
            :lhs {:atomic true
                  :expand @ident
                  :name "p"
                  :type :variable}
            :op :+
            :rhs {:atomic true
                  :expand @ident
                  :name "q"
                  :type :variable}
            :type :binop}
     :type :assignment
     :var {:name 2 :temp true}}
    {:expand "<function 0x5>"
     :expr {:atomic false
            :base {:atomic true
                   :expand @ident
                   :num 2
                   :type :temp}
            :expand "<function 0x6>"
            :field "r"
            :type :fieldRead}
     :field "p"
     :obj {:name 1 :temp true}
     :type :fieldSet}])


(test (cfgFromString 
`
main with x, y, z:
!a.f = (a + b)
`)
  @[{:expand "<function 0x1>"
     :expr {:atomic false
            :expand "<function 0x2>"
            :lhs {:atomic true
                  :expand @ident
                  :name "a"
                  :type :variable}
            :op :+
            :rhs {:atomic true
                  :expand @ident
                  :name "b"
                  :type :variable}
            :type :binop}
     :field "f"
     :obj {:atomic true
           :expand @ident
           :name "a"
           :type :variable}
     :type :fieldSet}])



(test (cfgFromString 
` 
main with x:
x = (1 + 2)
_ = &(x + a).f
`
)
  @[{:expand "<function 0x1>"
     :expr {:atomic false
            :expand "<function 0x2>"
            :lhs {:atomic true
                  :expand @ident
                  :type :constant
                  :value "1"}
            :op :+
            :rhs {:atomic true
                  :expand @ident
                  :type :constant
                  :value "2"}
            :type :binop}
     :type :assignment
     :var {:name "x" :temp false}}
    {:expand "<function 0x3>"
     :expr {:atomic false
            :expand "<function 0x4>"
            :lhs {:atomic true
                  :expand @ident
                  :name "x"
                  :type :variable}
            :op :+
            :rhs {:atomic true
                  :expand @ident
                  :name "a"
                  :type :variable}
            :type :binop}
     :type :assignment
     :var {:name 1 :temp true}}
    {:expand "<function 0x5>"
     :expr {:atomic false
            :base {:atomic true
                   :expand @ident
                   :num 1
                   :type :temp}
            :expand "<function 0x6>"
            :field "f"
            :type :fieldRead}
     :type :assignment
     :var {:name "0" :temp true}}])


(test (cfgFromString
`
main with x:
return x
`
)
  @[{:expand "<function 0x1>"
     :expr {:atomic true
            :expand @ident
            :name "x"
            :type :variable}
     :type :return}])
(test (cfgFromString
`
main with x:
return (1 + 2)
`
)
  @[{:expand "<function 0x1>"
     :expr {:atomic false
            :expand "<function 0x2>"
            :lhs {:atomic true
                  :expand @ident
                  :type :constant
                  :value "1"}
            :op :+
            :rhs {:atomic true
                  :expand @ident
                  :type :constant
                  :value "2"}
            :type :binop}
     :type :assignment
     :var 1}
    {:expand "<function 0x3>"
     :expr {:name 1 :temp true}
     :type :return}])


#test that branching stuff expands okay
(test (cfgFromString
`
main with x:
	x = 1
	if (x - 1): {
		print(x)
		return (x + 3)
	} else {
		x = @FOO
	}
	return 1
`)
  @[{:expand "<function 0x1>"
     :expr {:atomic true
            :expand @ident
            :type :constant
            :value "1"}
     :type :assignment
     :var {:name "x" :temp false}}
    {:expand "<function 0x2>"
     :expr {:atomic false
            :expand "<function 0x3>"
            :lhs {:atomic true
                  :expand @ident
                  :name "x"
                  :type :variable}
            :op :-
            :rhs {:atomic true
                  :expand @ident
                  :type :constant
                  :value "1"}
            :type :binop}
     :type :assignment
     :var 1}
    {:cond {:name 1 :temp true}
     :else @[{:expand "<function 0x8>"
              :expr {:atomic true
                     :classname "FOO"
                     :expand @ident
                     :type :classRef}
              :type :assignment
              :var {:name "x" :temp false}}]
     :expand "<function 0x9>"
     :if @[{:expand "<function 0x4>"
            :expr {:atomic true
                   :expand @ident
                   :name "x"
                   :type :variable}
            :type :print}
           {:expand "<function 0x5>"
            :expr {:atomic false
                   :expand "<function 0x6>"
                   :lhs {:atomic true
                         :expand @ident
                         :name "x"
                         :type :variable}
                   :op :+
                   :rhs {:atomic true
                         :expand @ident
                         :type :constant
                         :value "3"}
                   :type :binop}
            :type :assignment
            :var 2}
           {:expand "<function 0x7>"
            :expr {:name 2 :temp true}
            :type :return}]
     :type :ifelse}
    {:expand "<function 0x10>"
     :expr {:atomic true
            :expand @ident
            :type :constant
            :value "1"}
     :type :return}])

(test (cfgFromString
`
main with x:
	ifonly ^x.meth(1,2,(1 + 2)): {
		print(x)
		return (x + 3)
	}
`)
  @[{:expand "<function 0x1>"
     :expr {:atomic false
            :expand "<function 0x2>"
            :lhs {:atomic true
                  :expand @ident
                  :type :constant
                  :value "1"}
            :op :+
            :rhs {:atomic true
                  :expand @ident
                  :type :constant
                  :value "2"}
            :type :binop}
     :type :assignment
     :var {:name 1 :temp true}}
    {:expand "<function 0x3>"
     :expr {:args @[{:atomic true
                     :expand @ident
                     :type :constant
                     :value "1"}
                    {:atomic true
                     :expand @ident
                     :type :constant
                     :value "2"}
                    {:atomic true
                     :expand @ident
                     :num 1
                     :type :temp}]
            :atomic false
            :base {:atomic true
                   :expand @ident
                   :name "x"
                   :type :variable}
            :expand "<function 0x4>"
            :methodName "meth"
            :type :methodCall}
     :type :assignment
     :var 2}
    {:body @[{:expand "<function 0x5>"
              :expr {:atomic true
                     :expand @ident
                     :name "x"
                     :type :variable}
              :type :print}
             {:expand "<function 0x6>"
              :expr {:atomic false
                     :expand "<function 0x7>"
                     :lhs {:atomic true
                           :expand @ident
                           :name "x"
                           :type :variable}
                     :op :+
                     :rhs {:atomic true
                           :expand @ident
                           :type :constant
                           :value "3"}
                     :type :binop}
              :type :assignment
              :var 3}
             {:expand "<function 0x8>"
              :expr {:name 3 :temp true}
              :type :return}]
     :cond {:name 2 :temp true}
     :expand "<function 0x9>"
     :type :ifonly}])

(test (cfgFromString
`
main with x:
	while (x + 1): {
		return (x + 3)
	}
	return 1
`)
  @[{:expand "<function 0x1>"
     :expr {:atomic false
            :expand "<function 0x2>"
            :lhs {:atomic true
                  :expand @ident
                  :name "x"
                  :type :variable}
            :op :+
            :rhs {:atomic true
                  :expand @ident
                  :type :constant
                  :value "1"}
            :type :binop}
     :type :assignment
     :var 1}
    {:body @[{:expand "<function 0x3>"
              :expr {:atomic false
                     :expand "<function 0x4>"
                     :lhs {:atomic true
                           :expand @ident
                           :name "x"
                           :type :variable}
                     :op :+
                     :rhs {:atomic true
                           :expand @ident
                           :type :constant
                           :value "3"}
                     :type :binop}
              :type :assignment
              :var 2}
             {:expand "<function 0x5>"
              :expr {:name 2 :temp true}
              :type :return}]
     :cond {:name 1 :temp true}
     :expand "<function 0x6>"
     :type :while}
    {:expand "<function 0x7>"
     :expr {:atomic true
            :expand @ident
            :type :constant
            :value "1"}
     :type :return}])
