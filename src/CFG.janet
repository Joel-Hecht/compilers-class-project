#!/usr/local/bin/janet

(use judge)
#
##define cfg node 'classes'
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
	(def tempNumber @[0])
	(def body @[])
	#only gonna work for assignemnet right now
	(each statem (ast :body)
		(array/join body (:expand statem tempNumber))
	)
	body
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
                  :type :constant
                  :value "1"}
            :op :+
            :rhs {:atomic true
                  :type :constant
                  :value "2"}
            :type :binop}
     :type :assignment
     :var {:name 0 :temp true}}
    {:expand "<function 0x3>"
     :expr {:atomic false
            :expand "<function 0x4>"
            :lhs {:atomic true :name "y" :type :variable}
            :op :+
            :rhs {:atomic true
                  :type :constant
                  :value "2"}
            :type :binop}
     :type :assignment
     :var {:name 1 :temp true}}
    {:expand "<function 0x5>"
     :expr {:atomic false
            :expand "<function 0x6>"
            :lhs {:atomic true :name "x" :type :variable}
            :op :+
            :rhs {:atomic true :num 1 :type :temp}
            :type :binop}
     :type :assignment
     :var {:name 2 :temp true}}
    {:expand "<function 0x7>"
     :expr {:atomic false
            :expand "<function 0x8>"
            :lhs {:atomic true :num 0 :type :temp}
            :op :*
            :rhs {:atomic true :num 2 :type :temp}
            :type :binop}
     :type :assignment
     :var {:name "x" :temp false}}])
