#!/usr/local/bin/janet

(use judge)
#
##define cfg node 'classes'
(use ./utils)
(import ./parsers/ASTClasses)
(use ./CFGClasses)
(import ./parser)

#to be run after exanding 
(defn makeBlocks [statements blockIDs firstblockname endInstruction]
	(var blocks @{})
	(var curBlock @[])
	(var curBlockName firstblockname)
	(each s statements
		(if (s :branching)	
			(do
				#def somethign
				(put blocks curBlockName (new-block-nobranch curBlock ))

				#reserve next block here
				#this way we can tell everybody where we are headed after this 
				#branch
				(def reservedNext (getNextID blockIDs "l" ))
				
				(def branch-instruction (:toCFG s reservedNext blockIDs blocks makeBlocks))
				(put 
					(get blocks curBlockName) 
					:branch branch-instruction
				)
				(set curBlock @[])
				(set curBlockName reservedNext)
					
			)
			(array/push curBlock s)
		) 
	)

	#use leftover curblock in final add
	(put blocks curBlockName (new-block curBlock endInstruction))
)

(defn makeMain [body]
	(def blockID @{"l" 0 "loophead" 0 "body" 0})
	(makeBlocks body blockID "main" (ret (cfgvar 0 true)))
)



(defn statementsToCFG [body]
	(def blocks @{})
	(def blockID @[0])
	(var thisBlock @[])
	(each s body
		(case (s :type)
			#:assigment (do (def lhs (toCVGVar (s :var))) #(if ((s :expr) :atomic) #	(assign lhs (toCFGVar (s :expr)))
				#)
			#)
		)	
	)
)

## ast -> ast, but expands all compound expressions using temp variables
(defn expandCompounds [ast]
	#start at 1 because 0 reserved for throwaway commands
	#for right now just work on body, it would help to eb able to parse a full statement group to cfg and then appy it to all classes

	#(def newclasses @[])
	#(each class (ast :classes)
	#	(def tempNumber @[1])	
	#	(array/push newclasses (statement/expandStatements class tempNumber))	
	#)

	(def tempNumber @[1])
	(def body (ASTClasses/expandStatements (ast :body) tempNumber) )
		body

	#(parser/make-program newclasses (ast :vars) body)
)	

#should get macroed up using that <<- thing or whatever
(defn convertFromAST [ast]
	(makeMain (expandCompounds ast))
)

(defn expandFromString [s]
	(expandCompounds (parser/parseFromString s))
)

(defn cfgFromString [s]
	(convertFromAST (parser/parseFromString s))
)

(test (expandFromString 
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
     :type :assignment    :var {:atomic true
           :name 1
           :temp true
           :type :variable}}
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
     :var {:atomic true
           :name 2
           :temp true
           :type :variable}}
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
     :var {:atomic true
           :name 3
           :temp true
           :type :variable}}
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
     :var {:atomic true
           :name "x"
           :temp false
           :type :variable}}])

(test (expandFromString 
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
     :var {:atomic true
           :name "x"
           :temp false
           :type :variable}}
    {:expand "<function 0x2>"
     :expr {:atomic true
            :expand @ident
            :type :constant
            :value "1"}
     :type :assignment
     :var {:atomic true
           :name "y"
           :temp false
           :type :variable}}])

(test (expandFromString
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
     :var {:atomic true
           :name 1
           :temp true
           :type :variable}}
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
     :var {:atomic true
           :name 2
           :temp true
           :type :variable}}
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
     :obj {:atomic true
           :name 1
           :temp true
           :type :variable}
     :type :fieldSet}])


(test (expandFromString 
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



(test (expandFromString 
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
     :var {:atomic true
           :name "x"
           :temp false
           :type :variable}}
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
     :var {:atomic true
           :name 1
           :temp true
           :type :variable}}
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
     :var {:atomic true
           :name "0"
           :temp true
           :type :variable}}])


(test (expandFromString
`
main with x:
return x
`
)
  @[{:branching true
     :expand "<function 0x2>"
     :expr {:atomic true
            :expand @ident
            :name "x"
            :type :variable}
     :toCFG "<function 0x1>"
     :type :return}])
(test (expandFromString
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
     :var {:atomic true
           :name 1
           :temp true
           :type :variable}}
    {:branching true
     :expand "<function 0x4>"
     :expr {:atomic true
            :name 1
            :temp true
            :type :variable}
     :toCFG "<function 0x3>"
     :type :return}])


#test that branching stuff expands okay
(test (expandFromString
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
     :var {:atomic true
           :name "x"
           :temp false
           :type :variable}}
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
     :var {:atomic true
           :name 1
           :temp true
           :type :variable}}
    {:branching true
     :cond {:atomic true
            :name 1
            :temp true
            :type :variable}
     :else @[{:expand "<function 0x10>"
              :expr {:atomic true
                     :classname "FOO"
                     :expand @ident
                     :type :classRef}
              :type :assignment
              :var {:atomic true
                    :name "x"
                    :temp false
                    :type :variable}}]
     :expand "<function 0x11>"
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
            :var {:atomic true
                  :name 2
                  :temp true
                  :type :variable}}
           {:branching true
            :expand "<function 0x8>"
            :expr {:atomic true
                   :name 2
                   :temp true
                   :type :variable}
            :toCFG "<function 0x7>"
            :type :return}]
     :toCFG "<function 0x9>"
     :type :ifelse}
    {:branching true
     :expand "<function 0x13>"
     :expr {:atomic true
            :expand @ident
            :type :constant
            :value "1"}
     :toCFG "<function 0x12>"
     :type :return}])

(test (expandFromString
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
     :var {:atomic true
           :name 1
           :temp true
           :type :variable}}
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
     :var {:atomic true
           :name 2
           :temp true
           :type :variable}}
    {:body @[{:expand "<function 0x6>"
              :expr {:atomic true
                     :expand @ident
                     :name "x"
                     :type :variable}
              :type :print}
             {:expand "<function 0x7>"
              :expr {:atomic false
                     :expand "<function 0x8>"
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
              :var {:atomic true
                    :name 3
                    :temp true
                    :type :variable}}
             {:branching true
              :expand "<function 0x10>"
              :expr {:atomic true
                     :name 3
                     :temp true
                     :type :variable}
              :toCFG "<function 0x9>"
              :type :return}]
     :branching true
     :cond {:atomic true
            :name 2
            :temp true
            :type :variable}
     :expand "<function 0x11>"
     :toCFG "<function 0x5>"
     :type :ifonly}])

(test (expandFromString
`
main with x:
	while (x + 1): {
		return (x + 3)
	}
	return 1
`)
  @[{:body @[{:expand "<function 0x4>"
              :expr {:atomic false
                     :expand "<function 0x5>"
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
              :var {:atomic true
                    :name 2
                    :temp true
                    :type :variable}}
             {:branching true
              :expand "<function 0x7>"
              :expr {:atomic true
                     :name 2
                     :temp true
                     :type :variable}
              :toCFG "<function 0x6>"
              :type :return}]
     :branching true
     :cond {:atomic true
            :name 1
            :temp true
            :type :variable}
     :expand "<function 0x8>"
     :loophead @[{:expand "<function 0x2>"
                  :expr {:atomic false
                         :expand "<function 0x3>"
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
                  :var {:atomic true
                        :name 1
                        :temp true
                        :type :variable}}]
     :toCFG "<function 0x1>"
     :type :while}
    {:branching true
     :expand "<function 0x10>"
     :expr {:atomic true
            :expand @ident
            :type :constant
            :value "1"}
     :toCFG "<function 0x9>"
     :type :return}])

(test (cfgFromString 
`
main with x,y,z:
while (1 + 2): {
	x = 1
}
`
)
  @{"body0" @{:body @[{:expand "<function 0x5>"
                       :expr {:atomic true
                              :expand @ident
                              :type :constant
                              :value "1"}
                       :type :assignment
                       :var {:atomic true
                             :name "x"
                             :temp false
                             :type :variable}}]
              :branch {:block "loophead0"
                       :branching true
                       :type :jump}
              :type :basicBlock}
    "l0" @{:body @[]
           :branch {:branching true
                    :reg {:constant true
                          :name 0
                          :toStr "<function 0x4>"
                          :type :cfgvar}
                    :type :return}
           :type :basicBlock}
    "loophead0" @{:body @[{:expand "<function 0x1>"
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
                           :var {:atomic true
                                 :name 1
                                 :temp true
                                 :type :variable}}]
                  :branch {:branching true
                           :else "l0"
                           :if "body0"
                           :reg {:constant false
                                 :name "1"
                                 :toStr "<function 0x3>"
                                 :type :cfgvar}
                           :type :iff}
                  :type :basicBlock}
    "main" @{:body @[]
             :branch {:block "loophead0"
                      :branching true
                      :type :jump}
             :type :basicBlock}})

(test (cfgFromString 
`
main with x,y,z:
if (a * b): {
	a = x
} else {
	print(e)
}
return 0
`
)
  @{"l0" @{:body @[]
           :branch {:branching true
                    :reg {:constant true
                          :name "0"
                          :toStr "<function 0x5>"
                          :type :cfgvar}
                    :type :return}
           :type :basicBlock}
    "l1" @{:body @[{:expand "<function 0x7>"
                    :expr {:atomic true
                           :expand @ident
                           :name "x"
                           :type :variable}
                    :type :assignment
                    :var {:atomic true
                          :name "a"
                          :temp false
                          :type :variable}}]
           :branch {:block "l0"
                    :branching true
                    :type :jump}
           :type :basicBlock}
    "l2" @{:body @[{:expand "<function 0x6>"
                    :expr {:atomic true
                           :expand @ident
                           :name "e"
                           :type :variable}
                    :type :print}]
           :branch {:block "l0"
                    :branching true
                    :type :jump}
           :type :basicBlock}
    "l3" @{:body @[]
           :branch {:branching true
                    :reg {:constant true
                          :name 0
                          :toStr "<function 0x4>"
                          :type :cfgvar}
                    :type :return}
           :type :basicBlock}
    "main" @{:body @[{:expand "<function 0x1>"
                      :expr {:atomic false
                             :expand "<function 0x2>"
                             :lhs {:atomic true
                                   :expand @ident
                                   :name "a"
                                   :type :variable}
                             :op :*
                             :rhs {:atomic true
                                   :expand @ident
                                   :name "b"
                                   :type :variable}
                             :type :binop}
                      :type :assignment
                      :var {:atomic true
                            :name 1
                            :temp true
                            :type :variable}}]
             :branch {:branching true
                      :else "l2"
                      :if "l1"
                      :reg {:constant false
                            :name "1"
                            :toStr "<function 0x3>"
                            :type :cfgvar}
                      :type :iff}
             :type :basicBlock}})

(test (cfgFromString 
`
main with x,y,z:
while d: {
	ifonly (1 + 2): {
		p = (1 + (3 + x))
	}
	r = (x + 2)
}
print(fart)
`
)
  @{"body0" @{:body @[{:expand "<function 0x1>"
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
                       :var {:atomic true
                             :name 1
                             :temp true
                             :type :variable}}]
              :branch {:branching true
                       :else "l1"
                       :if "l2"
                       :reg {:constant false
                             :name "1"
                             :toStr "<function 0x3>"
                             :type :cfgvar}
                       :type :iff}
              :type :basicBlock}
    "l0" @{:body @[{:expand "<function 0x5>"
                    :expr {:atomic true
                           :expand @ident
                           :name "fart"
                           :type :variable}
                    :type :print}]
           :branch {:branching true
                    :reg {:constant true
                          :name 0
                          :toStr "<function 0x6>"
                          :type :cfgvar}
                    :type :return}
           :type :basicBlock}
    "l1" @{:body @[{:expand "<function 0x11>"
                    :expr {:atomic false
                           :expand "<function 0x12>"
                           :lhs {:atomic true
                                 :expand @ident
                                 :name "x"
                                 :type :variable}
                           :op :+
                           :rhs {:atomic true
                                 :expand @ident
                                 :type :constant
                                 :value "2"}
                           :type :binop}
                    :type :assignment
                    :var {:atomic true
                          :name "r"
                          :temp false
                          :type :variable}}]
           :branch {:block "loophead0"
                    :branching true
                    :type :jump}
           :type :basicBlock}
    "l2" @{:body @[{:expand "<function 0x7>"
                    :expr {:atomic false
                           :expand "<function 0x8>"
                           :lhs {:atomic true
                                 :expand @ident
                                 :type :constant
                                 :value "3"}
                           :op :+
                           :rhs {:atomic true
                                 :expand @ident
                                 :name "x"
                                 :type :variable}
                           :type :binop}
                    :type :assignment
                    :var {:atomic true
                          :name 2
                          :temp true
                          :type :variable}}
                   {:expand "<function 0x9>"
                    :expr {:atomic false
                           :expand "<function 0x10>"
                           :lhs {:atomic true
                                 :expand @ident
                                 :type :constant
                                 :value "1"}
                           :op :+
                           :rhs {:atomic true
                                 :expand @ident
                                 :num 2
                                 :type :temp}
                           :type :binop}
                    :type :assignment
                    :var {:atomic true
                          :name "p"
                          :temp false
                          :type :variable}}]
           :branch {:block "l1"
                    :branching true
                    :type :jump}
           :type :basicBlock}
    "loophead0" @{:body @[]
                  :branch {:branching true
                           :else "l0"
                           :if "body0"
                           :reg {:constant false
                                 :name "d"
                                 :toStr "<function 0x4>"
                                 :type :cfgvar}
                           :type :iff}
                  :type :basicBlock}
    "main" @{:body @[]
             :branch {:block "loophead0"
                      :branching true
                      :type :jump}
             :type :basicBlock}})
