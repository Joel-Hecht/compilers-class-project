(import /src/tokenizer)

(use judge)
# judge makes easy tests
# it only compares output to the previous output of judge
# As long as we test extensively ourselves, judge should catch any other breaking bugs

#addendum : tests here should be EXPLICIT - inline snapshot tests (using judge) are in the src file itself


(defn testPeek [str]
	(def got @[])
	#should also have a variant where peek is not checked EVERY time
	(def peeked @[])
	(def t (tokenizer/make-tokenizer str))
	(forever
		(array/push peeked (tokenizer/peekNextToken t))
		(array/push got (tokenizer/getNextToken t))
		(when (= ((array/peek got) :type) :eof) 
			(break)	
		)
	)
	(assert (deep= peeked got))
)



(defn main [& args]
	(testPeek "sdf")
	(testPeek "...q1111- ")
)
