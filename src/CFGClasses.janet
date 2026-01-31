#!/usr/local/bin/janet

(use ./utils)

#smallest unit of cfg representation, either a constant or an integer
#we will abstract this away but just saying that these CFGvars are the smallest possible unit
(defn cfgvar [name isConstant]
	{
		:type :cfgvar
		:constant isConstant
		:name name
		:toStr ( fn [this]
			(if (this :constant)
				(string (this :name))
				(string "%" (this :name))
			)
		)
	}
)

(defn toCVGVar [astvar]
	(when (not (astvar :atomic))
		(error (string "tried to create a cfg variable from non atomic " (string/format "%v" astvar)))
	)
	(var constant false)
	(var name (string (astvar :name)))
	(when (= (astvar :type) :constant)
	 (set constant true)
	 (set name (astvar :value))
	)
	(cfgvar name constant)
)

(defn assign [reg val]
	{:type :CFGassignment :reg reg :val val}
)
(defn operate [reg lval rval op]
	{:type :CFGoperation :reg reg :l lval :r rval :op op}
)
(defn call [reg addr receiver args]
	{:type :CFGcall :reg reg :addr addr :obj receiver :args args}
)
(defn phi [reg blocks vars]
	{:type :CFGphi :reg reg :blocks blocks :vars vars}
)
(defn pr [reg]
	{:type :CFGprint :reg reg}
)
(defn getelt [reg arr i]
	{:type :CFGgetelt :reg reg :arr arr :ind i }	
)
(defn setelt [reg arr i new]
	{:type :CFGsetelt :reg reg :arr arr :ind i :new new}	
)
(defn load [reg base]
	{:type :CFGload :reg reg :base base}
)
(defn store [base value]
	{:type :CFGstore :base base :value value :noReg true}	
)

#types of control flow
(defn jump [blockname]
	{:type :CFGjump :branching true :block blockname}
)
(defn iff [reg i e]
	{:type :CFGiff :reg reg :if i :else e :branching true}
)
(defn ret [reg]
	{:type :CFGreturn :reg reg :branching true}
)
(defn fail [ERROR]
	{:type :CFGfail :error ERROR :branching true}
)

(defn new-block [statements branch]
	@{	:type :basicBlock
		:body statements
		:branch branch
#should also have phi here
	}
)

(defn new-block-nobranch [statements ]
	(new-block statements -1)
)


