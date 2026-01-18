(declare-project
	:name "comp"
	# :description "make buoys description later"
	:dependencies [	"https://github.com/ianthehenry/judge" ]
)

(declare-executable
	:name "comp"
	:entry "src/parser.janet"
)



