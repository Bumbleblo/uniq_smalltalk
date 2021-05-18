Object subclass: ParametersUniqClass[
    | showCount repeated skip caseSensitive printDuplicates nChecks|

    ParametersUniqClass class >> new[
        |r|

        r := super new.
        r init.

        ^r
    ]

    init [
        showCount := false.
        repeated := false.
        skip := 1.
        nChecks := 0.
        caseSensitive := true.
        printDuplicates := false.
    ]

    setNChecks: value[ nChecks := value.]
    getNChecks[ ^nChecks ]

    setPrintDuplicates: value[ printDuplicates :=value.]
    getPrintDuplicates[ ^printDuplicates ]

    setShowCount: value[ showCount := value. ]
    getShowCount[ ^showCount ]

    setRepeated: value[ repeated := value. ]
    getRepeated[ ^repeated ]

    setSkip: value[ skip := value. ]
    getSkip[ ^skip ]

    setSensitive: value[ caseSensitive := value. ]
    getSensitive[ ^caseSensitive ]
]

Object subclass: Uniq[
    | caseSensitive skipN ignoreCase showCount showRepeated configParameters|
    <comment: 'Compare subsequent strings on a vector'>

    Uniq class >> new: parameters[
        |r|

        r := super new. 
        r init: parameters.
        ^r
    ]

    init: parameters[
        configParameters := parameters.
    ]

    compLine: line1 setLine2: line2[
        |line1Size line2Size char1 char2 line1t line2t|

        line1t := self truncString: line1.
        line2t := self truncString: line2.

        line1Size := line1t size.
        line2Size := line2t size.

        (line1Size ~~ line2Size) ifTrue:[
            ^false.
        ].

        (1 to: line1Size) do:[:i|

            char1 := line1t at: i.
            char2 := line2t at: i.
            
            (configParameters getSensitive not) ifTrue:[
                char1 := char1 asLowercase.
                char2 := char2 asLowercase.
            ].

            (char1 ~~ char2) ifTrue:[
                ^false.
            ].
            
        ].

        ^true
    ]

    uniqueLines: lines[
        | actualLine bool acc |

        bool := false.

        acc := 1.
        actualLine := lines first.

        lines removeFirst.

        "show lines 1 to n-1"
        lines do:[
            :line|

            bool := self compLine: actualLine setLine2: line.

            (bool)ifTrue:[
                acc := acc +1.

                (configParameters getPrintDuplicates) ifTrue:[
                    self showString: actualLine setIndex: acc.
                ].

            ];ifFalse: [

                (configParameters getRepeated) ifTrue:[
                    (acc > 1) ifTrue:[
                        self showString: actualLine setIndex: acc.
                    ].
                ]; ifFalse:[
                    self showString: actualLine setIndex: acc.
                ].

                actualLine := line.
                acc := 1.
            ].

        ].

        "Show last line"
        (configParameters getRepeated) ifTrue:[
            (acc > 1)ifTrue:[
                self showString: actualLine setIndex: acc.
            ].
        ]; ifFalse:[
            self showString: actualLine setIndex: acc.
        ].
   ]

   showString: string setIndex: index[
        (configParameters getShowCount) ifTrue: [
            ('      ', index asString, ' ') display.
        ].
        string displayNl.
   ]

    truncString: string[
        |rstring skip nChecks|

        skip := configParameters getSkip.
        nChecks := configParameters getNChecks.
        
        (configParameters getNChecks ~~ 0)ifTrue:[
            rstring := string copyFrom: skip to: skip + nChecks.
        ];ifFalse:[
            rstring := string copyFrom: skip.
        ].

        ^rstring
    ]
    
]

parameters := ParametersUniqClass new.

filename := nil.

Smalltalk arguments: '-h|--help --version --count --repeated --ignore-case --skip-fields: -D'
    do: [ :opt :arg |

        opt = nil ifTrue:[
            filename := arg.
        ].

        opt = 'help' ifTrue: [

        ].

        opt = 'skip-chars' ifTrue: [
            parameters setSkip: arg asNumber + 1.
        ].


        opt = 'ignore-case' ifTrue:[
            parameters setSensitive: false.
        ].
        
        opt = 'repeated' ifTrue: [
            parameters setRepeated: true.
        ].

        opt = 'count' ifTrue: [
            parameters setShowCount: true.
        ].

        opt = 'D' ifTrue: [
            parameters setPrintDuplicates: true.
        ].

        opt = 'version' ifTrue: [
            | msg |

            "TODO(felipe) Add endlines later"
            msg := 'uniq (GNU coreutils) 8.31 ',
               'A Smalltalk implementation of uniq utility ', 
               'Written by Felipe Borges'.
            
            msg display.
        ].
    ].

"Main"

data := FileStream open: filename mode: FileStream read.

comparator := Uniq new: parameters.

list := OrderedCollection new.

data linesDo:[:line | list add: line].

comparator uniqueLines: list.
