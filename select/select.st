Object subclass: #Select.
Select extend [
  "Intializes variables and OrderedCollections to be used in the Select class"
  | guardBlocks thenBlocks elseBlock item |
  init [guardBlocks := OrderedCollection new.
        thenBlocks  := OrderedCollection new.
        elseBlock   := OrderedCollection new.
        item := nil]

  switch: itm                    [item := itm]

  "Checks if switch has been called, if not, add to the OrderedCollection"
  if: guardBlock then: thenBlock [ item isNil ifFalse: [^'Error'].
                                   guardBlocks add: guardBlock.
                                   thenBlocks  add: thenBlock ]
  "Checks if switch has been called, if it has, add the truth value of the case
    to the OrderedCollection"
  case: collectionBlock then: thenBlock [ item isNil ifTrue: [^'Error'].
                                          guardBlocks add: [collectionBlock value includes: item].
                                          thenBlocks  add: thenBlock]
  else: elseblock                [ elseBlock add: elseblock ]

]
Select class extend [
  reload [ FileStream fileIn: 'select.st'. ^'ok' ]
  any [ |r| r := AnySelect new. r init. ^r ]
  first [ |r| r := FirstSelect new. r init. ^r ]
  all [ |r| r := AllSelect new. r init. ^r ]
  exclusive [ |r| r := ExclusiveSelect new. r init. ^r ]
]

Select subclass: AnySelect [
  end [
    " Properly follows the algorithm for the 'any' variant of
    Select. "
    | answer |
    "Runs through the OrderedCollection, storing answer as true values. Once the
    OrderedCollection is empty, ^answer, or if no answer, check and use else"
    [guardBlocks size ~= 0]
    whileTrue: [guardBlocks first value
                ifTrue: [answer := thenBlocks first.
                         thenBlocks removeFirst.
                         guardBlocks removeFirst]
                ifFalse: [thenBlocks removeFirst.
                          guardBlocks removeFirst.
                          answer isNil
                          ifTrue: [guardBlocks size = 0
                                   ifTrue: [elseBlock size = 1
                                   ifTrue: [^elseBlock first value]]]]].
    ^answer value
    ]
]

Select subclass: FirstSelect [
  end [
    " Properly follows the algorithm for the 'first' variant of
    Select. "
    | answer |
    "Runs until a true guard is reached, if there are none, check if there is an
    elseblock"
    [guardBlocks size ~= 0]
    whileTrue: [guardBlocks first value
                ifTrue: [answer := thenBlocks first. ^answer value]
                ifFalse: [thenBlocks removeFirst.
                          guardBlocks removeFirst.
                          guardBlocks size = 0
                          ifTrue: [elseBlock size = 1
                          ifTrue: [^elseBlock first value]]]]
  ]
]

Select subclass: AllSelect [
  end [
    " Properly follows the algorithm for the 'all' variant of
    Select. "
    | answer |
    "Runs through the entire OrderedCollection, printing each true thenBlock. If
    none are reached, check elseBlock."
    [guardBlocks size ~= 0]
    whileTrue: [guardBlocks first value
                ifTrue: [answer := thenBlocks first.
                         thenBlocks removeFirst.
                         guardBlocks removeFirst.
                         answer value printNl]
                ifFalse: [thenBlocks removeFirst.
                          guardBlocks removeFirst.
                          guardBlocks size = 0
                          ifTrue: [elseBlock size = 1
                          ifTrue: [answer isNil
                          ifTrue: [^elseBlock first value]]]]]
  ]
]

Select subclass: ExclusiveSelect [
  end [
    " Properly follows the algorithm for the 'exclusive' variant of
    Select. "
    | answer |
    "Stores the first trueGuard, if there are more than one return error, if
      there are no trueGuards, check elseBlock if it doesn't exist return error"
    [guardBlocks size ~= 0]
    whileTrue: [guardBlocks first value
                ifTrue: [answer isNil ifFalse: [^'Error']
                                      ifTrue:  [answer := thenBlocks first.
                                                thenBlocks removeFirst.
                                                guardBlocks removeFirst.
                                                guardBlocks size = 0
                                                ifTrue: [^answer value]]]
                ifFalse: [thenBlocks removeFirst.
                          guardBlocks removeFirst.
                          guardBlocks size = 0
                          ifTrue: [answer isNil
                                   ifFalse: [^answer value]
                                   ifTrue:  [elseBlock size = 1
                                             ifFalse:[^'Error']
                                             ifTrue: [^elseBlock first value]]]]
                                             ]
  ]
]
