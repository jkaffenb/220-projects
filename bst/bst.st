
Collection subclass: Bst [
  "Binary search tree class

  Using the Node class (reverse) ---changing it if necessary

  build a binary search tree class supporting

  insertion, search, traversal, removal (challenging)

  You decide the instance variable(s)
  and API (methods).  Here are some suggestions
  but you're free to do something else.

  You might inspect Collection for ideas of other methods to implement.

  "
  | root |

  init [
     "Initialize the instance variables for an empty tree"
     root := nil
  ]

  add: item [
    "Add item to this tree"
    | leaf curr prev |
    leaf := Node data: item.
    curr := root.
    prev := nil.
    [curr isNil] whileFalse: [prev := curr. item < curr data
                          ifTrue: [curr := curr left]
                          ifFalse: [curr := curr right]].
    prev == nil ifTrue: [root := leaf]
                ifFalse: [prev data > item ifTrue: [prev left: leaf]
                                           ifFalse: [prev right: leaf]].
    ^leaf
  ]
  contains: item [
    "answer whether (true/false) this tree contains item"
  ]
  search: item [
    "answer information about where item is located in the tree.
     (vague on purpose)"
  ]
  remove: item ifAbsent: aBlock [
    "remove item from tree, or do the block if it's not there."
  ]
  remove: item [
    self remove: item ifAbsent: [
      self error: 'Invalid argument ', item asString, ': object not found'
    ]
  ]

  iot: aBlock [
    "Evaluate aBlock on all data, in an in-order traversal"
    root isNil ifFalse: [root iot: aBlock].
    ^self
  ]

  do: aBlock [  " needed for printOn: aStream to work properly "
    ^self iot: aBlock
  ]
]


Object subclass: Node [
  | data left right |

  "getters"
  data [ ^data ]
  left [ ^left ]
  right [ ^right ]
  "setters"
  data: d [ data := d ]
  left: l [
    (l isKindOf: Node) ifFalse: [self error: 'setting left to non-node'].
    left := l
  ]
  right: r [
    (r isKindOf: Node) ifFalse: [self error: 'setting right to non-node'].
    right := r
  ]

  iot: aBlock [
    "evaluate aBlock on all data in subtree rooted at self, in an in-order-traversal"
    left isNil ifFalse: [left iot: aBlock].
    aBlock value: data.
    right isNil ifFalse: [right iot: aBlock].
  ]
]

Node class extend [
   data: d [
     |obj|
     obj := super new.
     obj data: d.
     ^obj
   ]
   data: d left: l right: r [
     |result|
     result := self data: d.
     result left: l; right: r.
     ^result
   ]
]
