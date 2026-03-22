
type index_position = LeftIndex | RightIndex | VertIndex

type math_code =
    NoMath
  | Ordinary
  | BinOp
  | Relation
  | Operator
  | Punct
  | Open
  | Close
  | Inner
  | SubScript
  | SuperScript
  | IndexPosition of index_position
