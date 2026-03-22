
open XNum

type 'a node =
  | Empty
  | Full of 'a
  | Split of 'a node * 'a node * 'a node * 'a node

type 'a map = {
  m_min1 : num;
  m_max1 : num;
  m_min2 : num;
  m_max2 : num;
  m_root : 'a node;
}

val empty : num -> num -> num -> num -> 'a map
val add : 'a map -> num -> num -> num -> num -> 'a -> 'a map
val set_rect : 'a map -> num -> num -> num -> num -> 'a -> 'a map
val find_free_top : 'a map -> num -> num -> num -> num -> num -> (num * num) option
val find_free_bottom : 'a map -> num -> num -> num -> num -> num -> (num * num) option
val find_free_left : 'a map -> num -> num -> num -> num -> num -> (num * num) option
val find_free_right : 'a map -> num -> num -> num -> num -> num -> (num * num) option
val vert_strip : 'a map -> num -> num -> (num * num) list
val horiz_strip : 'a map -> num -> num -> (num * num) list
val get_all : 'a map -> (num * num * num * num * 'a) list
