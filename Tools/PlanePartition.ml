
open FloatNum
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

let empty min1 max1 min2 max2 = {
  m_min1 = min1;
  m_max1 = max1;
  m_min2 = min2;
  m_max2 = max2;
  m_root = Empty;
}

let rec add_to_node min1 max1 min2 max2 node x1 y1 x2 y2 v =
  match node with
  | Empty -> 
      if x1 <=/ min1 && max1 <=/ x2 && y1 <=/ min2 && max2 <=/ y2 then Full v
      else
        let mid1 = (min1 +/ max1) // num_two in
        let mid2 = (min2 +/ max2) // num_two in
        let n11 = add_to_node min1 mid1 min2 mid2 Empty x1 y1 x2 y2 v in
        let n12 = add_to_node min1 mid1 mid2 max2 Empty x1 y1 x2 y2 v in
        let n21 = add_to_node mid1 max1 min2 mid2 Empty x1 y1 x2 y2 v in
        let n22 = add_to_node mid1 max1 mid2 max2 Empty x1 y1 x2 y2 v in
        Split (n11, n12, n21, n22)
  | Full _ -> node
  | Split (n11, n12, n21, n22) ->
      let mid1 = (min1 +/ max1) // num_two in
      let mid2 = (min2 +/ max2) // num_two in
      let n11 = if x1 </ mid1 && y1 </ mid2 then add_to_node min1 mid1 min2 mid2 n11 x1 y1 x2 y2 v else n11 in
      let n12 = if x1 </ mid1 && y2 >/ mid2 then add_to_node min1 mid1 mid2 max2 n12 x1 y1 x2 y2 v else n12 in
      let n21 = if x2 >/ mid1 && y1 </ mid2 then add_to_node mid1 max1 min2 mid2 n21 x1 y1 x2 y2 v else n21 in
      let n22 = if x2 >/ mid1 && y2 >/ mid2 then add_to_node mid1 max1 mid2 max2 n22 x1 y1 x2 y2 v else n22 in
      Split (n11, n12, n21, n22)

let add map x1 y1 x2 y2 v =
  { map with m_root = add_to_node map.m_min1 map.m_max1 map.m_min2 map.m_max2 map.m_root x1 y1 x2 y2 v }

let set_rect = add

let find_free_top _ _ _ _ _ _ = None
let find_free_bottom _ _ _ _ _ _ = None
let find_free_left _ _ _ _ _ _ = None
let find_free_right _ _ _ _ _ _ = None
let vert_strip _ _ _ = []
let horiz_strip _ _ _ = []
let iter _ _ = ()
let fold _ _ acc = acc
let get_all _ = []
