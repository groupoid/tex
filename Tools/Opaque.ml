
exception Type_error

type 'a opaque =
{
  data      : Obj.t;
  type_info : 'a type_info
}
and 'a type_info =
{
  name    : string;
  apply   : 'a opaque -> 'a -> 'a;
  compare : 'a opaque -> 'a opaque -> bool;
  unify   : 'a opaque -> 'a opaque -> bool
}

let type_name x = x.type_info.name

let same_type x y = (x.type_info == y.type_info)
let apply     x y = x.type_info.apply   x y
let compare   x y = x.type_info.compare x y
let unify     x y = x.type_info.unify   x y

let declare_type name apply_func cmp_func unify_func =
  let rec wrap x =
    {
      data      = Obj.magic x;
      type_info = ti
    }
  and unwrap x =
    if x.type_info == ti then
      Obj.magic x.data
    else
      raise Type_error
  and ti =
    {
      name    = name;
      apply   = (fun x y -> apply_func (unwrap x) y);
      compare = (fun x y -> cmp_func   (unwrap x) (unwrap y));
      unify   = (fun x y -> unify_func (unwrap x) (unwrap y))
    } in
  (wrap, unwrap)
