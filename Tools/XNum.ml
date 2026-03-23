include FloatNum

let ( +/ ) = add_num
let ( -/ ) = sub_num
let ( */ ) = mult_num
let ( // ) = div_num
let ( =/ ) = eq_num
let ( </ ) = lt_num
let ( >/ ) = gt_num
let ( <=/ ) = le_num
let ( >=/ ) = ge_num
let ( <>/ ) x y = not (eq_num x y)
let ( /: ) x y = num_of_int x // num_of_int y
