open XNum

type spline = (num * num * num * num * num * num * num * num)

type side_spec = 
  | Open
  | Curl of num
  | Angle of float
  | Explicit of num * num
  | Endpoint
  | Cycle

type knot = {
  mutable left_spec     : side_spec;
  mutable right_spec    : side_spec;
  left_tension  : num;
  right_tension : num;
  point_x       : num;
  point_y       : num;
}

type path_spec = knot list

let log_knot k =
  Printf.eprintf "(%f,%f)" (float_of_num k.point_x) (float_of_num k.point_y);
  (match k.left_spec with
   | Open -> Printf.eprintf " open"
   | Curl c -> Printf.eprintf " curl %f" (float_of_num c)
   | Angle a -> Printf.eprintf " angle %f" a
   | Explicit (x, y) -> Printf.eprintf " explicit %f %f" (float_of_num x) (float_of_num y)
   | Endpoint -> Printf.eprintf " endpoint"
   | Cycle -> Printf.eprintf " cycle");
  Printf.eprintf " %f" (float_of_num k.left_tension);
  (match k.right_spec with
   | Open -> Printf.eprintf " open"
   | Curl c -> Printf.eprintf " curl %f" (float_of_num c)
   | Angle a -> Printf.eprintf " angle %f" a
   | Explicit (x, y) -> Printf.eprintf " explicit %f %f" (float_of_num x) (float_of_num y)
   | Endpoint -> Printf.eprintf " endpoint"
   | Cycle -> Printf.eprintf " cycle");
  Printf.eprintf " %f\n" (float_of_num k.right_tension)

let log_knots knots = 
  for i = 0 to Array.length knots - 1 do
    Printf.eprintf "%d: " i;
    log_knot knots.(i)
  done

(* side specifications *)
let is_open spec = match spec with | Open -> true | _ -> false
let is_curl spec = match spec with | Curl _ -> true | _ -> false
let is_angle spec = match spec with | Angle _ -> true | _ -> false
let is_explicit spec = match spec with | Explicit _ -> true | _ -> false
let is_endpoint spec = match spec with | Endpoint -> true | _ -> false
let is_cycle spec = match spec with | Cycle -> true | _ -> false

let curl spec = match spec with
  | Curl c -> c
  | _ -> invalid_arg "Bezier.curl"

let convert_open_to_curl spec = match spec with
  | Open -> Curl num_one
  | _ -> spec

(* geometric functions *)
let pi = 4.0 *. atan 1.0
let angle_of_vec x y = 180.0 /. pi *. atan2 (float_of_num y) (float_of_num x)
let sind x = sin (pi *. x /. 180.0)
let cosd x = cos (pi *. x /. 180.0)

let fast_reduce_angle a =
  if a > 180.0 then a -. 180.0
  else if a < -180.0 then a +. 180.0
  else a

let slow_reduce_angle a = mod_float a 360.0

let num_4      = num_of_int 4
let num_16     = num_of_int 16
let num_sqrt_2 = num_of_float (sqrt 2.0)
let num_sqrt_5 = num_of_float (sqrt 5.0)

let const1 = (num_of_ints 3 2) */ (num_sqrt_5 -/ num_one)
let const2 = (num_of_ints 3 2) */ (num_three -/ num_sqrt_5)

let velocity sin_theta cos_theta sin_phi cos_phi tension =
  let n = num_two +/
            num_sqrt_2 */ (sin_theta -/ (sin_phi // num_16))
                       */ (sin_phi -/ (sin_theta // num_16))
                       */ (cos_theta -/ cos_phi) in
  let d = num_three +/ (const1 */ cos_theta) +/ (const2 */ cos_phi) in
  let x = n // (d */ tension) in
  if x >=/ num_4 then num_4 else x

let curl_ratio curl_val left_tension right_tension =
  let alpha = num_one // right_tension in
  let beta  = num_one // left_tension in
  let r     = alpha // beta in
  let gamma = r */ r */ curl_val in
  let d = (gamma */ alpha) +/ num_three -/ beta in
  let n = (gamma */ (num_three -/ alpha)) +/ beta in
  let x = n // d in
  if x >=/ num_4 then num_4 else x

(* Join knots with the same coordinates by a straight line. *)
let join_points spec =
  let len = Array.length spec in
  for i = 0 to len - 1 do
    let n = if i + 1 < len then i + 1 else 0 in
    let k1 = spec.(i) in
    let k2 = spec.(n) in
    if k1.point_x =/ k2.point_x && k1.point_y =/ k2.point_y then (
      match k1.right_spec with
      | Open | Curl _ | Angle _ ->
          spec.(i) <- { k1 with right_spec = Explicit (k2.point_x, k2.point_y);
                               left_spec  = convert_open_to_curl k1.left_spec };
          spec.(n) <- { k2 with left_spec  = Explicit (k1.point_x, k1.point_y);
                               right_spec = convert_open_to_curl k2.right_spec }
      | _ -> ()
    )
  done

let calculate_angles spec =
  let len     = Array.length spec in
  let delta_x = Array.make len num_zero in
  let delta_y = Array.make len num_zero in
  let dist    = Array.make len num_zero in
  let psi     = Array.make len num_zero in

  for i = 0 to len - 1 do
    let n = if i + 1 < len then i + 1 else 0 in
    let k1 = spec.(i) in
    let k2 = spec.(n) in
    delta_x.(i) <- k2.point_x -/ k1.point_x;
    delta_y.(i) <- k2.point_y -/ k1.point_y;
    dist.(i)    <- num_of_float (sqrt (float_of_num ((delta_x.(i) */ delta_x.(i)) +/ (delta_y.(i) */ delta_y.(i)))));

    if i > 0 then (
      let sine   = delta_y.(i-1) // dist.(i-1) in
      let cosine = delta_x.(i-1) // dist.(i-1) in
      psi.(i) <- num_of_float (angle_of_vec ((delta_x.(i) */ cosine) +/ (delta_y.(i) */ sine))
                                            ((delta_y.(i) */ cosine) -/ (delta_x.(i) */ sine)))
    )
  done;
  if is_cycle spec.(0).left_spec then (
    let sine   = delta_y.(len-1) // dist.(len-1) in
    let cosine = delta_x.(len-1) // dist.(len-1) in
    psi.(0) <- num_of_float (angle_of_vec ((delta_x.(0) */ cosine) +/ (delta_y.(0) */ sine))
                                          ((delta_y.(0) */ cosine) -/ (delta_x.(0) */ sine)));
    psi.(len-1) <- psi.(0)
  ) else (
    psi.(0)       <- num_zero;
    psi.(len-1) <- num_zero
  );
  (delta_x, delta_y, dist, psi)

let remove_open_at_endpoints spec first_knot last_knot =
  let k1 = spec.(first_knot) in
  let k2 = spec.(last_knot) in
  if is_open k1.right_spec then (
    match k1.left_spec with
    | Explicit (x, y) ->
        let dx = k1.point_x -/ x in
        let dy = k1.point_y -/ y in
        if dx =/ num_zero && dy =/ num_zero then
          spec.(first_knot) <- { k1 with right_spec = Curl num_one }
        else
          spec.(first_knot) <- { k1 with right_spec = Angle (angle_of_vec dx dy) }
    | _ -> ()
  );
  if is_open k2.left_spec then (
    match k2.right_spec with
    | Explicit (x, y) ->
        let dx = x -/ k2.point_x in
        let dy = y -/ k2.point_y in
        if dx =/ num_zero && dy =/ num_zero then
          spec.(last_knot) <- { k2 with left_spec = Curl num_one }
        else
          spec.(last_knot) <- { k2 with left_spec = Angle (angle_of_vec dx dy) }
    | _ -> ()
  )

let set_control_points knots left_knot right_knot delta_x delta_y sin_theta cos_theta sin_phi cos_phi =
  let lk = knots.(left_knot) in
  let rk = knots.(right_knot) in
  let left_tension  = lk.right_tension in
  let right_tension = rk.left_tension in
  let left_vel  = velocity sin_theta cos_theta sin_phi cos_phi (abs_num left_tension) in
  let right_vel = velocity sin_phi cos_phi sin_theta cos_theta (abs_num right_tension) in
  let (left_vel2, right_vel2) =
    if (left_tension </ num_zero || right_tension </ num_zero)
    && ((sin_theta >=/ num_zero && sin_phi >=/ num_zero) ||
        (sin_theta <=/ num_zero && sin_phi <=/ num_zero)) then (
      let st = abs_num sin_theta in
      let sp = abs_num sin_phi in
      let s = (num_of_ints 4097 4096) */ ((st */ cos_phi) +/ (sp */ cos_theta)) in
      if s >/ num_zero then (
        let l = if left_tension </ num_zero then min_num (sp // s) left_vel else left_vel in
        let r = if right_tension </ num_zero then min_num (st // s) right_vel else right_vel in
        (l, r)
      ) else (left_vel, right_vel)
    ) else (left_vel, right_vel) in

  let left_x  = lk.point_x +/ (left_vel2  */ ((delta_x */ cos_theta) -/ (delta_y */ sin_theta))) in
  let left_y  = lk.point_y +/ (left_vel2  */ ((delta_y */ cos_theta) +/ (delta_x */ sin_theta))) in
  let right_x = rk.point_x -/ (right_vel2 */ ((delta_x */ cos_phi)   +/ (delta_y */ sin_phi))) in
  let right_y = rk.point_y -/ (right_vel2 */ ((delta_y */ cos_phi)   -/ (delta_x */ sin_phi))) in
  knots.(left_knot)  <- { lk with right_spec = Explicit (left_x, left_y) };
  knots.(right_knot) <- { rk with left_spec  = Explicit (right_x, right_y) }

let calculate_splines knots (delta_x, delta_y, dist, psi) first_knot last_knot =
  let len = Array.length knots in
  let idx i = if i < 0 then i + len else if i >= len then i - len else i in
  let u     = Array.make len num_zero in
  let v     = Array.make len num_zero in
  let w     = Array.make len num_zero in
  let theta = Array.make len num_zero in
  let next = idx (first_knot + 1) in
  (match knots.(first_knot).right_spec with
   | Angle a -> (match knots.(next).left_spec with
       | Angle a2 ->
           let angle = angle_of_vec delta_x.(first_knot) delta_y.(first_knot) in
           set_control_points knots first_knot next delta_x.(first_knot) delta_y.(first_knot)
             (num_of_float (sind (a -. angle))) (num_of_float (cosd (a -. angle)))
             (num_of_float (sind (a2 -. angle))) (num_of_float (cosd (a2 -. angle)))
       | _ -> 
           u.(first_knot) <- num_zero;
           v.(first_knot) <- num_of_float (fast_reduce_angle (a -. angle_of_vec delta_x.(first_knot) delta_y.(first_knot)));
           w.(first_knot) <- num_zero)
   | Curl c -> (match knots.(next).left_spec with
       | Curl _ ->
           let kl = knots.(first_knot) in
           let kr = knots.(next) in
           let sl = num_three */ (abs_num kl.right_tension) in
           let sr = num_three */ (abs_num kr.left_tension) in
           knots.(first_knot) <- { kl with right_spec = Explicit (kl.point_x +/ (delta_x.(first_knot) // sl), 
                                                                  kl.point_y +/ (delta_y.(first_knot) // sl)) };
           knots.(next) <- { kr with left_spec = Explicit (kr.point_x -/ (delta_x.(first_knot) // sr), 
                                                           kr.point_y -/ (delta_y.(first_knot) // sr)) }
       | _ ->
           let lt = abs_num knots.(first_knot).right_tension in
           let rt = abs_num knots.(next).left_tension in
           u.(first_knot) <- if lt =/ num_one && rt =/ num_one then (c +/ c +/ num_one) // (c +/ num_two)
                             else curl_ratio c lt rt;
           v.(first_knot) <- minus_num (psi.(next) */ u.(first_knot));
           w.(first_knot) <- num_zero)
   | Open -> u.(first_knot) <- num_zero; v.(first_knot) <- num_zero; w.(first_knot) <- num_one
   | _ -> assert false);

  let rec iter middle =
    let left  = idx (middle-1) in
    let right = idx (middle+1) in
    (match knots.(middle).left_spec with
     | Cycle | Open ->
         let lt  = abs_num knots.(left).right_tension in
         let rt  = abs_num knots.(right).left_tension in
         let a = num_one // ((num_three */ lt) -/ num_one) in
         let b = num_one // ((num_three */ rt) -/ num_one) in
         let c = num_one -/ (a */ u.(left)) in
         let d = dist.(middle) */ (num_three -/ (num_one // lt)) in
         let e = dist.(left)   */ (num_three -/ (num_one // rt)) in
         let s = abs_num (knots.(middle).left_tension // knots.(middle).right_tension) in
         let f = e // (e +/ (c */ d */ s */ s)) in
         u.(middle) <- b */ f;
         let acc = minus_num (psi.(right) */ u.(middle)) in
         if is_curl knots.(left).right_spec then (
           v.(middle) <- acc -/ (psi.(idx (first_knot + 1)) */ (num_one -/ f));
           w.(middle) <- num_zero
         ) else (
           let f_val = (num_one -/ f) // c in
           let acc = acc -/ (psi.(middle) */ f_val) in
           let f_val2 = a */ f_val in
           v.(middle) <- acc -/ (f_val2 */ v.(left));
           w.(middle) <- minus_num (f_val2 */ w.(left))
         );
         if is_cycle knots.(middle).left_spec then (
           let rec iter2 a_val b_val i =
             let c_val = v.(i) -/ (a_val */ u.(i)) in
             let d_val = w.(i) -/ (b_val */ u.(i)) in
             if i <> last_knot then iter2 c_val d_val (idx (i-1))
             else (
               let e_val = c_val // (num_one -/ d_val) in
               theta.(last_knot) <- e_val;
               v.(first_knot)    <- e_val;
               let rec iter3 i =
                 let n = idx (i + 1) in
                 v.(i) <- v.(i) +/ (e_val */ w.(i));
                 if n <> last_knot then iter3 n
               in
               iter3 (idx (first_knot + 1))
             )
           in iter2 num_zero num_one left
         )
     | Curl c ->
         let lt = abs_num knots.(middle).left_tension in
         let rt = abs_num knots.(left).right_tension in
         let s = if lt =/ num_one && rt =/ num_one then (c +/ c +/ num_one) // (c +/ num_two)
                 else curl_ratio c rt lt in
         theta.(middle) <- minus_num (s */ v.(left)) // (num_one -/ (s */ u.(left)))
     | Angle a -> theta.(middle) <- num_of_float (fast_reduce_angle (a -. angle_of_vec delta_x.(left) delta_y.(left)))
     | _ -> ());
    if middle <> last_knot then iter right in
  iter (idx (first_knot + 1));

  let rec iter_back i =
    let n = idx (i + 1) in
    if i <> last_knot then theta.(i) <- v.(i) -/ (u.(i) */ theta.(n));
    let phi = minus_num psi.(n) -/ theta.(n) in
    let sin_theta = sind (float_of_num theta.(i)) in
    let cos_theta = cosd (float_of_num theta.(i)) in
    let sin_phi   = sind (float_of_num phi) in
    let cos_phi   = cosd (float_of_num phi) in
    set_control_points knots i n delta_x.(i) delta_y.(i)
      (num_of_float sin_theta) (num_of_float cos_theta)
      (num_of_float sin_phi) (num_of_float cos_phi);
    if i <> first_knot then iter_back (idx (i-1))
  in iter_back (idx (last_knot - 1))

let compute_path spec =
  let knots = Array.of_list spec in
  let len   = Array.length knots in
  if len < 2 then (
    if len = 0 then [||]
    else (let k = knots.(0) in [|(k.point_x, k.point_y, k.point_x, k.point_y, k.point_x, k.point_y, k.point_x, k.point_y)|])
  ) else (
    join_points knots;
    let dist_psi = calculate_angles knots in
    let idx i = if i < 0 then i + len else if i >= len then i - len else i in
    let make_path knts =
      let n = if is_endpoint knts.(0).left_spec then Array.length knts - 1 else Array.length knts in
      Array.init n (fun i ->
        let k1 = knts.(i) in
        let k2 = knts.(idx (i+1)) in
        match (k1.right_spec, k2.left_spec) with
        | (Explicit (x1, y1), Explicit (x2, y2)) -> (k1.point_x, k1.point_y, x1, y1, x2, y2, k2.point_x, k2.point_y)
        | _ -> assert false)
    in
    let rec find_next_break i =
      match (knots.(i).left_spec, knots.(i).right_spec) with
      | (Open, Open) -> find_next_break (idx (i+1))
      | _            -> i
    in
    let first_break = match knots.(0).left_spec with
      | Endpoint -> 0
      | Cycle    -> find_next_break 1
      | _        -> assert false
    in
    if first_break > 0 then knots.(0) <- { knots.(0) with left_spec = Open };
    
    let rec iter cur_break =
      let continue next_break =
        if next_break <> first_break && not (is_endpoint knots.(next_break).right_spec) then iter next_break
        else make_path knots
      in
      if is_explicit knots.(cur_break).right_spec then continue (idx (cur_break+1))
      else (
        let next_break = find_next_break (idx (cur_break+1)) in
        remove_open_at_endpoints knots cur_break next_break;
        calculate_splines knots dist_psi cur_break next_break;
        continue next_break
      )
    in iter first_break
  )

(* building path specifications *)
let make_spec x y = [ {
  left_spec     = Endpoint;
  right_spec    = Open;
  left_tension  = num_one;
  right_tension = num_one;
  point_x       = x;
  point_y       = y;
} ]

let make_specs_consistent knot = match (knot.left_spec, knot.right_spec) with
  | (Open, Curl c)   -> { knot with left_spec  = Curl c       }
  | (Open, Angle a)  -> { knot with left_spec  = Angle a      }
  | (Open, Endpoint) -> { knot with left_spec  = Curl num_one }
  | (Curl c,   Open) -> { knot with right_spec = Curl c       }
  | (Angle a,  Open) -> { knot with right_spec = Angle a      }
  | (Endpoint, Open) -> { knot with right_spec = Curl num_one }
  | _                -> knot

let rec cleanup_spec cycle result spec = match spec with
  | [k] -> 
      if cycle then
        let x = make_specs_consistent { k with left_spec = Open } in
        let y = { x with left_spec = Cycle } in
        y :: result
      else
        (make_specs_consistent k) :: result
  | k :: ks -> cleanup_spec cycle ((make_specs_consistent k) :: result) ks
  | []      -> assert false

let close_spec spec cycle = match spec with
  | []      -> assert false
  | k :: ks ->
      if is_cycle k.right_spec then invalid_arg "Bezier.close_spec: last point misssing!"
      else
        let s = if cycle then spec else ({ k with right_spec = Endpoint } :: ks) in
        compute_path (cleanup_spec cycle [] s)

let add_point spec x y = match spec with
  | []      -> assert false
  | k :: ks ->
      if is_cycle k.right_spec then
        { k with right_spec = Open; point_x = x; point_y = y } :: ks
      else
        { left_spec = Open; right_spec = Open; left_tension = num_one; right_tension = num_one; point_x = x; point_y = y } :: spec

let add_in_dir spec angle = match spec with
  | []     -> assert false
  | k :: _ ->
      if is_cycle k.right_spec then invalid_arg "Bezier.add_in_dir: left side already specified!"
      else
        { left_spec = Angle (slow_reduce_angle angle); right_spec = Cycle; left_tension = num_one; right_tension = num_one; point_x = num_zero; point_y = num_zero } :: spec

let add_in_curl spec curl_val = match spec with
  | []      -> assert false
  | k :: _ ->
      if is_cycle k.right_spec then invalid_arg "Bezier.add_in_curl: left side already specified!"
      else
        { left_spec = Curl curl_val; right_spec = Cycle; left_tension = num_one; right_tension = num_one; point_x = num_zero; point_y = num_zero } :: spec

let add_in_tension spec tension = match spec with
  | []      -> assert false
  | k :: ks ->
      if is_cycle k.right_spec then { k with left_tension = tension } :: ks
      else
        { left_spec = Open; right_spec = Cycle; left_tension = tension; right_tension = num_one; point_x = num_zero; point_y = num_zero } :: spec

let add_out_dir spec angle = match spec with
  | []      -> assert false
  | k :: ks ->
      if is_open k.right_spec then { k with right_spec = Angle (slow_reduce_angle angle) } :: ks
      else invalid_arg "Bezier.add_out_dir: right side already specified!"

let add_out_curl spec curl_val = match spec with
  | []      -> assert false
  | k :: ks ->
      if is_open k.right_spec then { k with right_spec = Curl curl_val } :: ks
      else invalid_arg "Bezier.add_out_curl: right side already specified!"

let add_out_tension spec tension = match spec with
  | []      -> assert false
  | k :: ks -> { k with right_tension = tension } :: ks

let add_control_points spec x1 y1 x2 y2 = match spec with
  | []      -> assert false
  | k :: ks ->
      if is_open k.right_spec then
        { left_spec = Explicit (x2, y2); right_spec = Cycle; left_tension = num_one; right_tension = num_one; point_x = num_zero; point_y = num_zero } ::
        { k with right_spec = Explicit (x1, y1) } :: ks
      else if is_cycle k.right_spec then invalid_arg "Bezier.add_control_points: point missing!"
      else invalid_arg "Bezier.add_control_points: right side already specified!"
