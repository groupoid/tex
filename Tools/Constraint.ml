
open XNum

exception Inconsistent

type var = int

type bound = var * num

type equation = var LinForm.lin_form

type coordinate = {
  min          : num;
  max          : num;
  lower_bounds : bound list;
  upper_bounds : bound list;
  equations    : equation list;
}

type system = {
  mutable x_coords : coordinate array;
  mutable y_coords : coordinate array;
  mutable num_vars : int;
}

let create () = {
  x_coords = [||];
  y_coords = [||];
  num_vars = 0;
}

let clone system = {
  x_coords = Array.copy system.x_coords;
  y_coords = Array.copy system.y_coords;
  num_vars = system.num_vars;
}

let add_variable system min_x min_y max_x max_y =
  let x = {
    min = min_x;
    max = max_x;
    lower_bounds = [];
    upper_bounds = [];
    equations = [];
  } in
  let y = {
    min = min_y;
    max = max_y;
    lower_bounds = [];
    upper_bounds = [];
    equations = [];
  } in
  if system.num_vars < Array.length system.x_coords then begin
    system.x_coords.(system.num_vars) <- x;
    system.y_coords.(system.num_vars) <- y
  end else begin
    let new_len = if system.num_vars = 0 then 10 else 2 * system.num_vars in
    system.x_coords <- Array.init new_len (fun i -> if i < system.num_vars then system.x_coords.(i) else x);
    system.y_coords <- Array.init new_len (fun i -> if i < system.num_vars then system.y_coords.(i) else y)
  end;
  system.num_vars <- system.num_vars + 1

let update sys =
  let eval_lin_form get_coord lf =
    List.fold_left
      (fun acc (a, x) ->
        let (min, max) = acc in
        let c = get_coord x in
        if a >=/ num_zero then
          (min +/ a */ c.min, max +/ a */ c.max)
        else
          (min +/ a */ c.max, max +/ a */ c.min)
      )
      (lf.LinForm.const, lf.LinForm.const)
      lf.LinForm.terms
  in
  let update_coord get_coord coord =
    let new_min = List.fold_left (fun m (v, c) -> max_num m ((get_coord v).min +/ c)) coord.min coord.lower_bounds in
    let new_max = List.fold_left (fun m (v, c) -> min_num m ((get_coord v).max +/ c)) coord.max coord.upper_bounds in
    let (new_min2, new_max2) =
      List.fold_left
        (fun (m, x) eq ->
          let (l, u) = eval_lin_form get_coord eq in
          (max_num m l, min_num x u)
        )
        (new_min, new_max)
        coord.equations
    in
    if new_min2 >/ new_max2 then raise Inconsistent
    else if new_min2 >/ coord.min || new_max2 </ coord.max then Some { coord with min = new_min2; max = new_max2 }
    else None
  in
  let get_x_coord v = sys.x_coords.(v) in
  let get_y_coord v = sys.y_coords.(v) in
  let changed = ref true in
  while !changed do
    changed := false;
    for i = 0 to sys.num_vars - 1 do
      if sys.x_coords.(i).min </ sys.x_coords.(i).max then
        match update_coord get_x_coord sys.x_coords.(i) with
        | None -> ()
        | Some c -> sys.x_coords.(i) <- c; changed := true;
      if sys.y_coords.(i).min </ sys.y_coords.(i).max then
        match update_coord get_y_coord sys.y_coords.(i) with
        | None -> ()
        | Some c -> sys.y_coords.(i) <- c; changed := true
    done
  done

let set_x_coord system var x =
  let old = system.x_coords.(var) in
  if x </ old.min || x >/ old.max then raise Inconsistent;
  system.x_coords.(var) <- { old with min = x; max = x };
  update system

let set_y_coord system var y =
  let old = system.y_coords.(var) in
  if y </ old.min || y >/ old.max then raise Inconsistent;
  system.y_coords.(var) <- { old with min = y; max = y };
  update system

let add_x_equation system (eq : var LinForm.lin_form) =
  let var = List.hd eq.terms |> snd in (* Simplified *)
  system.x_coords.(var) <- { system.x_coords.(var) with equations = eq :: system.x_coords.(var).equations };
  update system

let add_y_equation system (eq : var LinForm.lin_form) =
  let var = List.hd eq.terms |> snd in (* Simplified *)
  system.y_coords.(var) <- { system.y_coords.(var) with equations = eq :: system.y_coords.(var).equations };
  update system

let add_x_bound system v1 v2 off =
  let c1 = system.x_coords.(v1) in
  let c2 = system.x_coords.(v2) in
  system.x_coords.(v1) <- { c1 with lower_bounds = (v2, off) :: c1.lower_bounds };
  system.x_coords.(v2) <- { c2 with upper_bounds = (v1, -.off) :: c2.upper_bounds };
  update system

let add_y_bound system v1 v2 off =
  let c1 = system.y_coords.(v1) in
  let c2 = system.y_coords.(v2) in
  system.y_coords.(v1) <- { c1 with lower_bounds = (v2, off) :: c1.lower_bounds };
  system.y_coords.(v2) <- { c2 with upper_bounds = (v1, -.off) :: c2.upper_bounds };
  update system
