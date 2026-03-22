
open Tools.XNum
open Runtime
open Dim
open Box

type 'a table_entry =
{
  te_left     : int;       (* first column of the entry *)
  te_right    : int;       (* its last column           *)
  te_top      : int;       (* its first row             *)
  te_baseline : int;       (* the row of the baseline   *)
  te_bottom   : int;       (* the last row              *)
  te_contents : 'a         (* the contents of the entry *)
}

let calc_column_sizes num_columns entries = 
  let widths = Array.make
                 num_columns
                 {
                   d_base = minus_num infinite;
                   d_stretch_factor = infinite;
                   d_stretch_order  = 10;
                   d_shrink_factor  = infinite;
                   d_shrink_order   = 10
                 } in
  let rec sum_widths_internal f t = 
    if f > t then xdim_zero
    else xdim_add_dim (sum_widths_internal (f+1) t) widths.(f) in
  let rec calc_width col =
    if col < num_columns then begin
      List.iter (fun e -> if e.te_right = col then 
        let prev_width = xdim_to_dim (sum_widths_internal e.te_left (e.te_right - 1)) in
        let (cur_width, _, _) = e.te_contents in
        widths.(col) <- dim_max widths.(col) (dim_sub cur_width prev_width)) entries;
      calc_width (col + 1)
    end else widths
  in
  calc_width 0

let calc_row_sizes num_rows entries line_params = 
  let heights   = Array.make num_rows dim_zero in
  let depths    = Array.make num_rows dim_zero in
  let baselines = Array.make num_rows dim_zero in
  let rec sum_baselines_internal f t = 
    if f > t then xdim_zero
    else xdim_add_dim (sum_baselines_internal (f+1) t) baselines.(f) in
  
  let rec iter_rows row =
    (match row < num_rows with
     | true ->
        if row > 0 then begin
          let leading = line_params.Galley.leading
                          (new_rule_box dim_zero dim_zero depths.(row-1))
                          (new_rule_box dim_zero heights.(row) dim_zero)
                          line_params in
          baselines.(row-1) <- dim_add (dim_add depths.(row-1) heights.(row)) leading;
        end;
        
        List.iter (fun e -> 
          if e.te_baseline = row then
            let prev_height = xdim_to_dim (sum_baselines_internal e.te_top (e.te_baseline - 1)) in
            let (_, cur_height, _) = e.te_contents in
            heights.(row) <- dim_max heights.(row) (dim_sub cur_height prev_height)
          else if e.te_bottom = row then
            let prev_depth = xdim_to_dim (sum_baselines_internal e.te_baseline (e.te_bottom - 1)) in
            let (_, _, cur_depth) = e.te_contents in
            depths.(row) <- dim_max depths.(row) (dim_sub cur_depth prev_depth)
        ) entries;
        iter_rows (row + 1)
     | false -> (heights, depths, baselines)) in
  iter_rows 0

let make num_columns num_rows entries line_params = 
  let dimensions = List.map
                     (fun e -> { e with te_contents = HBox.dimensions e.te_contents })
                     entries in
  let widths                       = calc_column_sizes num_columns dimensions in
  let (heights, depths, baselines) = calc_row_sizes num_rows dimensions line_params in
  let col_start = Array.make (num_columns + 1) xdim_zero in
  let row_start = Array.make (num_rows    + 1) xdim_zero in
  for i = 0 to num_columns - 1 do
    col_start.(i+1) <- xdim_add_dim col_start.(i) widths.(i)
  done;
  for i = 0 to num_rows - 2 do
    row_start.(i+1) <- xdim_add_dim row_start.(i) baselines.(i)
  done;
  row_start.(num_rows) <- xdim_add_dim row_start.(num_rows - 1) depths.(num_rows - 1);
  let sum_widths_func first last = xdim_to_dim (xdim_sub col_start.(last + 1) col_start.(first)) in
  let rec layout_internal entries_list = match entries_list with
    | []      -> []
    | e :: es -> Graphic.PutBox
                   (xdim_to_dim col_start.(e.te_left),
                    (dim_neg (xdim_to_dim row_start.(e.te_baseline))),
                    (HBox.make_to HBox.LR (sum_widths_func e.te_left e.te_right).d_base e.te_contents), None)
                 :: layout_internal es in
  new_compound_box
    (xdim_to_dim col_start.(num_columns))
    heights.(0)
    (xdim_to_dim row_start.(num_rows))
    (layout_internal entries)