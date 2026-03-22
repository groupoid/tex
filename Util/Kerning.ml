
open XNum
open Runtime
open GlyphBitmap
open Unicode.UTypes
open Logging

let calc_momentum weight base glyph = 
  let rec iter_x x sum = 
    if x > glyph.g_max_x then sum
    else 
      let rec iter_y y n = 
        if y > glyph.g_max_y then
          if x <= base then iter_x (x + 1) (sum -/ (num_of_int n */ weight (base - x)))
          else iter_x (x + 1) (sum +/ (num_of_int n */ weight (x - base)))
        else if GlyphBitmap.point glyph x y then iter_y (y + 1) (n + 1)
        else iter_y (y + 1) n
      in iter_y glyph.g_min_y 0
  in iter_x glyph.g_min_x num_zero

let calc_center weight glyph = 
  let rec iter left right = 
    if right <= left + 1 then
      if (calc_momentum weight left glyph +/ calc_momentum weight right glyph) <=/ num_zero then left
      else right
    else
      let m = (left + right) / 2 in
      if calc_momentum weight m glyph <=/ num_zero then iter left m
      else iter m right
  in iter glyph.g_min_x glyph.g_max_x

let calc_left weight inv_weight center glyph = 
  let width  = glyph.g_max_x - glyph.g_min_x + 1 in
  let height = glyph.g_max_y - glyph.g_min_y + 1 in
  let rec iter_x x sum = 
    if x < glyph.g_min_x then center - inv_weight (sum // num_of_int (width * height))
    else
      let rec iter_y y n = 
        if y > glyph.g_max_y then iter_x (x - 1) (sum +/ (num_of_int n */ weight (center - x)))
        else if GlyphBitmap.point glyph x y then iter_y (y + 1) (n + 1)
        else iter_y (y + 1) n
      in iter_y glyph.g_min_y 0
  in iter_x (center - 1) num_zero

let calc_right weight inv_weight center glyph = 
  let width  = glyph.g_max_x - glyph.g_min_x + 1 in
  let height = glyph.g_max_y - glyph.g_min_y + 1 in
  let rec iter_x x sum = 
    if x < glyph.g_min_x then center + inv_weight (sum // num_of_int (width * height))
    else
      let rec iter_y y n = 
        if y > glyph.g_max_y then iter_x (x - 1) (sum +/ (num_of_int n */ weight (x - center)))
        else if GlyphBitmap.point glyph x y then iter_y (y + 1) (n + 1)
        else iter_y (y + 1) n
      in iter_y glyph.g_min_y 0
  in iter_x (center - 1) num_zero

let calc_left_border glyph = 
  let rec iter_y y border = 
    if y > glyph.g_max_y then List.rev border
    else
      let rec iter_x x = 
        if x > glyph.g_max_x || GlyphBitmap.point glyph x y then iter_y (y + 1) (x :: border)
        else iter_x (x + 1)
      in iter_x glyph.g_min_x
  in iter_y glyph.g_min_y []

let calc_right_border glyph = 
  let width = glyph.g_width - 1 in
  let rec iter_y y border = 
    if y > glyph.g_max_y then List.rev border
    else
      let rec iter_x x = 
        if x < glyph.g_min_x || GlyphBitmap.point glyph x y then iter_y (y + 1) ((width - x) :: border)
        else iter_x (x - 1)
      in iter_x glyph.g_max_x
  in iter_y glyph.g_min_y []

let calc_projection max_x border = 
  let rec iter x result = 
    let num = List.length (List.filter (fun z -> z <= x) border) in
    if num = 0 then (x + 1, result)
    else iter (x - 1) (num :: result)
  in iter max_x []

let calc_protuding_area weight projection = 
  let rec iter out p = match p with
    | []      -> []
    | n :: ns -> 
        let rec iter_sum sum x l = match l with
          | []      -> sum :: iter (n :: out) ns
          | m :: ms -> iter_sum (sum +/ weight x m) (x+1) ms
        in iter_sum num_zero 0 (n :: out)
  in iter [] projection

let calc_border_diff delta projection = 
  let rec iter prev l = match l with
    | []      -> []
    | n :: ns -> (n - List.nth prev (delta - 1)) :: iter (n :: prev) ns
  in iter (Tools.XList.repeat delta 0) projection

let calc_kern hppp vppp glyph border = 
  let find_pos cmp_gt threshold l = 
    let rec iter pos l = match l with
      | []      -> pos
      | n :: ns -> if cmp_gt n threshold then pos else iter (pos+1) ns
    in iter 0 l
  in
  let (first, numbers) = calc_projection glyph.g_width border in
  let area_threshold   = hppp */ vppp in
  let area_weight x n  = num_of_int n */ num_of_int (x + 1) // hppp in
  let area             = calc_protuding_area area_weight numbers in
  let area_pos         = find_pos gt_num area_threshold area in
  let diff_order       = hppp // num_of_int 8 in
  let diff_threshold   = int_of_num (round_num (num_of_int 2 */ vppp)) in
  let diff             = calc_border_diff (int_of_num (round_num diff_order)) numbers in
  let diff_pos         = find_pos (>) diff_threshold diff in
  first + min area_pos diff_pos

let calc_left_kern  hppp vppp glyph = calc_kern hppp vppp glyph (calc_left_border glyph)
let calc_right_kern hppp vppp glyph = calc_kern hppp vppp glyph (calc_right_border glyph)

let output_lrcodes left_kern right_kern glyphs = 
  let std_l = List.nth left_kern  77 in
  let std_r = List.nth right_kern 77 in
  let rec iter lk rk glyphs = match glyphs with
    | []      -> ()
    | g :: gs -> 
        let l = List.hd lk in
        let r = List.hd rk in
        log_string "\\lpcode\\font ";
        log_int    g.g_glyph;
        log_string "=";
        log_int    (int_of_num (round_num (num_of_int 1000 */ num_of_int (l - std_l) // num_of_int g.g_width)));
        log_string "\\rpcode\\currentfont";
        log_int    g.g_glyph;
        log_string "=";
        log_int    (int_of_num (round_num (num_of_int 1000 */ num_of_int (r - std_r) // num_of_int g.g_width)));
        log_string "\n";
        iter (List.tl lk) (List.tl rk) gs
  in iter left_kern right_kern glyphs

let output_margin_test left_kern right_kern glyphs = 
  let output_line l r g = 
    log_string "\\hbox to \\linewidth{\\hbox to 1mm{}\\kern ";
    log_num    (num_of_ints (-l) 600);
    log_string "in \\char";
    log_int    g.g_glyph;
    log_string "x\\,\\leaders\\hbox{\\,i\\,}\\hfill x\\char";
    log_int    g.g_glyph;
    log_string "\\kern ";
    log_num    (num_of_ints (-r) 600);
    log_string "in\\hbox to 1mm{}}\n"
  in
  log_string "\\documentclass[a4paper,10pt]{article}\n";
  log_string "\\usepackage{geometry,multicol}\n";
  log_string "%\\geometry{left=1cm, textwidth=2cm, top=1cm, bottom=1cm}\n";
  log_string "\\parindent0pt\n";
  log_string "\\begin{document}%\n";
  log_string "\\begin{multicols}{5}%\n";
  let fill_l = List.nth left_kern  77 in
  let fill_r = List.nth right_kern 77 in
  let fill_g = List.nth glyphs     77 in
  let rec iter lk rk glyphs = match glyphs with
    | []      -> log_string "\\end{multicols}\n\\end{document}\n"
    | g :: gs -> 
        output_line (List.hd lk) (List.hd rk) g;
        output_line fill_l fill_r fill_g;
        iter (List.tl lk) (List.tl rk) gs
  in iter left_kern right_kern glyphs

let rec draw_chart l = match l with
  | []      -> log_string "\\par\\vskip 5mm\\hskip 0mm "
  | x :: xs -> 
      log_string "\\vrule width 2mm depth 0mm height ";
      log_num    (num_of_int 2 */ x);
      log_string "mm\n";
      draw_chart xs

let draw_glyph glyph = 
  log_string "\\vbox{%\n\\hbox{";
  log_uc_string (GlyphBitmap.print_to_string glyph (Unicode.UString.of_string "\\1") (Unicode.UString.of_string "\\0") (Unicode.UString.of_string "}%\n\\hbox{"));
  log_string "}}\n"

let output_glyphs lk rk glyphs = 
  let draw_line x g = 
    log_string "  \\hbox to 0pt{\\hskip ";
    log_num    (num_of_ints 13 10 */ num_of_int (x - g.g_min_x));
    log_string "mm\\vrule width 0.1mm height 0pt depth ";
    log_num    (num_of_ints 13 10 */ num_of_int (g.g_max_y - g.g_min_y + 1));
    log_string "mm\\hss}%\n"
  in
  let pow4 x = num_of_int x */ num_of_int x */ num_of_int x */ num_of_int x in
  log_string "\\documentclass[a4paper,10pt]{article}\n";
  log_string "\\usepackage{geometry}\n";
  log_string "\\geometry{left=1cm, right=1cm, top=1cm, bottom=1cm}\n";
  log_string "\\parindent0pt\n";
  log_string "\\begin{document}\n";
  log_string "\\baselineskip=0pt\n";
  log_string "\\lineskip=0pt\n";
  log_string "\\lineskiplimit=0pt\n";
  log_string "\\def\\1{\\vbox to 1.3mm{\\vss\\hbox to 1.3mm{\\hss\\tiny$\\bullet$\\hss}\\vss}}\n";
  log_string "\\def\\0{\\vbox to 1.3mm{\\hbox to 1.3mm{\\hfil}\\vfil}}\n";
  let rec iter lk rk glyphs = match glyphs with
    | []      -> ()
    | g :: gs -> 
        log_string "\n\n(";
        log_int    g.g_glyph;
        log_string ", ";
        log_string ")\n\n";
        log_string "\\vbox{\\vbox to 0pt{%\n";
        log_string "  \\vbox to 0pt{}%\n";
        log_string "  \\hbox{";
        draw_line (List.hd lk)                 g;
        draw_line (g.g_width - List.hd rk) g;
        draw_line (calc_center pow4 g)         g;
        log_string "}\\vss}%\n";
        draw_glyph g;
        log_string "}\\vfill\n";
        iter (List.tl lk) (List.tl rk) gs
  in iter lk rk glyphs;
  log_string "\\end{document}\n"

let () = ()
