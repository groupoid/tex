
include String

let match_suffix name suf =
  let len_n = length name in
  let len_s = length suf in
  len_n >= len_s && sub name (len_n - len_s) len_s = suf
