open Runtime
open Unicode.UTypes

let tables = [
  (UString.uc_string_of_ascii "en", Hyphenation.load_hyphen_table "en");
  (UString.uc_string_of_ascii "british", Hyphenation.load_hyphen_table "british")
]
