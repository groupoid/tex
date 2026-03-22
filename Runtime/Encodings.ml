open Unicode
open GlyphMetric
open Substitute

module GlyphSpecTrie = DynamicTrie
type glyph_spec_trie = int GlyphSpecTrie.t

let undefined = Charmap.create `Undef
let uc_to_ot1 = Charmap.create `Undef
let uc_to_t1 = Charmap.create `Undef
let uc_to_ott = Charmap.create `Undef
let uc_to_oms = Charmap.create `Undef
let uc_to_oml = Charmap.create `Undef
let fake = Charmap.create `Undef

let ot1_to_uc = [||]
let t1_to_uc = [||]
let ott_to_uc = [||]
let oms_to_uc = [||]
let oml_to_uc = [||]

let fake_encoding _ = Charmap.create `Undef
let charmap_encoding _ _ = `Undef
let array_decoding _ _ = [||]

let raw_encoding char = `Char char
let raw_decoding glyph = match glyph with `Char c -> [|c|] | _ -> [||]
