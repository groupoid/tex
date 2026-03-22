open Tag

type script = {
  s_tag : Tag.tag;
  s_default_lang : feature option;
  s_langs : (Tag.tag * feature) list;
}

and feature_list = {
  ls_required : feature option;
  ls_features : feature list;
}

and feature = {
  f_tag : Tag.tag;
  f_params : int;
  f_lookups : int array;
  f_script_list : Tag.tag TagMap.t;
}
