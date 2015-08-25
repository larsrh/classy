theory Classy
imports Pure
keywords
  "ML.class" :: thy_decl % "ML" and
  "ML.instance" :: thy_decl % "ML" and
  "ML.print_classes" :: diag
begin

ML_file "ml_types.ML"
ML_file "classy.ML"

end