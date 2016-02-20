theory Classy
imports Pure "lib/multi-isabelle/Multi_Isabelle"
keywords
  "ML.class" :: thy_decl % "ML" and
  "ML.instance" :: thy_decl % "ML" and
  "ML.print_classes" :: diag
begin

ML_cond ("2015") \<open>
  val sort_by = sort_wrt
  fun secure_use_text context {line, file, verbose, debug} txt =
    Secure.use_text context (line, file) verbose txt
\<close>

ML_cond ("2016") \<open>
  val secure_use_text = Secure.use_text
\<close>

ML_file "ml_types.ML"
ML_file "classy.ML"

setup \<open>Classy.setup\<close>

ML_file "pretty_class.ML"

ML.class pretty = \<open>'a pretty_class\<close>

ML.instance \<open>Pretty_Class.string\<close> :: pretty
ML.instance \<open>Pretty_Class.pretty\<close> :: pretty
ML.instance \<open>Pretty_Class.list\<close> :: pretty

end