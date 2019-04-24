theory Classy
imports Multi_Isabelle.Multi_Isabelle
keywords
  "ML.class" :: thy_decl % "ML" and
  "ML.instance" :: thy_decl % "ML" and
  "ML.print_classes" :: diag
begin

ML_cond ("2017", "2018") \<open>
  val input_source_content = Input.source_content
\<close>

ML_cond ("2019*") \<open>
  val input_source_content = fst o Input.source_content
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
