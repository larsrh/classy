theory Classy
imports Multi_Isabelle.Multi_Isabelle
keywords
  "ML.class" :: thy_decl % "ML" and
  "ML.instance" :: thy_decl % "ML" and
  "ML.print_classes" :: diag
begin

(* FIXME unfold in ML files *)
ML\<open>
  val secure_use_text = ML_Compiler0.ML
  val context_set_thread_data = Context.setmp_generic_context
  fun ml_env_context return : ML_Compiler0.context =
    {name_space = #name_space ML_Env.context,
     print_depth = SOME 1000000,
     here = #here ML_Env.context,
     print = fn r => return := SOME r,
     error = #error ML_Env.context}
  val print_int = Value.print_int
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
