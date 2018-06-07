theory Classy
imports "../multi-isabelle/Multi_Isabelle"
keywords
  "ML.class" :: thy_decl % "ML" and
  "ML.instance" :: thy_decl % "ML" and
  "ML.print_classes" :: diag
begin

ML_cond ("2016") \<open>
  val secure_use_text = Secure.use_text
  val context_set_thread_data = Context.setmp_thread_data
  fun ml_env_context return : use_context =
    {tune_source = #tune_source ML_Env.local_context,
     name_space = #name_space ML_Env.local_context,
     str_of_pos = #str_of_pos ML_Env.local_context,
     print = fn r => return := SOME r,
     error = #error ML_Env.local_context}
  val print_int = Markup.print_int
\<close>

ML_cond ("2016-1") \<open>
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

ML_cond ("2017", "2018-RC0") \<open>
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
