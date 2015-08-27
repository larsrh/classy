theory Examples
imports Classy
begin

ML.print_classes

ML\<open>format @{ML.resolve \<open>string list\<close> :: pretty} ["a", "b"] |> Pretty.writeln\<close> (* okay *)

context begin

  (* conflicting instance *)
  ML.instance \<open>Pretty_Class.string\<close> :: pretty

  ML\<open>@{ML.resolve \<open>string\<close> :: pretty}\<close> (* error *)

end

end