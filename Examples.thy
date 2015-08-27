theory Examples
imports Classy
begin

ML.print_classes

ML\<open>format @{ML.resolve \<open>string list\<close> :: pretty} ["a", "b"] |> Pretty.writeln\<close>

end