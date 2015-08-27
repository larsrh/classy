datatype 'a pretty_class = Pretty of ('a -> Pretty.T)

fun format (Pretty f) = f

structure Pretty_Class = struct

val string: string pretty_class = Pretty Pretty.str
val pretty: Pretty.T pretty_class = Pretty I
fun list (Pretty f): 'a list pretty_class =
  Pretty (fn xs => Pretty.block (Pretty.str "[" :: Pretty.separate "," (map f xs) @ [Pretty.str "]"]))

end