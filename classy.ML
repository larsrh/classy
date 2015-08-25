signature CLASSY = sig
  val register_class: binding -> ML_Types.ml_type -> local_theory -> local_theory
  val register_class_cmd: binding -> string -> local_theory -> local_theory

  val register_instance_cmd: xstring * Position.T -> binding  -> Input.source -> local_theory -> local_theory

  val print_classes: Context.generic -> unit
end

structure Classy: CLASSY = struct

type class_table = (ml_type * (ml_type * Input.source) Name_Space.table) Name_Space.table

exception DUP

structure Classes = Generic_Data
(
  type T = class_table
  val empty = Name_Space.empty_table "ML class"
  val merge = Name_Space.join_tables (fn _ => raise DUP) (* FIXME consistency check *)
  val extend = I
)

fun pretty_classes context =
  let
    val table = Classes.get context
    val ctxt = Context.proof_of context
    val space = Name_Space.space_of_table table
    val entries = Name_Space.extern_table true ctxt table

    fun pretty_class ((class, _), (typ, sub_table)) =
      let
        val header = Pretty.block [Name_Space.pretty ctxt space class, Pretty.str ":", Pretty.brk 1, ML_Types.pretty typ]

        val sub_space = Name_Space.space_of_table sub_table
        val sub_entries = Name_Space.extern_table true ctxt sub_table

        fun pretty_instance ((instance, _), (typ, _)) =
          Pretty.item [Name_Space.pretty ctxt sub_space instance, Pretty.str ":", Pretty.brk 1, ML_Types.pretty typ]

        val instances = map pretty_instance sub_entries
      in
        Pretty.item (Pretty.fbreaks (header :: instances))
      end
  in
    Pretty.big_list "Classes" (map pretty_class entries)
  end

val print_classes =
  Pretty.writeln o pretty_classes

fun register_class binding typ =
  let
    val name =
      (case typ of
        ML_Types.Con (name, [ML_Types.Var _]) => name
      | _ => error "Malformed type")
    fun decl _ context =
      let
        val classes = Classes.get context
        val table = Name_Space.empty_table ("ML instances for " ^ name)
        val (_, classes') = Name_Space.define context true (binding, (typ, table)) classes
      in
        Classes.put classes' context
      end
  in
    Local_Theory.declaration {syntax = false, pervasive = false} decl
  end

fun register_class_cmd binding typ =
  register_class binding (ML_Types.read_ml_type typ)

fun register_instance_cmd raw_binding binding source lthy =
  let
    val typ = ML_Types.ml_type_of lthy (Input.source_content source)
    (* doesn't have any effect except for markup *)
    val _ = ML_Context.eval_source_in (SOME lthy) ML_Compiler.flags source

    fun decl _ context =
      let
        val classes = Classes.get context
        val (key, _) = Name_Space.check context classes raw_binding
        val upd = Name_Space.define context true (binding, (typ, source)) #> snd
        val classes' = Name_Space.map_table_entry key (apsnd upd) classes
      in Classes.put classes' context end
  in
    Local_Theory.declaration {syntax = false, pervasive = false} decl lthy
  end

val _ =
  Outer_Syntax.local_theory @{command_keyword "ML.class"} "register new type class"
    (Parse.binding --| @{keyword "="} -- Parse.cartouche
      >> (fn (binding, typ) => register_class_cmd binding typ))

val _ =
  let
    val opt_binding =
      Parse.binding --| @{keyword "="} ||
        Parse.position (Scan.succeed ()) >>
          (fn ((), pos) => Binding.make ("instance" ^ Markup.print_int (serial ()), pos))
  in
    Outer_Syntax.local_theory @{command_keyword "ML.instance"} "register new type class instance"
      (opt_binding -- (Parse.ML_source --| @{keyword "::"} -- Parse.position Parse.name)
        >> (fn (binding, (source, name)) => register_instance_cmd name binding source))
  end

val _ =
  Outer_Syntax.command @{command_keyword "ML.print_classes"} "print all registered classes"
    (Scan.succeed (Toplevel.keep (print_classes o Toplevel.generic_theory_of)))

end