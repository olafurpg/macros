package scala.macros.internal.plugins.scalac.reflect

trait Definitions_v3 {
  self: ReflectToolkit_v3 =>
  import global._

  def macroEngine = "v3.0"

  def isDefMacro_v3(ddef: DefDef): Boolean = {
    ddef.mods.annotations.exists { annot =>
      typer.typed(annot).tpe <:< typeOf[scala.macros.v3]
    }
  }
}
