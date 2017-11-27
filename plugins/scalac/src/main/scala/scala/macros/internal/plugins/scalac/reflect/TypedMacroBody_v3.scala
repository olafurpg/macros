package scala.macros.internal.plugins.scalac.reflect

import scala.reflect.macros.compiler.DefaultMacroCompiler
import scala.reflect.internal.Flags
import scala.util.control.NonFatal

trait TypedMacroBody_v3 {
  self: ReflectToolkit_v3 =>
  import global._
  class TypedMacroBodyImpl(
      typer: global.analyzer.Typer,
      ddef: global.analyzer.global.DefDef
  ) {
    val macroDef = ddef.symbol
    def fail() = {
      if (macroDef != null) macroDef setFlag Flags.IS_ERROR
      ddef setType ErrorType; EmptyTree
    }
    def success(macroImplRef: Tree) = {
      // +scalac deviation
      val pickle = pickle_v3(macroImplRef) // custom socrates pickle.
      // -scalac deviation
      val annotInfo = AnnotationInfo(definitions.MacroImplAnnotation.tpe, List(pickle), Nil)
      macroDef withAnnotation annotInfo
      println(s"INFO: $annotInfo")
      macroImplRef
    }
    private val macroDdef1: ddef.type = ddef
    private val typer1: typer.type = typer
    private val macroCompiler = new {
      val global: self.global.type = self.global
      val typer: self.global.analyzer.Typer =
        typer1.asInstanceOf[self.global.analyzer.Typer]
      val macroDdef: self.global.DefDef = macroDdef1
    } with DefaultMacroCompiler {
      override def resolveMacroImpl: global.Tree = {
        def tryCompile(compiler: MacroImplRefCompiler): scala.util.Try[Tree] = {
          try {
            println("Trying compile!")
            // +scalac deviation
            /* compiler.validateMacroImplRef(); skip validation */
            // -scalac deviation
            scala.util.Success(compiler.macroImplRef)
          } catch {
            case NonFatal(ex) if ex.getClass.getName.contains("MacroImplResolution") =>
              scala.util.Failure(ex)
          }
        }
        println("resolving... ")
        val vanillaImplRef = MacroImplRefCompiler(macroDdef.rhs.duplicate, isImplBundle = false)
        val vanillaResult = tryCompile(vanillaImplRef)
        try {
          vanillaResult.get
        } catch {
          case MacroImplResolutionException(pos, msg) =>
            context.error(pos, msg)
            EmptyTree
        }
      }
    }
    def run(): Option[Tree] = {
      val macroImplRef = macroCompiler.resolveMacroImpl
      if (macroImplRef.isEmpty) fail() else success(macroImplRef)
      val untypedMacroImplRef = ddef.rhs.duplicate
      val typed =
        typer.silent(
          _.typed(analyzer.markMacroImplRef(untypedMacroImplRef)),
          reportAmbiguousErrors = false
        )
      typed match {
        case analyzer.SilentResultValue(macroImplRef @ DefMacro_v3()) =>
          Some(macroImplRef)
        case analyzer.SilentResultValue(macroImplRef) =>
          reporter.error(macroImplRef.pos, showCode(macroImplRef, printTypes = true))
          None
        case analyzer.SilentTypeError(err) =>
          reporter.error(err.errPos, err.errMsg)
          None
      }
    }
  }
}
