package scala.macros.internal
package plugins.scalac

import scala.macros.internal.plugins.scalac.reflect.ReflectToolkit_v3
import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{Plugin => NscPlugin}
import scala.macros.internal.plugins.scalac.typechecker.AnalyzerPlugins_v3

class Plugin(val global: Global) extends NscPlugin with ReflectToolkit_v3 with AnalyzerPlugins_v3 {
  val name = "scalamacros-plugins-scalac"
  val description = "Implementation of new-style Scala macros for scalac"
  val components = Nil
  global.analyzer.addMacroPlugin(MacroPlugin_v3)
}
