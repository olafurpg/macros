package scala.macros.internal
package plugins.scalac
package reflect

import scala.macros.internal.plugins.scalac.typechecker.AnalyzerPlugins_v3
import scala.tools.nsc.Global

trait ReflectToolkit_v3
    extends AnalyzerPlugins_v3
    with Definitions_v3
    with TypedMacroBody_v3
    with MacroArgs_v3
    with MacroPickle_v3 {
  val global: Global
}
