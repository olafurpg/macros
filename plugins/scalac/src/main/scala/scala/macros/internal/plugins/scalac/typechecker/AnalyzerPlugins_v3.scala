package scala.macros.internal
package plugins.scalac
package typechecker

import scala.macros.internal.plugins.scalac.reflect.ReflectToolkit_v3
import scala.reflect.internal.util.ScalaClassLoader

trait AnalyzerPlugins_v3 { self: ReflectToolkit_v3 =>
  import global._
  import analyzer.{MacroPlugin => _}
  import analyzer._

  object MacroPlugin_v3 extends analyzer.MacroPlugin {
    private lazy val pluginMacroClassloader: ClassLoader = {
      val classpath = global.classPath.asURLs
      macroLogVerbose("macro classloader: initializing from -cp: %s".format(classpath))
      ScalaClassLoader.fromURLs(classpath, this.getClass.getClassLoader)
    }

    private class PluginRuntimeResolver(sym: Symbol) extends MacroRuntimeResolver(sym) {
      override def resolveJavaReflectionRuntime(defaultClassLoader: ClassLoader): MacroRuntime = {
        // NOTE: defaultClassLoader only includes libraryClasspath + toolClasspath.
        // We need to include pluginClasspath, so that the new macro shim can instantiate
        // ScalacUniverse and ScalacExpansion.
        super.resolveJavaReflectionRuntime(pluginMacroClassloader)
      }
    }

    private val newMacroRuntimesCache = perRunCaches.newWeakMap[Symbol, MacroRuntime]
    override def pluginsMacroRuntime(expandee: Tree): Option[MacroRuntime] = {
      println(s"=> pluginsMacroRuntime $expandee")
      def mkResolver = new PluginRuntimeResolver(expandee.symbol).resolveRuntime()
      Some(newMacroRuntimesCache.getOrElseUpdate(expandee.symbol, mkResolver))
    }

    override def pluginsTypedMacroBody(
        typer: Typer,
        ddef: DefDef
    ): Option[Tree] = {
      println(s"=> pluginsTypedMacroBody $ddef")
      if (!isDefMacro_v3(ddef)) None
      else {
        new TypedMacroBodyImpl(typer, ddef).run()
      }
    }

    override def pluginsMacroArgs(
        typer: Typer,
        expandee: Tree
    ): Option[global.analyzer.MacroArgs] = {
      println(s"=> pluginsMacroArgs ${isDefMacro_v3(expandee)}")
      if (!isDefMacro_v3(expandee)) None
      else {
        val macroDef = expandee.symbol
        val standardArgs = standardMacroArgs(typer, expandee)
        val binding = loadMacroImplBinding(macroDef).get
        macroArgs_v3(typer, expandee, standardArgs, binding)
      }
    }
  }
}
