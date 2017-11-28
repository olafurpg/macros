package scala.macros.internal.plugins.scalac.reflect

import scala.tools.nsc.typechecker.Fingerprint

trait MacroPickle_v3 { self: ReflectToolkit_v3 =>
  import global._
  import treeInfo._
  import analyzer.MacroImplBinding

  case class MacroImplResolutionException(pos: Position, msg: String) extends Exception
  object DefMacro_v3 {
    private def refPart(tree: Tree): Tree = tree match {
      case TypeApply(fun, _) => refPart(fun)
      case ref: RefTree => ref
      case _ => EmptyTree
    }
    def unapply(tree: Tree): Boolean = refPart(tree) match {
      case _: RefTree =>
        // TODO(olafur) validate shape is:
        // (c: Expansion)(term: tpd.Term, arg2: tpd.Term)(implicit ev: TypeTag[T]): Term
        true
    }
  }

  def isMacroContext_v3(tpe: Type): Boolean =
    tpe <:< typeOf[scala.macros.Expansion]

  def transformTypeTagEvidenceParams_v3(
      macroImplRef: Tree,
      transform: (Symbol, Symbol) => Symbol
  ): List[List[Symbol]] = {
    val runDefinitions = currentRun.runDefinitions
    import runDefinitions._

    val MacroContextUniverse = definitions.MacroContextUniverse
    val treeInfo.MacroImplReference(isBundle, _, _, macroImpl, _) =
      macroImplRef
    val paramss = macroImpl.paramss
    val ContextParam = paramss match {
      case Nil | _ :+ Nil =>
        NoSymbol // no implicit parameters in the signature => nothing to do
      case _ if isBundle => macroImpl.owner.tpe member nme.c
      case (cparam :: _) :: _ if definitions.isMacroContextType(cparam.tpe) => cparam
      // +scalac deviation
      case (cparam :: _) :: _ if isMacroContext_v3(cparam.tpe) => cparam
      // -scalac deviation
      case _ =>
        NoSymbol // no context parameter in the signature => nothing to do
    }
    val MacroTypeTag_v3 = rootMirror.getClassIfDefined("scala.macros.TypeTag")
    def transformTag(param: Symbol): Symbol = {
      param.tpe.dealias match {
        case TypeRef(
            SingleType(SingleType(_, ContextParam), MacroContextUniverse),
            WeakTypeTagClass,
            targ :: Nil
            ) =>
          transform(param, targ.typeSymbol)
        // +scalac deviation
        case TypeRef(_, MacroTypeTag_v3, targ :: Nil) =>
          transform(param, targ.typeSymbol)
        // -scalac deviation
        case _ => param
      }
    }
    ContextParam match {
      case NoSymbol => paramss
      case _ =>
        paramss.last map transformTag filter (_.exists) match {
          case Nil => paramss.init
          case transformed => paramss.init :+ transformed
        }
    }
  }

  object TypedTerm_v3 {
    def unapply(arg: Type): Boolean =
      arg <:< typeOf[scala.macros.tpd.Term]
  }

  /** Adaptation of MacroImplBinding.pickle that handles v3 context. */
  def pickle_v3(macroImplRef: Tree): Tree = {
    val runDefinitions = currentRun.runDefinitions
    import runDefinitions._
    val MacroImplReference(isBundle, isBlackbox, owner, macroImpl, targs) =
      macroImplRef

    // todo. refactor when fixing scala/bug#5498
    def className: String = {
      def loop(sym: Symbol): String = sym match {
        case sym if sym.isTopLevel =>
          val suffix = if (sym.isModuleClass) "$" else ""
          sym.fullName + suffix
        case sym =>
          val separator = if (sym.owner.isModuleClass) "" else "$"
          loop(sym.owner) + separator + sym.javaSimpleName.toString
      }

      loop(owner)
    }
    import scala.tools.nsc.typechecker.Fingerprint._
    import definitions.RepeatedParamClass

    def signature: List[List[Fingerprint]] = {
      def fingerprint(tpe: Type): Fingerprint = {
        tpe.dealiasWiden match {
          case TypeRef(_, RepeatedParamClass, underlying :: Nil) =>
            fingerprint(underlying)
          case ExprClassOf(_) => LiftedTyped
          case TreeType() => LiftedUntyped
          // +scalac deviation
          case TypedTerm_v3() => LiftedUntyped
          // -scalac deviation
          case _ => Other
        }
      }

      val transformed =
        transformTypeTagEvidenceParams_v3(macroImplRef, (param, tparam) => tparam)
      mmap(transformed)(p => if (p.isTerm) fingerprint(p.info) else Tagged(p.paramPos))
    }

    println(s"SIGNATURE: $signature")
    val payload = List[(String, Any)](
      "macroEngine" -> macroEngine,
      "isBundle" -> isBundle,
      "isBlackbox" -> isBlackbox,
      "className" -> className,
      "methodName" -> macroImpl.name.toString,
      "signature" -> signature
    )

    // the shape of the nucleus is chosen arbitrarily. it doesn't carry any payload.
    // it's only necessary as a stub `fun` for an Apply node that carries metadata in its `args`
    // so don't try to find a program element named "macro" that corresponds to the nucleus
    // I just named it "macro", because it's macro-related, but I could as well name it "foobar"
    val nucleus = Ident(newTermName("macro"))
    val wrapped = Apply(nucleus, payload map {
      case (k, v) =>
        Assign(MacroImplBinding.pickleAtom(k), MacroImplBinding.pickleAtom(v))
    })
    val pickle = gen.mkTypeApply(wrapped, targs map (_.duplicate))

    // assign NoType to all freshly created AST nodes
    // otherwise pickler will choke on tree.tpe being null
    // there's another gotcha
    // if you don't assign a ConstantType to a constant
    // then pickling will crash
    new Transformer {
      override def transform(tree: Tree) = {
        tree match {
          case Literal(const @ Constant(x)) if tree.tpe == null =>
            tree setType ConstantType(const)
          case _ if tree.tpe == null => tree setType NoType
          case _ => ;
        }
        super.transform(tree)
      }
    }.transform(pickle)
  }
}
