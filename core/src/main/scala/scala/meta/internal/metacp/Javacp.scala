package scala.meta.internal.metacp

import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.SimpleFileVisitor
import java.nio.file.attribute.BasicFileAttributes
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.meta.internal.semanticdb3.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb3.SymbolInformation.{Property => p}
import scala.meta.internal.{semanticdb3 => s}
import scala.tools.asm.ClassReader
import scala.tools.asm.signature.SignatureReader
import scala.tools.asm.signature.SignatureVisitor
import scala.tools.asm.tree.ClassNode
import scala.tools.asm.{Opcodes => o}
import org.langmeta.internal.io.PathIO
import org.langmeta.io.AbsolutePath

object Javacp {

  def asmNodeFromBytes(bytes: Array[Byte]): ClassNode = {
    val node = new ClassNode()
    new ClassReader(bytes).accept(
      node,
      ClassReader.SKIP_DEBUG |
        ClassReader.SKIP_FRAMES |
        ClassReader.SKIP_CODE
    )
    node
  }

  def existential = s.Type(
    s.Type.Tag.EXISTENTIAL_TYPE,
    existentialType = Some(
      s.ExistentialType(
        tpe = Some(ref("?"))
        // ???
      )
    )
  )

  def ref(symbol: String, args: List[s.Type] = Nil): s.Type = {
    s.Type(
      s.Type.Tag.TYPE_REF,
      typeRef = Some(s.TypeRef(prefix = None, symbol, args))
    )
  }

  object void extends SignatureVisitor(o.ASM5)
  class JType(
      var isArray: Boolean,
      var symbol: String,
      val args: ListBuffer[JType]) {
    override def toString: String = {
      val suffix = if (isArray) "[]" else ""
      if (args.isEmpty) symbol + suffix
      else symbol + args.mkString("<", ", ", ">") + suffix
    }
  }

  sealed trait SignatureMode
  object SignatureMode {
    case object ParameterType extends SignatureMode
    case object FormalType extends SignatureMode
    case object ReturnType extends SignatureMode
  }
  class SemanticdbSignatureVisitor extends SignatureVisitor(o.ASM5) {
    import SignatureMode._
    def newTpe = new JType(false, "", ListBuffer.empty[JType])
    var owners = List.empty[JType]
    var tpe: JType = newTpe
    def returnType = tpe
    val parameterTypes = ListBuffer.empty[JType]
    val formatTypeParameters = ListBuffer.empty[String]
    var mode: SignatureMode = FormalType


    override def visitParameterType(): SignatureVisitor = {
      mode = ParameterType
      pprint.log("Parameter type")
      tpe = newTpe
      owners = tpe :: owners
      parameterTypes += tpe
      this
    }

    override def visitClassType(name: String): Unit = {
      pprint.log(name)
      tpe.symbol = ssym(name)
    }

    override def visitFormalTypeParameter(name: String): Unit = {
      pprint.log(name)
      formatTypeParameters += name
    }

    override def visitTypeArgument(wildcard: Char): SignatureVisitor = {
      pprint.log(wildcard)
      val arg = newTpe
      tpe.args += arg
      tpe = arg
      owners = tpe :: owners
      this
    }

    override def visitArrayType(): SignatureVisitor = {
      tpe.isArray = true
      pprint.log("Array type")
      this
    }

    override def visitTypeArgument(): Unit = {
      pprint.log("Type Argument")
    }

    override def visitEnd(): Unit = {
      mode match {
        case ParameterType =>
          tpe = owners.head
          owners = owners.tail
        case ReturnType =>
          pprint.log(tpe)
          pprint.log(owners)
          tpe = owners.head
          owners = owners.tail
        case _ =>
      }
      pprint.log("END")
    }

    def endType(): Unit = {

    }

    override def visitTypeVariable(name: String): Unit = {
      tpe.symbol = ssym(name)
      pprint.log(owners)
      owners = owners.tail
      pprint.log(name)
    }

    override def visitExceptionType(): SignatureVisitor = {
      pprint.log("exceptionType")
      this
    }

    override def visitSuperclass(): SignatureVisitor = {
      pprint.log("superClass")
      this
    }

    override def visitInterface(): SignatureVisitor = {
      pprint.log("Interface")
      this
    }

    override def visitInterfaceBound(): SignatureVisitor = {
      pprint.log("interface bound")
      this
    }

    override def visitInnerClassType(name: String): Unit = {
      pprint.log(name)
    }

    override def visitClassBound(): SignatureVisitor = {
      pprint.log("classBound")
      this
    }

    override def visitBaseType(descriptor: Char): Unit = {
      pprint.log(descriptor)
    }

    override def visitReturnType(): SignatureVisitor = {
      mode = ReturnType
      pprint.log("Return type")
      tpe = newTpe
      owners = tpe :: owners
      pprint.log(owners)
      this
    }

  }

  def array(tpe: s.Type) =
    ref("_root_.scala.Array#", tpe :: Nil)

  def ssym(string: String): String =
    "_root_." + string.replace('/', '.').replace('$', '.') + "."

  implicit class XtensionAccess(n: Int) {
    def hasFlag(flag: Int): Boolean =
      (flag & n) != 0
  }

  def process(root: Path, file: Path): s.TextDocument = {
    val bytes = Files.readAllBytes(file)
    val node = asmNodeFromBytes(bytes)
    val buf = ArrayBuffer.empty[s.SymbolInformation]
    val classSymbol = ssym(node.name)
    val className = {
      val idx = {
        val dollar = node.name.lastIndexOf('$')
        if (dollar < 0) node.name.lastIndexOf('/')
        else dollar
      }
      if (idx < 0) ???
      else node.name.substring(idx + 1)
    }
    val classOwner =
      ssym(node.name.substring(0, node.name.length - className.length - 1))
    val classKind =
      if (node.access.hasFlag(o.ACC_INTERFACE)) k.TRAIT
      else k.CLASS
    node.methods.asScala.foreach { method =>
      if (method.signature != null) {
        pprint.log(method.signature)
        val signatureReader = new SignatureReader(method.signature)
        val v = new SemanticdbSignatureVisitor
        signatureReader.accept(v)
        pprint.log(v.tpe)
        pprint.log(v.parameterTypes)
        pprint.log(v.returnType)
        pprint.log(v.owners)
      }
      val descriptor = method.desc
      val methodSymbol = classSymbol + method.name + "(" + descriptor + ")."
      val methodKind = k.DEF
      if (method.parameters != null) {
        method.parameters.asScala.zipWithIndex.foreach {
          case (param, i) =>
            val paramName =
              if (param.name == null) "arg" + i
              else param.name
            val paramSymbol = methodSymbol + "(" + paramName + ")"
            val paramKind = k.PARAMETER
            buf += s.SymbolInformation(
              symbol = paramSymbol,
              kind = paramKind,
              name = paramName,
              owner = methodSymbol
            )
        }
      }
      buf += s.SymbolInformation(
        symbol = methodSymbol,
        kind = methodKind,
        name = method.name,
        owner = classSymbol
      )
    }

    node.fields.asScala.foreach { field =>
      val fieldSymbol = classSymbol + field.name + "."
      val fieldKind = k.VAR
      buf += s.SymbolInformation(
        symbol = fieldSymbol,
        kind = fieldKind,
        name = field.name,
        owner = classOwner
      )
    }

    val decls = buf.map(_.symbol)
    val parents = (node.superName +: node.interfaces.asScala).map { parent =>
      val symbol = ssym(parent)
      s.Type(s.Type.Tag.TYPE_REF, typeRef = Some(s.TypeRef(symbol = symbol)))
    }

    val classTpe = s.Type(
      tag = s.Type.Tag.STRUCTURAL_TYPE,
      structuralType = Some(
        s.StructuralType(
          declarations = decls,
          parents = parents
        ))
    )

    buf += s.SymbolInformation(
      symbol = classSymbol,
      kind = classKind,
      name = className,
      owner = classOwner,
      tpe = Some(classTpe)
    )

    val uri = root.relativize(file).toString
    s.TextDocument(
      schema = s.Schema.SEMANTICDB3,
      uri = uri,
      symbols = buf
    )
  }

  def main(args: Array[String]): Unit = {
    run(args)
//    val signature =
//      "<T:Ljava/lang/Object;>(Ljava/util/ArrayList<Ljava/util/ArrayList<[TT;>;>;)Ljava/util/ArrayList<Ljava/util/ArrayList<[TT;>;>;"
//    val sr = new SignatureReader(signature)
//    val v = new SemanticdbSignatureVisitor
//    sr.accept(v)
  }

  def run(args: Array[String]): Unit = {
    val root = AbsolutePath("core/target/scala-2.12/classes/test")
    Files.walkFileTree(
      root.toNIO,
      new SimpleFileVisitor[Path] {
        override def visitFile(
            file: Path,
            attrs: BasicFileAttributes): FileVisitResult = {
          if (PathIO.extension(file) == "class") {

            val db = process(root.toNIO, file)
//            if (!file.toString.contains('$')) {
//              pprint.log(db.toProtoString)
//            }
          }
          FileVisitResult.CONTINUE
        }
      }
    )

  }
}
