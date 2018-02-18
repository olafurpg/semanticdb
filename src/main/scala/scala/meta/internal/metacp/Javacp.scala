package scala.meta.internal.metacp

import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.SimpleFileVisitor
import java.nio.file.attribute.BasicFileAttributes
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.meta.internal.semanticdb3.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb3.SymbolInformation.{Property => p}
import scala.meta.internal.{semanticdb3 => s}
import scala.tools.asm.ClassReader
import scala.tools.asm.tree._
import scala.tools.asm.{Opcodes => o}
import org.langmeta.internal.io.PathIO
import org.langmeta.io.AbsolutePath

object Javacp {
  def asmNodeFromBytes(bytes: Array[Byte]): ClassNode = {
    val node = new ClassNode()
    new ClassReader(bytes).accept(node,
                                  ClassReader.SKIP_DEBUG |
                                    ClassReader.SKIP_FRAMES |
                                    ClassReader.SKIP_CODE)
    node
  }

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
      if (node.access.hasFlag(o.ACC_INTERFACE)) k.TRAIT else k.CLASS
    node.methods.asScala.foreach { method =>
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
    implicit val cwd: AbsolutePath =
      PathIO.workingDirectory.resolve("..").resolve("scalameta")
    val root = AbsolutePath(
      "semanticdb/integration/target/scala-2.12/classes/cp")
    Files.walkFileTree(
      root.toNIO,
      new SimpleFileVisitor[Path] {
        override def visitFile(file: Path,
                               attrs: BasicFileAttributes): FileVisitResult = {
          if (PathIO.extension(file) == "class") {
            val db = process(root.toNIO, file)
            if (!file.toString.contains('$')) {
              println(db.toProtoString)
            }
          }
          FileVisitResult.CONTINUE
        }
      }
    )

  }
}
