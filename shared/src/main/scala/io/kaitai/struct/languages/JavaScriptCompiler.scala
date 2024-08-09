package io.kaitai.struct.languages

import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.JavaScriptTranslator
import io.kaitai.struct.{ClassTypeProvider, RuntimeConfig, Utils}

class JavaScriptCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with ObjectOrientedLanguage
    with UpperCamelCaseClasses
    with SingleOutputFile
    with UniversalFooter
    with FetchInstances
    with EveryWriteIsExpression
    with GenericChecks
    with UniversalDoc
    with AllocateIOLocalVar
    with EveryReadIsExpression
    with SwitchIfOps {
  import JavaScriptCompiler._

  override val translator = new JavaScriptTranslator(typeProvider)

  override def indent: String = "  "
  override def outFileName(topClassName: String): String = s"${type2class(topClassName)}.js"

  /** See [[subIOWriteBackHeader]] => the code generated when `true` will be inside the definition
   * of the "write back handler" callback function. */
  private var inSubIOWriteBackHandler = false

  override def universalFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def outImports(topClass: ClassSpec) = {
    val impList = importList.toList
    val quotedImpList = impList.map((x) => s"'$x'")
    val defineArgs = quotedImpList.mkString(", ")
    val moduleArgs = quotedImpList.map((x) => s"require($x)").mkString(", ")
    val argClasses = impList.map((x) => x.split('/').last)
    val rootArgs = argClasses.map((x) => s"root.$x").mkString(", ")

    "(function (root, factory) {\n" +
      indent + "if (typeof define === 'function' && define.amd) {\n" +
      indent * 2 + s"define([$defineArgs], factory);\n" +
      indent + "} else if (typeof module === 'object' && module.exports) {\n" +
      indent * 2 + s"module.exports = factory($moduleArgs);\n" +
      indent + "} else {\n" +
      indent * 2 + s"root.${types2class(topClass.name)} = factory($rootArgs);\n" +
      indent + "}\n" +
      s"}(typeof self !== 'undefined' ? self : this, function (${argClasses.mkString(", ")}) {"
  }

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s"// $headerComment")
    outHeader.puts

    importList.add("kaitai-struct/KaitaiStream")
  }

  override def fileFooter(name: String): Unit = {
    out.puts(s"return ${type2class(name)};")
    out.puts("}));")
  }

  override def opaqueClassDeclaration(classSpec: ClassSpec): Unit = {
    val className = type2class(classSpec.name.head)
    importList.add(s"./$className")
  }

  override def classHeader(name: List[String]): Unit = {
    val shortClassName = type2class(name.last)

    val addNameExpr = if (name.size > 1) {
      s" = ${types2class(name.takeRight(2))}"
    } else {
      ""
    }

    out.puts
    out.puts(s"var $shortClassName$addNameExpr = (function() {")
    out.inc
  }

  override def fetchInstancesHeader(): Unit = {
    out.puts
    out.puts(s"${type2class(typeProvider.nowClass.name.last)}.prototype._fetchInstances = function() {")
    out.inc
  }

  // TODO: replace with universal footer?
  override def fetchInstancesFooter(): Unit = {
    writeFooter()
  }

  override def attrInvokeFetchInstances(baseExpr: Ast.expr, exprType: DataType, dataType: DataType): Unit = {
    val expr = expression(baseExpr)
    out.puts(s"$expr._fetchInstances()")
  }

  override def attrInvokeInstance(instName: InstanceIdentifier): Unit = {
    out.puts(s"_ = this.${publicMemberName(instName)}")
  }

  override def writeHeader(endian: Option[FixedEndian], isEmpty: Boolean) = {
    val suffix = endian match {
      case Some(e) => "_" + Utils.upperCamelCase(e.toSuffix)
      case None => ""
    }
    out.puts(s"${type2class(typeProvider.nowClass.name.last)}.prototype._write__seq$suffix = function() {")
    out.inc
  }

  override def checkHeader(): Unit = {
    out.puts
    out.puts(s"${type2class(typeProvider.nowClass.name.last)}.prototype._check = function() {")
    out.inc
  }

  override def checkFooter(): Unit = {
    out.puts("}")
    out.dec
  }
  override def writeInstanceFooter(): Unit = {
    out.puts("}")
    out.dec
  }
  override def checkInstanceFooter(): Unit = {
    out.puts("}")
    out.dec
  }

  override def writeInstanceHeader(instName: InstanceIdentifier): Unit = {
    out.puts
    out.puts(s"${type2class(typeProvider.nowClass.name.last)}.prototype._write_${publicMemberName(instName)} = function() {")
    out.inc
    instanceClearWriteFlag(instName)
  }

  override def checkInstanceHeader(instName: InstanceIdentifier): Unit = {
    out.puts
    out.puts(s"${type2class(typeProvider.nowClass.name.last)}.prototype._check${publicMemberName(instName)} = function() {")
    out.inc
    instanceClearWriteFlag(instName)
  }

  override def writeFooter() = {
    out.dec
    out.puts("}")
  }

  override def classFooter(name: List[String]): Unit = {
    out.puts
    out.puts(s"return ${type2class(name.last)};")
    out.dec
    out.puts("})();")
  }

  override def classConstructorHeader(name: List[String], parentClassName: DataType, rootClassName: List[String], isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {
    val endianSuffix = if (isHybrid) {
      ", _is_le"
    } else {
      ""
    }

    val paramsList = Utils.join(params.map((p) => paramName(p.id)), ", ", ", ", "")

    out.puts(s"function ${type2class(name.last)}(_io, _parent, _root$endianSuffix$paramsList) {")
    out.inc
    out.puts("this._io = _io;")
    out.puts("this._parent = _parent;")
    if (name == rootClassName) {
      out.puts("this._root = _root || this;")
    } else {
      out.puts("this._root = _root;")
    }

    if (isHybrid)
      out.puts("this._is_le = _is_le;")

    // Store parameters passed to us
    params.foreach((p) => handleAssignmentSimple(p.id, paramName(p.id)))

    if (config.readStoresPos) {
      out.puts("this._debug = {};")
    }
    out.puts
  }

  override def classConstructorFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def runRead(name: List[String]): Unit = {
    out.puts("this._read();")
  }

  override def runReadCalc(): Unit = {
    out.puts
    out.puts("if (this._is_le === true) {")
    out.inc
    out.puts("this._readLE();")
    out.dec
    out.puts("} else if (this._is_le === false) {")
    out.inc
    out.puts("this._readBE();")
    out.dec
    out.puts("} else {")
    out.inc
    out.puts("throw new KaitaiStream.UndecidedEndiannessError();")
    out.dec
    out.puts("}")
  }

  override def runWriteCalc(): Unit = {
    out.puts
    out.puts("if (this._is_le === true) {")
    out.inc
    out.puts("this._writeLE();")
    out.dec
    out.puts("} else if (this._is_le === false) {")
    out.inc
    out.puts("this._writeBE();")
    out.dec
    out.puts("} else {")
    out.inc
    out.puts("throw new KaitaiStream.UndecidedEndiannessError();")
    out.dec
    out.puts("}")
  }

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean) = {
    val suffix = endian match {
      case Some(e) => Utils.upperCamelCase(e.toSuffix)
      case None => ""
    }
    out.puts(s"${type2class(typeProvider.nowClass.name.last)}.prototype._read$suffix = function() {")
    out.inc
  }

  override def readFooter() = {
    out.dec
    out.puts("}")
  }

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {}

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {}

  override def attributeSetter(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {
    if (attrName.isInstanceOf[InstanceIdentifier]) {
      val name = publicMemberName(attrName)

      // todo implement setter
      // out.inc
      // handleAssignmentSimple(attrName, "v")
      // out.dec
    }
  }

  override def attrSetProperty(base: Ast.expr, propName: Identifier, value: String): Unit = {
    out.puts(s"${expression(base)}.${publicMemberName(propName)} = $value")
  }

  override def attrUnprocess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier, rep: RepeatSpec, dt: BytesType, exprTypeOpt: Option[DataType]): Unit = {
    // todo handleAssignment
  }

  override def attrUnprocessPrepareBeforeSubIOHandler(proc: ProcessExpr, varSrc: Identifier): Unit = {
    // todo handleAssignment
  }



  override def universalDoc(doc: DocSpec): Unit = {
    // JSDoc docstring style: http://usejsdoc.org/about-getting-started.html
    out.puts
    out.puts( "/**")

    doc.summary.foreach(summary => out.putsLines(" * ", summary))

    // http://usejsdoc.org/tags-see.html
    doc.ref.foreach {
      case TextRef(text) =>
        out.putsLines(" * ", s"@see $text")
      case UrlRef(url, text) =>
        out.putsLines(" * ", s"@see {@link $url|$text}")
    }

    out.puts( " */")
  }

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = {
    out.puts("if (this._is_le) {")
    out.inc
    leProc()
    out.dec
    out.puts("} else {")
    out.inc
    beProc()
    out.dec
    out.puts("}")
  }

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier, rep: RepeatSpec): Unit = {
    val srcExpr = getRawIdExpr(varSrc, rep)

    val expr = proc match {
      case ProcessXor(xorValue) =>
        val procName = translator.detectType(xorValue) match {
          case _: IntType => "processXorOne"
          case _: BytesType => "processXorMany"
        }
        s"$kstreamName.$procName($srcExpr, ${expression(xorValue)})"
      case ProcessZlib =>
        s"$kstreamName.processZlib($srcExpr)"
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        s"$kstreamName.processRotateLeft($srcExpr, $expr, 1)"
      case ProcessCustom(name, args) =>
        val nameInit = name.init
        val pkgName = if (nameInit.isEmpty) "" else nameInit.mkString("-") + "/"
        val procClass = type2class(name.last)

        importList.add(s"$pkgName$procClass")

        out.puts(s"var _process = new $procClass(${args.map(expression).mkString(", ")});")
        s"_process.decode($srcExpr)"
    }
    handleAssignment(varDest, expr, rep, false)
  }

  override def allocateIO(varName: Identifier, rep: RepeatSpec): String = {
    val langName = idToStr(varName)
    val memberCall = privateMemberName(varName)

    val ioName = s"_io_$langName"

    val args = getRawIdExpr(varName, rep)

    out.puts(s"var $ioName = new $kstreamName($args);")
    ioName
  }

  override def allocateIOFixed(varName: Identifier, size: String): String = {
    val langName = idToStr(varName)
    val memberCall = privateMemberName(varName)
    val ioName = s"_io_$langName"

    out.puts(s"var $ioName = new $kstreamName(new ArrayBuffer($size));")
    ioName
  }

  override def exprIORemainingSize(io: String): String =
    s"$io.size - $io.pos"

  override def subIOWriteBackHeader(subIO: String, process: Option[ProcessExpr]): String = {
    val parentIoName = "parent"
    // NOTE: local variables "$subIO" and "_process_val" are captured here as default values of
    // "handler" parameters, see
    // https://docs.python.org/3/faq/programming.html#why-do-lambdas-defined-in-a-loop-with-different-values-all-return-the-same-result
    val processValArg =
      process.map(proc => proc match {
        case _: ProcessXor | _: ProcessRotate | _: ProcessCustom =>
          ", _process_val=_process_val"
        case _ =>
          ""
      }).getOrElse("")
    out.puts(s"function handler(parent, $subIO=$subIO$processValArg) {")
    out.inc

    inSubIOWriteBackHandler = true

    parentIoName
  }

  override def subIOWriteBackFooter(subIO: String): Unit = {
    inSubIOWriteBackHandler = false
    out.puts("}")
    out.dec
    out.puts(s"$subIO.writeBackHandler = $kstreamName.WriteBackHandler(_pos2, handler)")
  }

  override def addChildIO(io: String, childIO: String): Unit =
    out.puts(s"$io.addChildStream($childIO)")

  override def pushPosForSubIOWriteBackHandler(io: String): Unit =
    out.puts(s"_pos2 = $io.pos")

  override def seekRelative(io: String, relPos: String): Unit =
    out.puts(s"$io.seek($io.pos + ($relPos))")

  override def condRepeatCommonHeader(id: Identifier, io: String, dataType: DataType): Unit = {
    out.puts(s"for (let i = 0; i < (${privateMemberName(id)}).length; i++) {")
    out.inc
  }

  def getRawIdExpr(varName: Identifier, rep: RepeatSpec): String = {
    val memberName = privateMemberName(varName)
    rep match {
      case NoRepeat => memberName
      case RepeatExpr(_) => s"$memberName[i]"
      case _ => s"$memberName[$memberName.length - 1]"
    }
  }

  override def useIO(ioEx: expr): String = {
    out.puts(s"var io = ${expression(ioEx)};")
    "io"
  }

  override def pushPos(io: String): Unit =
    out.puts(s"var _pos = $io.pos;")

  override def seek(io: String, pos: Ast.expr): Unit =
    out.puts(s"$io.seek(${expression(pos)});")

  override def popPos(io: String): Unit =
    out.puts(s"$io.seek(_pos);")

  override def alignToByte(io: String): Unit =
    out.puts(s"$io.alignToByte();")

  override def attrDebugStart(attrId: Identifier, attrType: DataType, io: Option[String], rep: RepeatSpec): Unit = {
    val debugName = attrDebugName(attrId, rep, false)

    val ioProps = io match {
      case None => ""
      case Some(x) => s"start: $x.pos, ioOffset: $x.byteOffset"
    }

    val enumNameProps = attrType match {
      case t: EnumType => s"""enumName: \"${types2class(t.enumSpec.get.name)}\""""
      case _ => ""
    }

    out.puts(s"$debugName = { $ioProps${if (ioProps != "" && enumNameProps != "") ", " else ""}$enumNameProps };")
  }

  override def attrDebugArrInit(id: Identifier, attrType: DataType): Unit =
    out.puts(s"this._debug.${idToStr(id)}.arr = [];")

  override def attrDebugEnd(attrId: Identifier, attrType: DataType, io: String, rep: RepeatSpec): Unit = {
    val debugName = attrDebugName(attrId, rep, true)

    out.puts(s"$debugName.end = $io.pos;")
  }

  override def condIfHeader(expr: expr): Unit = {
    out.puts(s"if (${expression(expr)}) {")
    out.inc
  }

  // TODO: replace this with UniversalFooter
  override def condIfFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def condRepeatInitAttr(id: Identifier, dataType: DataType): Unit =
    out.puts(s"${privateMemberName(id)} = [];")

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType): Unit = {
    out.puts("var i = 0;")
    out.puts(s"while (!$io.isEof()) {")
    out.inc
  }

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)}.push($expr);")
  }

  override def condRepeatEosFooter: Unit = {
    out.puts("i++;")
    out.dec
    out.puts("}")
  }

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, repeatExpr: expr): Unit = {
    out.puts(s"for (var i = 0; i < ${expression(repeatExpr)}; i++) {")
    out.inc
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit =
    handleAssignmentRepeatEos(id, expr)

  override def condRepeatExprFooter: Unit = {
    out.dec
    out.puts("}")
  }

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, untilExpr: expr): Unit = {
    out.puts("var i = 0;")
    out.puts("do {")
    out.inc
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    val tmpName = translator.doName(if (isRaw) Identifier.ITERATOR2 else Identifier.ITERATOR)
    out.puts(s"var $tmpName = $expr;")
    out.puts(s"${privateMemberName(id)}.push($tmpName);")
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, untilExpr: expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.puts("i++;")
    out.dec
    out.puts(s"} while (!(${expression(untilExpr)}));")
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit = {
    out.puts(s"${privateMemberName(id)} = $expr;")
  }

  override def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit =
    out.puts(s"var $id = $expr;")

  override def parseExpr(dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian]): String = {
    dataType match {
      case t: ReadableType =>
        s"$io.read${Utils.capitalize(t.apiCall(defEndian))}()"
      case blt: BytesLimitType =>
        s"$io.readBytes(${expression(blt.size)})"
      case _: BytesEosType =>
        s"$io.readBytesFull()"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        s"$io.readBytesTerm($terminator, $include, $consume, $eosError)"
      case BitsType1(bitEndian) =>
        s"$io.readBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}(1) != 0"
      case BitsType(width: Int, bitEndian) =>
        s"$io.readBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}($width)"
      case t: UserType =>
        val parent = t.forcedParent match {
          case Some(USER_TYPE_NO_PARENT) => "null"
          case Some(fp) => translator.translate(fp)
          case None => "this"
        }
        val root = if (t.isOpaque) "null" else "this._root"
        val addEndian = t.classSpec.get.meta.endian match {
          case Some(InheritedEndian) => ", this._is_le"
          case _ => ""
        }
        val addParams = Utils.join(t.args.map((a) => translator.translate(a)), ", ", ", ", "")
        s"new ${types2class(t.name)}($io, $parent, $root$addEndian$addParams)"
    }
  }

  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Int], include: Boolean) = {
    val expr1 = padRight match {
      case Some(padByte) => s"$kstreamName.bytesStripRight($expr0, $padByte)"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) => s"$kstreamName.bytesTerminate($expr1, $term, $include)"
      case None => expr1
    }
    expr2
  }

  override def userTypeDebugRead(id: String, dataType: DataType, assignType: DataType): Unit = {
    out.puts(s"$id._read();")
  }

  override def switchRequiresIfs(onType: DataType): Boolean = onType match {
    case _: IntType | _: BooleanType | _: EnumType | _: StrType => false
    case _ => true
  }

  //<editor-fold desc="switching: true version">

  override def switchStart(id: Identifier, on: Ast.expr): Unit =
    out.puts(s"switch (${expression(on)}) {")

  override def switchCaseFirstStart(condition: Ast.expr): Unit =
    switchCaseStart(condition)

  override def switchCaseStart(condition: Ast.expr): Unit = {
    out.puts(s"case ${expression(condition)}:")
    out.inc
  }

  override def switchCaseEnd(): Unit = {
    out.puts("break;")
    out.dec
  }

  override def switchElseStart(): Unit = {
    out.puts("default:")
    out.inc
  }

  override def switchEnd(): Unit =
    out.puts("}")

  //</editor-fold>

  //<editor-fold desc="switching: emulation with ifs">

  val NAME_SWITCH_ON = Ast.expr.Name(Ast.identifier(Identifier.SWITCH_ON))

  override def switchIfStart(id: Identifier, on: Ast.expr, onType: DataType): Unit = {
    out.puts("{")
    out.inc
    out.puts(s"var ${expression(NAME_SWITCH_ON)} = ${expression(on)};")
  }

  private def switchCmpExpr(condition: Ast.expr): String =
    expression(
      Ast.expr.Compare(
        NAME_SWITCH_ON,
        Ast.cmpop.Eq,
        condition
      )
    )

  override def switchIfCaseFirstStart(condition: Ast.expr): Unit = {
    out.puts(s"if (${switchCmpExpr(condition)}) {")
    out.inc
  }

  override def switchIfCaseStart(condition: Ast.expr): Unit = {
    out.puts(s"else if (${switchCmpExpr(condition)}) {")
    out.inc
  }

  override def switchIfCaseEnd(): Unit = {
    out.dec
    out.puts("}")
  }

  override def switchIfElseStart(): Unit = {
    out.puts("else {")
    out.inc
  }

  override def switchIfEnd(): Unit = {
    out.dec
    out.puts("}")
  }

  override def instanceWriteFlagDeclaration(attrName: InstanceIdentifier): Unit = {}

  override def instanceWriteFlagInit(attrName: InstanceIdentifier): Unit = {
    instanceClearWriteFlag(attrName)
    out.puts(s"this.${publicMemberName(attrName)}__to_write = true;")
  }

  override def instanceSetWriteFlag(instName: InstanceIdentifier): Unit = {
    out.puts(s"this._should_write_${publicMemberName(instName)} = this.${publicMemberName(instName)}__to_write;")
  }

  override def instanceClearWriteFlag(instName: InstanceIdentifier): Unit = {
    out.puts(s"this._should_write_${publicMemberName(instName)} = false;")
  }

  override def instanceToWriteSetter(instName: InstanceIdentifier): Unit = {}

  override def instanceCheckWriteFlagAndWrite(instName: InstanceIdentifier): Unit = {
    out.puts(s"if (this._should_write_${publicMemberName(instName)}) {")
    out.inc
    out.puts(s"this._write_${publicMemberName(instName)}()")
    out.dec
    out.puts("}")
  }

  override def instanceInvalidate(instName: InstanceIdentifier): Unit = {
    out.puts
    out.puts(s"${type2class(typeProvider.nowClass.name.last)}.prototype._invalidate_${publicMemberName(instName)} = function() {")
    out.inc
    // out.puts(s"delete ${privateMemberName(instName)}")
    // ???
    out.dec
    out.puts("}")
  }

  override def internalEnumIntType(basedOn: IntType): DataType =
    basedOn

  override def attrPrimitiveWrite(
    io: String,
    valueExpr: Ast.expr,
    dataType: DataType,
    defEndian: Option[FixedEndian],
    exprTypeOpt: Option[DataType]
  ): Unit = {
    val expr = expression(valueExpr)

    val stmt = dataType match {
      case t: ReadableType =>
        s"$io.write${Utils.capitalize(t.apiCall(defEndian))}($expr);"
      case BitsType1(bitEndian) =>
        s"$io.$io.writeBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}(1, ${translator.boolToInt(valueExpr)})"
      case BitsType(width: Int, bitEndian) =>
        s"$io.$io.writeBitsInt${Utils.upperCamelCase(bitEndian.toSuffix)}($width, $expr)"
      case _: BytesType =>
        s"$io.writeBytes($expr)"
    }
    out.puts(stmt)
  }

  override def attrBytesLimitWrite(io: String, expr: Ast.expr, size: String, term: Int, padRight: Int): Unit =
    out.puts(s"$io.writeBytesLimit(${expression(expr)}, $size, $term, $padRight)")

  override def attrUserTypeInstreamWrite(io: String, valueExpr: Ast.expr, dataType: DataType, exprType: DataType) = {
    val expr = expression(valueExpr)
    out.puts(s"$expr._write__seq($io)")
  }

  override def exprStreamToByteArray(io: String): String =
    s"$io.toByteArray()"

  override def attrBasicCheck(checkExpr: Ast.expr, actual: Ast.expr, expected: Ast.expr, msg: String): Unit = {
    val msgStr = expression(Ast.expr.Str(msg))

    out.puts(s"if (${expression(checkExpr)}) {")
    out.inc
    out.puts(s"throw new KaitaiStream.ConsistencyError(${msgStr}, ${expression(actual)}, ${expression(expected)});")
    out.dec
    out.puts("}")
  }

  override def attrObjectsEqualCheck(actual: Ast.expr, expected: Ast.expr, msg: String): Unit = {
    val msgStr = expression(Ast.expr.Str(msg))

    out.puts(s"if (${expression(actual)} !== ${expression(expected)}) {")
    out.inc
    out.puts(s"throw new KaitaiStream.ConsistencyError(${msgStr}, ${expression(actual)}, ${expression(expected)});")
    out.dec
    out.puts("}")
  }

  override def attrParentParamCheck(actualParentExpr: Ast.expr, ut: UserType, shouldDependOnIo: Option[Boolean], msg: String): Unit = {
    if (ut.isOpaque) return

    val (expectedParent, dependsOnIo) = ut.forcedParent match {
      case Some(USER_TYPE_NO_PARENT) => ("null", false)
      case Some(fp) => (expression(fp), userExprDependsOnIo(fp))
      case None => ("this", false)
    }
    
    if (shouldDependOnIo.map(shouldDepend => dependsOnIo != shouldDepend).getOrElse(false)) return

    val msgStr = expression(Ast.expr.Str(msg))

    out.puts(s"if (${expression(actualParentExpr)} !== ${expectedParent}) {")
    out.inc
    out.puts(s"throw new KaitaiStream.ConsistencyError(${msgStr}, ${expression(actualParentExpr)}, ${expectedParent});")
    out.dec
    out.puts("}")
  }

  override def attrIsEofCheck(io: String, expectedIsEof: Boolean, msg: String): Unit = {
    val msgStr = expression(Ast.expr.Str(msg))

    val eofExpr = s"${io}.isEof()"
    val ifExpr = if (expectedIsEof) {
      s"(${eofExpr} === false)"
    } else {
      eofExpr
    }

    out.puts(s"if (${ifExpr}) {")
    out.inc
    out.puts(s"throw new KaitaiStream.ConsistencyError(${msgStr}, ${exprIORemainingSize(io)}, 0);")
    out.dec
    out.puts("}")
  }

  override def condIfIsEofHeader(io: String, wantedIsEof: Boolean): Unit = {
    val eofExpr = s"${io}.isEof()"
    val ifExpr = if (!wantedIsEof) {
      s"(${eofExpr} === false)"
    } else {
      eofExpr
    }

    out.puts(s"if (${ifExpr}) {")
    out.inc
  }

  override def condIfIsEofFooter: Unit = {
    out.dec
    out.puts("}")
  }

  //</editor-fold>

  override def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    out.puts(s"Object.defineProperty(${type2class(className.last)}.prototype, '${publicMemberName(instName)}', {")
    out.inc
    out.puts("get: function() {")
    out.inc
  }

  override def instanceFooter: Unit = {
    out.dec
    out.puts("}")
    out.dec
    out.puts("});")
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit = {
    out.puts(s"if (${privateMemberName(instName)} !== undefined)")
    out.inc
    instanceReturn(instName, dataType)
    out.dec
  }

  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType): Unit = {
    out.puts(s"return ${privateMemberName(instName)};")
  }

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, EnumValueSpec)]): Unit = {
    out.puts(s"${type2class(curClass.last)}.${type2class(enumName)} = Object.freeze({")
    out.inc

    // Name to ID mapping
    enumColl.foreach { case (id, label) =>
      out.puts(s"${enumValue(enumName, label.name)}: ${translator.doIntLiteral(id)},")
    }
    out.puts

    // ID to name mapping
    enumColl.foreach { case (id, label) =>
      val idStr = if (id < 0) {
        "\"" + id.toString + "\""
      } else {
        id.toString
      }
      out.puts(s"""$idStr: "${enumValue(enumName, label.name)}",""")
    }

    out.dec
    out.puts("});")
    out.puts
  }

  def enumValue(enumName: String, label: String) = Utils.upperCamelCase(label)

  override def debugClassSequence(seq: List[AttrSpec]) = {
    //val seqStr = seq.map((attr) => "\"" + idToStr(attr.id) + "\"").mkString(", ")
    //out.puts(s"SEQ_FIELDS = [$seqStr]")
  }

  override def classToString(toStringExpr: Ast.expr): Unit = {
    val className = type2class(translator.provider.nowClass.name.last)

    out.puts
    out.puts(s"${className}.prototype.toString = function() {")
    out.inc
    out.puts(s"return ${translator.translate(toStringExpr)};")
    out.dec
    out.puts("}")
  }

  def idToStr(id: Identifier): String = JavaScriptCompiler.idToStr(id)

  override def publicMemberName(id: Identifier) = JavaCompiler.publicMemberName(id)

  override def privateMemberName(id: Identifier): String = s"this.${idToStr(id)}"

  override def localTemporaryName(id: Identifier): String = s"_t_${idToStr(id)}"

  override def ksErrorName(err: KSError): String = JavaScriptCompiler.ksErrorName(err)

  override def attrValidateExpr(
    attr: AttrLikeSpec,
    checkExpr: Ast.expr,
    err: KSError,
    useIo: Boolean,
    expected: Option[Ast.expr] = None
  ): Unit = {
    val errArgsStr = expected.map(expression) ++ List(
      expression(Ast.expr.InternalName(attr.id)),
      if (useIo) expression(Ast.expr.InternalName(IoIdentifier)) else "null",
      expression(Ast.expr.Str(attr.path.mkString("/", "/", "")))
    )
    out.puts(s"if (!(${translator.translate(checkExpr)})) {")
    out.inc
    out.puts(s"throw new ${ksErrorName(err)}(${errArgsStr.mkString(", ")});")
    out.dec
    out.puts("}")
  }

  def attrDebugName(attrId: Identifier, rep: RepeatSpec, end: Boolean) = {
    val arrIndexExpr = rep match {
      case NoRepeat => ""
      case _: RepeatExpr => ".arr[i]"
      case RepeatEos | _: RepeatUntil => s".arr[${privateMemberName(attrId)}.length${if (end) " - 1" else ""}]"
    }

    s"this._debug.${idToStr(attrId)}$arrIndexExpr"
  }
}

object JavaScriptCompiler extends LanguageCompilerStatic
  with UpperCamelCaseClasses
  with StreamStructNames
  with ExceptionNames {
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new JavaScriptCompiler(tp, config)

  def idToStr(id: Identifier): String =
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => Utils.lowerCamelCase(name)
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => s"_m_${Utils.lowerCamelCase(name)}"
      case RawIdentifier(innerId) => s"_raw_${idToStr(innerId)}"
    }

  def publicMemberName(id: Identifier): String =
    id match {
      case InstanceIdentifier(name) => Utils.lowerCamelCase(name)
      case _ => idToStr(id)
    }

  override def kstreamName: String = "KaitaiStream"

  // FIXME: probably KaitaiStruct will emerge some day in JavaScript runtime, but for now it is unused
  override def kstructName: String = ???

  override def ksErrorName(err: KSError): String = err match {
    case EndOfStreamError => s"KaitaiStream.EOFError"
    case _ => s"KaitaiStream.${err.name}"
  }

  def types2class(types: List[String]): String = types.map(type2class).mkString(".")
}
