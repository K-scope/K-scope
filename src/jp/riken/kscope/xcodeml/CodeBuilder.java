/*
 * K-scope
 * Copyright 2012-2013 RIKEN, Japan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package jp.riken.kscope.xcodeml;

import java.io.File;
import java.io.PrintWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.exception.XcodeMLException;
import jp.riken.kscope.utils.FileUtils;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.xcodeml.util.XmlNodeUtil;
import jp.riken.kscope.xcodeml.xml.*;
import jp.riken.kscope.xcodeml.xml.gen.*;

/**
 * Source code generation class
 *
 * @author RIKEN
 */
public class CodeBuilder extends XcodeMLVisitorImpl {
  /** Space character */
  public static final String DEFAULT_SPC_CHARS = " ";
  /** Indent string */
  public static final String DEFAULT_INDENT_CHARS = " ";
  /** Text output stream */
  private PrintWriter _out;
  /** Code line buffer */
  private StringBuilder m_lineBuf = new StringBuilder();
  /** Line of code list */
  private ArrayList<CodeLine> m_sourceList = new ArrayList<CodeLine>();
  /** XML parsing execution state */
  private XcodeMLContext _context;
  /** Dummy execution buffer (use only string assembly function) */
  private StringBuilder m_dummyBuf = null;
  /** Lowercase syntax characters, uppercase conversion flag true = lowercase. */
  private boolean lowercase = true;

  /**
   * Constructor
   *
   * @param context XcodeML parser execution status class
   */
  public CodeBuilder(XcodeMLContext context) {
    _context = context;
    _out = null;
  }

  /**
   * Get the number of bytes of a character.
   *
   * @param c character
   * @return 1 or 2
   */
  private static int _getColumnCount(char c) {
    // Character code considers less than 0x100
    // to be a halfwidth character.
    if (c < 0x0100) {
      return 1;
    }
    return 2;
  }

  /**
   * Get the number of bytes in the character array.
   *
   * @param array Character array
   * @return Number of bytes
   */
  private static int _getColumnCount(char[] array) {
    int columnCount = 0;
    for (char c : array) {
      columnCount += _getColumnCount(c);
    }
    return columnCount;
  }

  /**
   * Get the number of bytes in a string
   *
   * @param s string
   * @return Number of bytes
   */
  @SuppressWarnings("unused")
  private static int _getColumnCount(String s) {
    return _getColumnCount(s.toCharArray());
  }

  /**
   * Convert to quoted string.
   *
   * @param s Any string.
   * @return Converted string.
   */
  private static String _toStringLiteral(String s) {
    // Buffer capacity = <source length> * 2 + <literal prefix and suffix>
    StringBuilder buff = new StringBuilder(s.length() * 2 + 2);
    buff.append('"');
    for (char c : s.toCharArray()) {
      if (c == '"') {
        buff.append("\"\"");
      } else {
        buff.append(c);
      }
    }
    buff.append('"');
    return buff.toString();
  }

  /**
   * Get writer set by this writer.
   *
   * @return Writer.
   */
  public Writer getWriter() {
    return _out;
  }

  /**
   * Set writer set by this writer.
   *
   * @param out Text output stream
   */
  public void setWriter(PrintWriter out) {
    _out = out;
  }

  /** Setup new line. */
  public void updateCodeLine() {
    updateCodeLine(null);
  }

  /**
   * Print a line of code.
   *
   * @param node Output node
   * @return Success or failure
   */
  public boolean updateCodeLine(IXmlNode node) {
    // Do not update during dummy execution.
    if (m_dummyBuf != null) {
      return true;
    }
    if (m_lineBuf.length() > 0) {
      if (_out != null) {
        _out.println(m_lineBuf);
        flush();
      }
      CodeLine code = createCodeLine(node);
      assert (code != null);

      m_sourceList.add(code);
      m_lineBuf = new StringBuilder();

      return true;
    }

    //        _context.setLastErrorMessage("fault:updateCodeLine class=" +
    // node.getClass().getName());
    //        return false;
    return true;
  }

  /**
   * Performs case conversion.
   *
   * @param text Conversion source string
   * @return Uppercase / lowercase conversion string
   */
  private String toCaseSensitive(String text) {
    if (text == null || text.length() == 0) {
      return "";
    }
    if (this.lowercase) {
      return text.toLowerCase();
    } else {
      return text.toUpperCase();
    }
  }

  /**
   * Write in the designated string as token.
   *
   * @param s String to write
   */
  public void writeToken(String s) {
    if (s == null || s.length() == 0) {
      return;
    }

    _writeCharacterArray(s.toCharArray());
  }

  /**
   * Write in the designated string as literal string of Fortran.
   *
   * @param s String to write
   */
  public void writeLiteralString(String s) {
    if (s == null || s.length() == 0) {
      s = "\"\"";
    } else {
      s = _toStringLiteral(s);
    }

    char[] chArray = s.toCharArray();

    _writeCharacterArray(chArray);
  }

  /**
   * Output a character list.
   *
   * @param chArray Character list
   */
  private void _writeCharacterArray(char[] chArray) {
    if (m_dummyBuf != null) {
      // Dummy running
      m_dummyBuf.insert(m_dummyBuf.length(), chArray, 0, chArray.length);
    } else {
      // Normal execution
      m_lineBuf.insert(m_lineBuf.length(), chArray, 0, chArray.length);
    }
  }

  /**
   * Output line number and FpragmaStatement.
   *
   * <p>Do not add to the code line class. Only output a file for debugging.
   *
   * @param s String to write
   */
  public void writeIsolatedLine(String s) {
    // Note: Omit count up of _columnNumber
    if (_out != null) {
      _out.println(s);
    }
    updateCodeLine();
  }

  /** Flush writer. */
  public void flush() {
    if (_out != null) {
      _out.flush();
    }
  }

  /** Close writer. */
  public void close() {
    if (_out != null) {
      _out.close();
    }
  }

  /**
   * Get the last line of code.
   *
   * @return Last line of code
   */
  public CodeLine getLastCodeLine() {
    if (m_sourceList.size() == 0) return null;
    return m_sourceList.get(m_sourceList.size() - 1);
  }

  /**
   * Generate a line of code class.
   *
   * @param filename Source file name
   * @param startlineno Start line number
   * @param endlineno End line number
   * @param statement Code statement
   * @return Generated code line class
   */
  private CodeLine createCodeLine(
      String filename, String startlineno, String endlineno, String statement) {

    // Delete There is an element (VarDecl) that is parsed from XML and does not have a starting
    // line number.
    // assert (startlineno != null && startlineno.length() > 0);
    assert (m_lineBuf != null || m_lineBuf.length() <= 0);

    int start_no = 0;
    if (startlineno != null && startlineno.length() > 0) {
      start_no = Integer.parseInt(startlineno);
    }
    int end_no = start_no;
    if (endlineno != null && endlineno.length() > 0) {
      end_no = Integer.parseInt(endlineno);
    }

    // Source file, source code line
    File srcFile =
        FileUtils.joinFilePath(_context.getSourceXmlFile().getFile().getParentFile(), filename);
    SourceFile codeFile = null;
    if (srcFile != null) {
      if (_context.getBaseFolder() != null) {
        String path = FileUtils.getRelativePath(srcFile, _context.getBaseFolder());
        if (path != null) {
          codeFile = new SourceFile(path);
        }
      } else {
        codeFile = new SourceFile(srcFile);
      }
    }

    CodeLine lineInfo = new CodeLine(codeFile, statement, start_no, end_no, filename);

    return lineInfo;
  }

  /**
   * Generate a line of code class.
   *
   * @param node XML node
   * @return code line class
   */
  public CodeLine createCodeLine(IXmlNode node) {
    // Get the file name, start line number, and end line number from the parent element of the
    // current element.
    IDefBaseStatement base_node = _context.getInvokeBaseStatement(node);
    if (base_node == null) return null;

    String filename = base_node.getFile();
    String startlineno = base_node.getLineno();
    String endlineno = base_node.getEndlineno();

    return createCodeLine(filename, startlineno, endlineno, m_lineBuf.toString());
  }

  /**
   * Generate a line of code class.
   *
   * @param node XML node
   * @param line Code line String
   * @return code line class
   */
  public CodeLine createCodeLine(IXmlNode node, String line) {
    // Get the file name, start line number, and end line number from the parent element of the
    // current element.
    IDefBaseStatement base_node = _context.getInvokeBaseStatement(node);
    if (base_node == null) return null;

    String filename = base_node.getFile();
    String startlineno = base_node.getLineno();
    String endlineno = base_node.getEndlineno();

    if (filename == null) return null;

    return createCodeLine(filename, startlineno, endlineno, line);
  }

  /**
   * return if current context is under FmoduleDefinition's grandchild <br>
   * ex 1) return true <br>
   * FmoduleModuleDefinition <br>
   * +declarations <br>
   * +current <br>
   * <br>
   * ex 2) return false <br>
   * FmoduleModuleDefinition <br>
   * +declarations <br>
   * +FfunctionDefinition <br>
   * +declarations <br>
   * +current
   *
   * @return true if the current context is undef FmoduleDefinition's grandchild
   */
  private boolean _isUnderModuleDef() {
    return _context.isInvokeNodeOf(FmoduleDefinition.class, 2);
  }

  /**
   * Get the data type
   *
   * @param type type name
   * @return data type
   */
  private XcodeMLTypeManager.TypeList getTypeList(String type) {
    XcodeMLTypeManager typeManager = _context.getTypeManager();
    XcodeMLTypeManager.TypeList typeList = null;

    try {
      typeList = typeManager.getTypeReferenceList(type);
    } catch (XcodeMLException e) {
      _context.debugPrintLine(e.toString());
      _context.setLastErrorMessage(
          XmlNodeUtil.formatError(_context.peekInvokeNode(), EnumError.XCODEML_CYCLIC_TYPE, type));
      return null;
    }

    if (typeList == null || typeList.isEmpty()) {
      _context.debugPrintLine("Type list is empty.");
      _context.setLastErrorMessage(
          XmlNodeUtil.formatError(
              _context.peekInvokeNode(), EnumError.XCODEML_TYPE_NOT_FOUND, type));
      return null;
    }

    return typeList;
  }

  /**
   * Write line directive.
   *
   * @param lineNumber Line number.
   * @param filePath File path.
   */
  public void writeLineDirective(String lineNumber, String filePath) {
    if (_context.isOutputLineDirective() && lineNumber != null) {
      if (filePath == null) writeIsolatedLine(String.format("# %s", lineNumber));
      else writeIsolatedLine(String.format("# %s \"%s\"", lineNumber, filePath));
    }
  }

  /**
   * Write unary expression.
   *
   * @param expr Instance of IDefModelExprChoice.
   * @param operation Operation string.
   * @param grouping Grouping flag.
   * @return true/false
   */
  public boolean writeUnaryExpr(IXmlNode expr, String operation, boolean grouping) {
    XcodeMLVisitor visiter = _context.getVisitor();

    if (grouping == true) {
      writeToken("(");
    }

    writeToken(operation);

    if (visiter.invokeEnter(expr) == false) {
      return false;
    }

    if (grouping == true) {
      writeToken(")");
    }

    return true;
  }

  /**
   * Write binary expression.
   *
   * @param leftExpr Instance of IDefModelExprChoice which expresses a Lvalue.
   * @param rightExpr Instance of IDefModelExprChoice which expresses a Rvalue.
   * @param operation Operation string.
   * @param grouping Grouping flag.
   * @return true/false
   */
  public boolean writeBinaryExpr(
      IXmlNode leftExpr, IXmlNode rightExpr, String operation, boolean grouping) {
    XcodeMLVisitor visiter = _context.getVisitor();

    // Get the next higher parent element.
    IXmlNode parentNode = _context.getInvokeNode(1);

    boolean isExpr = XmlNodeUtil.isExprModel(parentNode);

    if (grouping && isExpr) {
      writeToken("(");
    }

    if (visiter.invokeEnter(leftExpr) == false) {
      return false;
    }

    writeToken(" ");
    writeToken(operation);
    writeToken(" ");

    if (visiter.invokeEnter(rightExpr) == false) {
      return false;
    }

    if (grouping && isExpr) {
      writeToken(")");
    }

    return true;
  }

  /**
   * Write simple primitive symbol declaration.
   *
   * @param symbol Instance of XmlSymbol.
   */
  private void _writeSimplePrimitiveSymbolDecl(XmlSymbol symbol) {
    writeToken(toCaseSensitive(symbol.getTypeId().fortranName()));
    writeToken(" :: ");
    writeToken(symbol.getSymbolName());
  }

  /**
   * Write kind and length in character declaration.
   *
   * @param kind data type
   * @param lenElem Data length
   * @return Success or failure
   */
  public boolean writeTypeParam(Kind kind, Len lenElem) {
    if (kind == null && lenElem == null) {
      // Succeed forcibly.
      return true;
    }

    XcodeMLVisitor visiter = _context.getVisitor();
    writeToken("(");

    if (lenElem != null) {
      writeToken(toCaseSensitive("LEN="));
      if (visiter.invokeEnter(lenElem) == false) {
        return false;
      }
    }

    if (kind != null) {
      if (lenElem != null) {
        writeToken(", ");
      }
      writeToken(toCaseSensitive("KIND="));
      if (visiter.invokeEnter(kind) == false) {
        return false;
      }
    }
    writeToken(")");

    return true;
  }

  /**
   * Write index ranges of array.
   *
   * @param indexRangeArray Index range
   * @return true / false Success or failure
   * @example <div class = "Example"> INTEGER value <span class = "Strong"> (10, 1:20) </ span> </
   *     div>
   */
  public boolean writeIndexRangeArray(IXmlNode[] indexRangeArray) {
    if (indexRangeArray == null) {
      // Succeed forcibly.
      return true;
    }

    XcodeMLVisitor visiter = _context.getVisitor();
    writeToken("(");

    int indexRangeCount = 0;

    for (IXmlNode arraySubscriptChoice : indexRangeArray) {
      if (indexRangeCount > 0) {
        writeToken(", ");
      }

      if ((arraySubscriptChoice instanceof IndexRange)
          || (arraySubscriptChoice instanceof ArrayIndex)) {
        if (visiter.invokeEnter(arraySubscriptChoice) == false) {
          return false;
        }
      } else {
        _context.debugPrintLine("Detected discontinuous 'indexRange' or 'arrayIndex' element.");
        _context.setLastErrorMessage(
            XmlNodeUtil.formatError(
                arraySubscriptChoice,
                EnumError.XCODEML_SEMANTICS,
                XmlNodeUtil.getElementName(arraySubscriptChoice)));
        return false;
      }
      ++indexRangeCount;
    }
    writeToken(")");

    return true;
  }

  /**
   * Write coindex ranges of array.
   *
   * @param indexRangeArray Index range
   * @return true / false
   * @example <div class = "Example"> INTEGER value <span class = "Strong"> [10, 1: *] </ span> </
   *     div>
   */
  public boolean writeCoIndexRangeArray(IndexRange[] indexRangeArray) {
    if (indexRangeArray == null) {
      // Succeed forcibly.
      return true;
    }

    XcodeMLVisitor visiter = _context.getVisitor();
    writeToken("[");

    int indexRangeCount = 0;

    for (IndexRange arraySubscriptChoice : indexRangeArray) {
      if (indexRangeCount > 0) {
        writeToken(", ");
      }

      // if ((arraySubscriptChoice instanceof IndexRange)
      // || (arraySubscriptChoice instanceof ArrayIndex)) {
      if ((arraySubscriptChoice instanceof IndexRange)) {
        if (visiter.invokeEnter(arraySubscriptChoice) == false) {
          return false;
        }
      } else {
        _context.debugPrintLine("Detected discontinuous 'indexRange' or 'arrayIndex' element.");
        _context.setLastErrorMessage(
            XmlNodeUtil.formatError(
                arraySubscriptChoice,
                EnumError.XCODEML_SEMANTICS,
                XmlNodeUtil.getElementName(arraySubscriptChoice)));
        return false;
      }
      ++indexRangeCount;
    }
    writeToken("]");

    return true;
  }

  /**
   * Output data definition attributes
   *
   * @param top Root data definition table
   * @param low data definition
   */
  private void _writeDeclAttr(IXmlTypeTableChoice top, IXmlTypeTableChoice low) {
    if ((top instanceof FbasicType) && (low instanceof FbasicType)) {
      _writeBasicTypeAttr((FbasicType) top, (FbasicType) low);
      return;
    }

    if (top instanceof FbasicType) {
      _writeBasicTypeAttr((FbasicType) top);
    }

    if (low instanceof FbasicType) {
      _writeBasicTypeAttr((FbasicType) low);
    }
  }

  /**
   * Output data definition attributes
   *
   * @param basicTypeArray Data definition
   */
  private void _writeBasicTypeAttr(FbasicType... basicTypeArray) {
    if (basicTypeArray == null) {
      return;
    }

    /* public, private are allowed only in module definition */
    if (_isUnderModuleDef()) {
      for (FbasicType basicTypeElem : basicTypeArray) {
        if (XmlNodeUtil.isBoolean(basicTypeElem.isIsPublic())) {
          writeToken(", ");
          writeToken(toCaseSensitive("PUBLIC"));
          break;
        }
      }

      for (FbasicType basicTypeElem : basicTypeArray) {
        if (XmlNodeUtil.isBoolean(basicTypeElem.isIsPrivate())) {
          writeToken(", ");
          writeToken(toCaseSensitive("PRIVATE"));
          break;
        }
      }
    }

    for (FbasicType basicTypeElem : basicTypeArray) {
      if (XmlNodeUtil.isBoolean(basicTypeElem.isIsPointer())) {
        writeToken(", ");
        writeToken(toCaseSensitive("POINTER"));
        break;
      }
    }

    for (FbasicType basicTypeElem : basicTypeArray) {
      if (XmlNodeUtil.isBoolean(basicTypeElem.isIsTarget())) {
        writeToken(", ");
        writeToken(toCaseSensitive("TARGET"));
        break;
      }
    }

    for (FbasicType basicTypeElem : basicTypeArray) {
      if (XmlNodeUtil.isBoolean(basicTypeElem.isIsOptional())) {
        writeToken(", ");
        writeToken(toCaseSensitive("OPTIONAL"));
        break;
      }
    }

    for (FbasicType basicTypeElem : basicTypeArray) {
      if (XmlNodeUtil.isBoolean(basicTypeElem.isIsSave())) {
        writeToken(", ");
        writeToken(toCaseSensitive("SAVE"));
        break;
      }
    }

    for (FbasicType basicTypeElem : basicTypeArray) {
      if (XmlNodeUtil.isBoolean(basicTypeElem.isIsParameter())) {
        writeToken(", ");
        writeToken(toCaseSensitive("PARAMETER"));
        break;
      }
    }

    for (FbasicType basicTypeElem : basicTypeArray) {
      if (XmlNodeUtil.isBoolean(basicTypeElem.isIsAllocatable())) {
        writeToken(", ");
        writeToken(toCaseSensitive("ALLOCATABLE"));
        break;
      }
    }

    for (FbasicType basicTypeElem : basicTypeArray) {
      DefChoiceIntent defIntent = basicTypeElem.getIntent();
      if (defIntent != null) {
        String intent = defIntent.value();
        if (StringUtils.isNullOrEmpty(intent) == false) {
          writeToken(", ");
          writeToken(toCaseSensitive("INTENT(" + intent + ")"));
          break;
        }
      }
    }
  }

  /**
   * Output function data type
   *
   * @param symbol XmlSymbol
   * @param funcType FfunctionType
   * @param visitable element
   * @return Success or failure
   */
  public boolean writeFunctionSymbol(XmlSymbol symbol, FfunctionType funcType, IXmlNode visitable) {
    XcodeMLTypeManager typeManager = _context.getTypeManager();
    XcodeMLVisitor visiter = _context.getVisitor();
    IXmlTypeTableChoice lowType = null;

    if (XmlNodeUtil.isBoolean(funcType.isIsIntrinsic())) {
      writeToken(toCaseSensitive("INTRINSIC "));
      writeToken(symbol.getSymbolName());
      return true;
    }

    boolean isFirstToken = true;
    boolean isPrivateEmit = false;
    boolean isPublicEmit = false;

    /*
     * - always type declaration for SUBROUTINE must not be output. - type
     * declaration for FUNCTION under MODULE must not be output.
     */
    if (typeManager.isDecompilableType(funcType.getReturnType())
        && (_isUnderModuleDef() == false || XmlNodeUtil.isBoolean(funcType.isIsExternal()))) {

      EnumType type = EnumType.getTypeIdFromXcodemlTypeName(funcType.getReturnType());
      if (type.isPrimitive()) {
        writeToken(toCaseSensitive(type.fortranName()));
      } else {
        XcodeMLTypeManager.TypeList typeList = getTypeList(funcType.getReturnType());
        if (typeList == null) return false;

        lowType = typeList.getLast();
        IXmlTypeTableChoice topType = typeList.getFirst();

        if (lowType instanceof FbasicType) {
          FbasicType bt = (FbasicType) lowType;
          if (XmlNodeUtil.isBoolean(bt.isIsPublic())) {
            isPublicEmit = true;
          }
          if (XmlNodeUtil.isBoolean(bt.isIsPrivate())) {
            isPrivateEmit = true;
          }
        }

        if (topType instanceof FbasicType) {
          FbasicType bt = (FbasicType) topType;
          if (XmlNodeUtil.isBoolean(bt.isIsPublic())) {
            isPublicEmit = true;
          }
          if (XmlNodeUtil.isBoolean(bt.isIsPrivate())) {
            isPrivateEmit = true;
          }
        }

        if (topType instanceof FbasicType) {
          if (writeBasicType((FbasicType) topType, typeList) == false) return false;

        } else if (topType instanceof FstructType) {
          FstructType structTypeElem = (FstructType) topType;
          String aliasStructTypeName = typeManager.getAliasTypeName(structTypeElem.getType());
          writeToken(toCaseSensitive("TYPE") + "(" + aliasStructTypeName + ")");

        } else {
          /* topType is FfunctionType. */
          return false;
        }

        _writeDeclAttr(topType, lowType);
      }
      isFirstToken = false;
    }

    if (_isUnderModuleDef()) {
      if (XmlNodeUtil.isBoolean(funcType.isIsPublic()) && isPublicEmit == false) {
        writeToken((isFirstToken ? "" : ", ") + toCaseSensitive("PUBLIC"));
        isFirstToken = false;
      } else if (XmlNodeUtil.isBoolean(funcType.isIsPrivate()) && isPrivateEmit == false) {
        writeToken((isFirstToken ? "" : ", ") + toCaseSensitive("PRIVATE"));
        isFirstToken = false;
      }
    }

    if (isFirstToken == false) {
      writeToken(" :: ");
      writeToken(symbol.getSymbolName());

      if (lowType != null && (lowType instanceof FbasicType)) {
        FbasicType basicTypeElem = (FbasicType) lowType;

        List<IXmlNode> list = basicTypeElem.getIndexRangeOrArrayIndex();
        DefModelArraySubscriptSequence basicTypeChoice = null;
        if (list != null && list.size() > 0) {
          basicTypeChoice = new DefModelArraySubscriptSequence(list);
        }
        if (basicTypeChoice != null
            && (basicTypeChoice instanceof DefModelArraySubscriptSequence)) {
          if (visiter.invokeEnter(basicTypeChoice) == false) {
            return false;
          }
        }
      }
    }

    if (XmlNodeUtil.isBoolean(funcType.isIsExternal())) {
      if (isFirstToken == false) updateCodeLine();
      writeToken(toCaseSensitive("EXTERNAL "));
      writeToken(symbol.getSymbolName());
    }

    return true;
  }

  /**
   * Output basic data type
   *
   * @param basicTypeElem Basic data type element
   * @param typeList typeTable
   * @return Success or failure
   */
  public boolean writeBasicType(FbasicType basicTypeElem, XcodeMLTypeManager.TypeList typeList) {
    String refName = basicTypeElem.getRef();
    EnumType refTypeId = EnumType.getTypeIdFromXcodemlTypeName(refName);
    assert (refTypeId != null);

    if (refTypeId.isPrimitive() == false) {
      _context.debugPrint(
          "Top level type is basic-type, but is not primitive type. (%s)%n", refName);
      if (typeList != null) _context.debugPrintLine(typeList.toString());
      _context.setLastErrorMessage(
          XmlNodeUtil.formatError(
              basicTypeElem,
              EnumError.XCODEML_TYPE_MISMATCH,
              "top-level FbasicType",
              refName,
              "primitive type"));
      return false;
    }

    writeToken(toCaseSensitive(refTypeId.fortranName()));

    Kind kind = basicTypeElem.getKind();
    Len lenElem = null;

    IXmlNode basicTypeChoice = basicTypeElem.getLen();
    if (basicTypeChoice != null) {
      if (basicTypeChoice instanceof Len) {
        if (refTypeId != EnumType.CHARACTER) {
          _context.debugPrint(
              "A 'len' element is included in a definition of '%s' type.%n",
              refTypeId.xcodemlName());
          _context.setLastErrorMessage(
              XmlNodeUtil.formatError(
                  basicTypeChoice,
                  EnumError.XCODEML_SEMANTICS,
                  XmlNodeUtil.getElementName(basicTypeChoice)));
          return false;
        }
        lenElem = (Len) basicTypeChoice;
      }
    }
    if (writeTypeParam(kind, lenElem) == false) {
      return false;
    }

    return true;
  }

  /**
   * Write variable declaration.
   *
   * @param symbol Variable symbol.
   * @param visitable XML Node
   * @return true/false
   * @example <div class="Example"> PROGRAM main <div class="Indent1"><div class="Strong"> INTEGER
   *     :: int_variable<br>
   *     TYPE(USER_TYPE) :: derived_variable </div> int_variable = 0 </div> END PROGRAM main </div>
   */
  public boolean writeSymbolDecl(XmlSymbol symbol, IXmlNode visitable) {
    XcodeMLVisitor visiter = _context.getVisitor();

    if (symbol == null) {
      throw new IllegalArgumentException();
    }

    EnumType typeId = symbol.getTypeId();
    if (typeId.isPrimitive()) {
      _writeSimplePrimitiveSymbolDecl(symbol);
      return true;
    }

    XcodeMLTypeManager typeManager = _context.getTypeManager();
    XcodeMLTypeManager.TypeList typeList = getTypeList(symbol.getDerivedName());

    if (typeList == null) return false;

    /*
     * The assumption that typeList.size() <= 2 is not valid for now.
     * m-hirano
     */
    // if (typeList.size() > 2) {
    // _context.debugPrintLine("Type list count > 2.");
    // _context.debugPrintLine(typeList.toString());
    // _context.setLastErrorMessage(XmlNodeUtil.formatError(_invokeNodeStack.peek(),
    // EnumError.XCODEML_SEMANTICS,
    // XmlNodeUtil.getElementName(_invokeNodeStack.peek())));
    // return false;
    // }

    IXmlTypeTableChoice topTypeChoice = typeList.getFirst();
    IXmlTypeTableChoice lowTypeChoice = typeList.getLast();

    // ================
    // Top type element
    // ================
    if (topTypeChoice instanceof FbasicType) {
      FbasicType basicTypeElem = (FbasicType) topTypeChoice;
      if (writeBasicType(basicTypeElem, typeList) == false) return false;
    } else if (topTypeChoice instanceof FstructType) {
      FstructType structTypeElem = (FstructType) topTypeChoice;
      String aliasStructTypeName = typeManager.getAliasTypeName(structTypeElem.getType());
      writeToken(toCaseSensitive("TYPE") + "(" + aliasStructTypeName + ")");
    } else if (topTypeChoice instanceof FfunctionType) {
      return writeFunctionSymbol(symbol, (FfunctionType) topTypeChoice, visitable);
    }

    _writeDeclAttr(topTypeChoice, lowTypeChoice);

    // ================
    // Low type element
    // ================
    if (lowTypeChoice instanceof FbasicType) {
      FbasicType basicTypeElem = (FbasicType) lowTypeChoice;
      String refName = basicTypeElem.getRef();
      EnumType refTypeId = EnumType.getTypeIdFromXcodemlTypeName(refName);
      assert (refTypeId != null);

      writeToken(" :: ");
      writeToken(symbol.getSymbolName());

      DefModelArraySubscriptSequence basicTypeChoice = null;
      List<IXmlNode> list = basicTypeElem.getIndexRangeOrArrayIndex();
      if (list != null && list.size() > 0) {
        basicTypeChoice = new DefModelArraySubscriptSequence(list);
      }
      if (basicTypeChoice != null) {
        if (basicTypeChoice instanceof DefModelArraySubscriptSequence) {
          if (visiter.invokeEnter(basicTypeChoice) == false) {
            return false;
          }
        }
      }

      CoShape coShape = basicTypeElem.getCoShape();
      if (coShape != null) {
        if (!visiter.invokeEnter(coShape)) return false;
      }

    } else if (lowTypeChoice instanceof FstructType) {
      writeToken(" :: ");
      writeToken(symbol.getSymbolName());
    }

    return true;
  }

  /**
   * Call enter method of node, and write delimiter.
   *
   * @param nodeArray IXmlNode array.
   * @param delim Delimiter.
   * @return true/false
   */
  private boolean _invokeEnterAndWriteDelim(IXmlNode[] nodeArray, String delim) {
    XcodeMLVisitor visiter = _context.getVisitor();

    if (nodeArray == null) {
      // Succeed forcibly.
      return true;
    }

    int nodeCount = 0;
    for (IXmlNode node : nodeArray) {
      if (nodeCount > 0) {
        writeToken(delim);
      }
      if (visiter.invokeEnter(node) == false) {
        return false;
      }

      ++nodeCount;
    }
    return true;
  }

  /**
   * Checks if object represents a constant expression.
   *
   * @param node Instance of IXmlNode
   * @param parent parent node
   * @return true if node represents a constant expression.
   */
  private boolean _isConstantExpr(IXmlNode node, IXmlNode parent) {
    if (node instanceof UnaryMinusExpr) {
      assert (parent != null);

      if ((parent instanceof UnaryMinusExpr) == false) {
        node = XmlNodeUtil.getXmlNodeChoice((UnaryMinusExpr) node);
      }
    }

    if ((node instanceof FintConstant)
        || (node instanceof FlogicalConstant)
        || (node instanceof FcharacterConstant)
        || (node instanceof FrealConstant)
        || (node instanceof FcomplexConstant)
        || (node instanceof Value)) return true;
    else return false;
  }

  /**
   * Make internal symbol from symbol name and type name.
   *
   * @param symbolName Symbol name.
   * @param typeName Type name.
   * @return Instance of XmlSymbol.
   */
  private XmlSymbol _makeSymbol(String symbolName, String typeName) {
    if (StringUtils.isNullOrEmpty(symbolName)) {
      // Symbol name is empty.
      return null;
    }

    XcodeMLTypeManager typeManager = _context.getTypeManager();

    if (StringUtils.isNullOrEmpty(typeName)) {
      Id idElem = typeManager.findSymbol(symbolName);
      if (idElem == null) {
        // Symbol not found.
        return null;
      }
      typeName = idElem.getType();
      if (StringUtils.isNullOrEmpty(typeName)) {
        // Type name of symbol is empty.
        return null;
      }
    }

    if (symbolName.equals("**")
        || symbolName.equals("*")
        || symbolName.equals("/")
        || symbolName.equals("+")
        || symbolName.equals("-")
        || symbolName.equals("//")) {
      symbolName = toCaseSensitive("OPERATOR(") + symbolName + ")";
    } else if (symbolName.equals("=")) {
      symbolName = toCaseSensitive("ASSIGNMENT(") + symbolName + ")";
    } else if (symbolName.startsWith(".") && symbolName.endsWith(".")) {
      symbolName = toCaseSensitive("OPERATOR(") + symbolName + ")";
    }

    XmlSymbol symbol = null;
    EnumType typeId = EnumType.getTypeIdFromXcodemlTypeName(typeName);
    if (typeId.isPrimitive()) {
      symbol = new XmlSymbol(symbolName, typeId);
    } else {
      symbol = new XmlSymbol(symbolName, typeId, typeName);
    }

    return symbol;
  }

  /**
   * Make internal symbol from name element.
   *
   * @param nameElem Instance of Name.
   * @return Instance of XmlSymbol.
   */
  private XmlSymbol _makeSymbol(Name nameElem) {
    if (nameElem == null) {
      // Instance is null.
      return null;
    }

    String symbolName = nameElem.getValue();
    return _makeSymbol(symbolName, nameElem.getType());
  }

  /**
   * Decompile "arguments" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   *      variable = function(<span class="Strong">arg1, arg2</span>)<br/>
   *      call subroutine(<span class="Strong">arg1, arg2</span>)<br/>
   * </div></code>
   */
  @Override
  public boolean enter(Arguments visitable) {
    // DONE: Arguments
    List<IXmlNode> list = visitable.getFintConstantOrFrealConstantOrFcomplexConstant();
    IXmlNode[] array = (IXmlNode[]) list.toArray(new IXmlNode[0]);
    if (_invokeEnterAndWriteDelim(array, ", ") == false) {
      return false;
    }

    return true;
  }

  /** Decompile child group of "FbasicType" element in XcodeML/F. */
  @Override
  public boolean enter(DefModelArraySubscriptSequence visitable) {
    // DONE: DefModelArraySubscriptSequence
    IXmlNode[] arraySubscriptChoice = visitable.getIndexRangeOrArrayIndex();
    if (arraySubscriptChoice != null) {
      if (writeIndexRangeArray(arraySubscriptChoice) == false) {
        return false;
      }
    }

    return true;
  }

  /** Decompile child group of "FbasicType" element in XcodeML/F. */
  @Override
  public boolean enter(CoShape visitable) {
    // DONE: CoShape
    List<IndexRange> list = visitable.getIndexRange();
    IndexRange[] indexRange = (IndexRange[]) list.toArray(new IndexRange[0]);
    if (indexRange != null) {
      if (!writeCoIndexRangeArray(indexRange)) return false;
    }

    return true;
  }

  /**
   * Decompile "FassignStatement" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   *
   * @example <code><div class="Example">
   * PROGRAM main<br/>
   * <div class="Indent1">
   *      INTEGER :: int_variable<br/>
   *      <div class="Strong">
   *      int_variable = 0<br/>
   *      </div>
   *      (any expression statement ...)<br/>
   * </div>
   * END PROGRAM main<br/>
   * </div></code>
   */
  @Override
  public boolean enter(FassignStatement visitable) {
    // DONE: FassignStatement
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    List<IXmlNode> content = visitable.getContent();
    // Left side
    IXmlNode leftExpr = (content != null && content.size() >= 1) ? content.get(0) : null;
    // Right side
    IXmlNode rightExpr = (content != null && content.size() >= 2) ? content.get(1) : null;

    assert (leftExpr != null && rightExpr != null); // Left side and right side are required

    if (writeBinaryExpr(leftExpr, rightExpr, "=", false) == false) {
      return false;
    }

    if (!updateCodeLine(visitable)) {
      return false;
    }

    return true;
  }

  /**
   * Decompile "condition" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * IF <span class="Strong">(variable == 1)</span> THEN<br/>
   * <div class="Indent1">
   *      (any statement...)<br/>
   * </div>
   * ELSE<br/>
   * <div class="Indent1">
   *      (any statement...)<br/>
   * </div>
   * END IF<br/>
   * </div></code>
   */
  @Override
  public boolean enter(Condition visitable) {
    XcodeMLVisitor visiter = _context.getVisitor();
    // DONE: Condition

    writeToken("(");

    IXmlNode node = XmlNodeUtil.getXmlNodeChoice(visitable);
    if (visiter.invokeEnter(node) == false) {
      return false;
    }

    writeToken(")");
    return true;
  }

  /**
   * Decompile "ContinueStatement" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * DO, variable = 1, 10<br/>
   * <div class="Indent1">
   *      (any statement...)<br/>
   *      <div class="Strong">
   *      CONTINUE<br/>
   *      </div>
   * </div>
   * END DO<br/>
   * </div></code>
   */
  @Override
  public boolean enter(ContinueStatement visitable) {
    // DONE: ContinueStatement
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    writeToken(toCaseSensitive(" CONTINUE"));
    if (!updateCodeLine(visitable)) {
      return false;
    }

    return true;
  }

  /**
   * Decompile "divExpr" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   *
   * @example <code><div class="Example">
   * variable = <span class="Strong">(&lt;any expression&gt; / &lt;any expression&gt;)</span><br/>
   * </div></code>
   */
  @Override
  public boolean enter(DivExpr visitable) {
    // DONE: DivExpr
    List<IXmlNode> list = visitable.getContent();
    if (list == null || list.size() < 2) return false;
    IXmlNode leftExpr = list.get(0);
    IXmlNode rightExpr = list.get(1);
    if (writeBinaryExpr(leftExpr, rightExpr, "/", true) == false) {
      return false;
    }

    return true;
  }

  /**
   * Decompile "exprStatement" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   *
   * @example <code><div class="Example">
   * PROGRAM main<br/>
   * <div class="Indent1">
   *      INTEGER :: int_variable<br/>
   *      TYPE(USER_TYPE) :: derived_variable<br/>
   *      <div class="Strong">
   *      int_variable = 0<br/>
   *      (any expression statement...)<br/>
   *      </div>
   * </div>
   * END PROGRAM main<br/>
   * </div></code>
   */
  @Override
  public boolean enter(ExprStatement visitable) {
    XcodeMLVisitor visiter = _context.getVisitor();
    // DONE: ExprStatement
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    if (visiter.invokeEnter(XmlNodeUtil.getXmlNodeChoice(visitable)) == false) {
      return false;
    }

    if (!updateCodeLine(visitable)) {
      return false;
    }

    return true;
  }

  /**
   * Decompile "externDecl" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * <div class="Strong">
   * EXTERNAL function_name<br/>
   * </div>
   * </div></code>
   */
  @Override
  public boolean enter(ExternDecl visitable) {
    // DONE: ExternDecl
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    Name nameElem = visitable.getName();
    String externalName = nameElem.getValue();

    writeToken(toCaseSensitive("EXTERNAL "));
    writeToken(externalName);

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  /**
   * Decompile "FallocateStatement" element in XcodeML/F.
   *
   * @param visitable FallocateStatement
   * @example <code><div class="Example">
   *      <div class="Strong">
   *      ALLOCATE (ptr, ptr_array(10), STAT = error)<br/>
   *      </div>
   * </div></code>
   */
  @Override
  public boolean enter(FallocateStatement visitable) {
    // XcodeMLVisitor visiter = _context.getVisitor();
    // DONE: FallocateStatement
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    writeToken(toCaseSensitive("ALLOCATE ("));

    List<Alloc> list = visitable.getAlloc();
    Alloc[] array = (Alloc[]) list.toArray(new Alloc[0]);
    if (_invokeEnterAndWriteDelim(array, ", ") == false) {
      return false;
    }

    String statName = visitable.getStatName();
    if (StringUtils.isNullOrEmpty(statName) == false) {
      writeToken(", ");
      writeToken(toCaseSensitive("STAT = "));
      writeToken(statName);
    }

    writeToken(")");
    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  /**
   * Decompile "FarrayConstructor" element in XcodeML/F.
   *
   * @param visitable FarrayConstructor
   * @example <code><div class="Example">
   *      array = <span class="Strong">(/ 1, 2, (I, I = 1, 10, 2) /)</span><br/>
   * </div></code>
   */
  @Override
  public boolean enter(FarrayConstructor visitable) {
    // DONE: FarrayConstructor
    writeToken("(/ ");

    List<IXmlNode> list = visitable.getDefModelExpr();
    IXmlNode[] array = (IXmlNode[]) list.toArray(new IXmlNode[0]);
    if (_invokeEnterAndWriteDelim(array, ", ") == false) {
      return false;
    }

    writeToken(" /)");

    return true;
  }

  /**
   * Decompile "FarrayRef" element in XcodeML/F.
   *
   * @param visitable FarrayRef
   * @example <code><div class="Example">
   *      array = <span class="Strong">int_array_variable(10, 1:10, 1:, :)</span><br/>
   * </div></code>
   */
  @Override
  public boolean enter(FarrayRef visitable) {
    XcodeMLVisitor visiter = _context.getVisitor();
    // DONE: FarrayRef
    if (visiter.invokeEnter(visitable.getVarRef()) == false) {
      return false;
    }

    writeToken("(");

    List<IXmlNode> list = visitable.getIndexRangeOrArrayIndexOrFarrayConstructor();
    IXmlNode[] array = (IXmlNode[]) list.toArray(new IXmlNode[0]);
    if (_invokeEnterAndWriteDelim(array, ", ") == false) {
      return false;
    }

    writeToken(")");

    return true;
  }

  /**
   * Decompile "FcoArrayRef" element in XcodeML/F.
   *
   * @param visitable FcoArrayRef
   * @example <code><div class="Example">
   *      array = <span class="Strong">int_coarray_variable[10, 1:10, 1:, *]</span><br/>
   * </div></code>
   */
  @Override
  public boolean enter(FcoArrayRef visitable) {
    XcodeMLVisitor visiter = _context.getVisitor();
    // DONE: FcoArrayRef
    if (visiter.invokeEnter(visitable.getVarRef()) == false) {
      return false;
    }

    writeToken("[");

    List<ArrayIndex> list = visitable.getArrayIndex();
    ArrayIndex[] array = (ArrayIndex[]) list.toArray(new ArrayIndex[0]);
    if (_invokeEnterAndWriteDelim(array, ", ") == false) {
      return false;
    }

    writeToken("]");

    return true;
  }

  /**
   * Decompile "FbackspaceStatement" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * <div class="Strong">
   * BACKSPACE (UNIT=1, ...)<br/>
   * </div>
   * </div></code>
   */
  @Override
  public boolean enter(FbackspaceStatement visitable) {
    XcodeMLVisitor visiter = _context.getVisitor();
    // DONE: FbackspaceStatement
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    writeToken(toCaseSensitive("BACKSPACE "));
    if (visiter.invokeEnter(visitable.getNamedValueList()) == false) {
      return false;
    }

    if (!updateCodeLine(visitable)) {
      return false;
    }

    return true;
  }

  /**
   * Decompile "FcaseLabel" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * SELECT CASE (variable)<br/>
   * <div class="Strong">
   * CASE (1)<br/>
   * <div class="Indent1">
   *      (any statement...)<br/>
   * </div>
   * </div>
   * <div class="Strong">
   * CASE (2)<br/>
   * <div class="Indent1">
   *      (any statement...)<br/>
   * </div>
   * </div>
   * <div class="Strong">
   * CASE DEFAULT<br/>
   * <div class="Indent1">
   *      (any statement...)<br/>
   * </div>
   * </div>
   * END SELECT<br/>
   * </div></code>
   */
  @Override
  public boolean enter(FcaseLabel visitable) {
    // DONE: FcaseLabel
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    writeToken(toCaseSensitive("CASE"));

    List<IXmlNode> list = visitable.getValueOrIndexRange();
    IXmlNode[] caseLabelChoice = (IXmlNode[]) list.toArray(new IXmlNode[0]);
    if ((caseLabelChoice != null) && (caseLabelChoice.length > 0)) {
      writeToken(" (");
      if (_invokeEnterAndWriteDelim(caseLabelChoice, ", ") == false) {
        return false;
      }
      writeToken(")");
    } else {
      writeToken(toCaseSensitive(" DEFAULT"));
    }

    String constuctName = visitable.getConstructName();
    if (StringUtils.isNullOrEmpty(constuctName) == false) {
      writeToken(" ");
      writeToken(constuctName);
    }

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  /**
   * Decompile "FcharacterConstant" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * PROGRAM main<br/>
   * <div class="Indent1">
   *      CHARACTER(LEN=10) :: string_variable = <span class="Strong">"text"</span><br/>
   * </div>
   * END PROGRAM main<br/>
   * </div></code>
   */
  @Override
  public boolean enter(FcharacterConstant visitable) {
    // DONE: FcharacterConstant
    String kind = visitable.getKind();
    if (StringUtils.isNullOrEmpty(kind) == false) {
      writeToken(kind + "_");
    }

    writeLiteralString(visitable.getValue());

    return true;
  }

  /**
   * Decompile "FcharacterRef" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   *      substring = <span class="Strong">char_variable(1:10)</span><br/>
   * </div></code>
   */
  @Override
  public boolean enter(FcharacterRef visitable) {
    XcodeMLVisitor visiter = _context.getVisitor();
    // DONE: FcharacterRef
    if (visiter.invokeEnter(visitable.getVarRef()) == false) {
      return false;
    }

    writeToken("(");

    if (visiter.invokeEnter(visitable.getIndexRange()) == false) {
      return false;
    }

    writeToken(")");

    return true;
  }

  /**
   * Decompile "FcloseStatement" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * <div class="Strong">
   * CLOSE (UNIT=1, ...)<br/>
   * </div>
   * </div></code>
   */
  @Override
  public boolean enter(FcloseStatement visitable) {
    XcodeMLVisitor visiter = _context.getVisitor();
    // DONE: FcloseStatement
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    writeToken(toCaseSensitive("CLOSE "));
    if (visiter.invokeEnter(visitable.getNamedValueList()) == false) {
      return false;
    }

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  /**
   * Decompile "FcommonDecl" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * <div class="Strong">
   * COMMON /NAME/ variable1, array, // variable3, variable4<br/>
   * </div>
   * </div></code>
   */
  @Override
  public boolean enter(FcommonDecl visitable) {
    // DONE: FcommonDecl
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    writeToken(toCaseSensitive("COMMON "));

    List<VarList> list = visitable.getVarList();
    VarList[] array = (VarList[]) list.toArray(new VarList[0]);
    if (_invokeEnterAndWriteDelim(array, ", ") == false) {
      return false;
    }

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  /**
   * Decompile "FcomplexConstant" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * PROGRAM main<br/>
   * <div class="Indent1">
   *      COMPLEX cmp_variable = <span class="Strong">(1.0, 2.0)</span><br/>
   * </div>
   * END PROGRAM main<br/>
   * </div></code>
   */
  @Override
  public boolean enter(FcomplexConstant visitable) {
    XcodeMLVisitor visiter = _context.getVisitor();
    // DONE: FcomplexConstant
    // IDefModelExprChoice realPart = visitable.getDefModelExpr1();
    // IDefModelExprChoice imaginalPart = visitable.getDefModelExpr2();
    List<IXmlNode> list = visitable.getContent();
    IXmlNode realPart = list.get(0) != null ? list.get(0) : null;
    IXmlNode imaginalPart = list.get(1) != null ? list.get(1) : null;

    if ((realPart == null) || (imaginalPart == null)) {
      _context.setLastErrorMessage(
          XmlNodeUtil.formatError(
              visitable, EnumError.XCODEML_SEMANTICS, XmlNodeUtil.getElementName(visitable)));
      return false;
    }

    String typeName = visitable.getType();
    if (StringUtils.isNullOrEmpty(typeName) == false) {
      XcodeMLTypeManager typeManager = _context.getTypeManager();
      String bottomTypeName = typeManager.getBottomTypeName(typeName);
      if (bottomTypeName == null) {
        _context.setLastErrorMessage(
            XmlNodeUtil.formatError(
                visitable,
                EnumError.XCODEML_TYPE_NOT_FOUND,
                XmlNodeUtil.getElementName(visitable),
                typeName));
        return false;
      }

      EnumType typeId = EnumType.getTypeIdFromXcodemlTypeName(bottomTypeName);
      if (typeId != EnumType.DERIVED && typeId != EnumType.COMPLEX) {
        _context.setLastErrorMessage(
            XmlNodeUtil.formatError(
                visitable,
                EnumError.XCODEML_TYPE_MISMATCH,
                XmlNodeUtil.getElementName(visitable),
                typeName,
                "Fcomplex"));
        return false;
      }
    }

    if ((_isConstantExpr(realPart, visitable) == false)
        || (_isConstantExpr(imaginalPart, visitable) == false)) {
      writeToken(toCaseSensitive("CMPLX"));
    }

    writeToken("(");
    if (visiter.invokeEnter(realPart) == false) {
      return false;
    }
    writeToken(", ");
    if (visiter.invokeEnter(imaginalPart) == false) {
      return false;
    }
    writeToken(")");

    return true;
  }

  /**
   * Decompile "FconcatExpr" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   *
   * @example <code><div class="Example">
   * variable = <span class="Strong">(&lt;any expression&gt; // &lt;any expression&gt;)</span><br/>
   * </div></code>
   */
  @Override
  public boolean enter(FconcatExpr visitable) {
    // DONE: FconcatExpr
    List<IXmlNode> list = visitable.getContent();
    IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
    IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
    if (writeBinaryExpr(leftExpr, rightExpr, "//", true) == false) {
      return false;
    }

    return true;
  }

  /**
   * Decompile "FcontainsStatement" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * PROGRAM main<br/>
   * <div class="Indent1">
   *      INTEGER :: int_variable<br/>
   *      (any statement...)<br/>
   * </div>
   * <br/>
   * <div class="Strong">
   * CONTAINS<br/>
   * </div>
   * <div class="Indent1">
   *      SUBROUTINE sub()<br/>
   *      (any statement...)<br/>
   *      END SUBROUTINE sub<br/>
   *      <br/>
   *      FUNCTION func()<br/>
   *      (any statement...)<br/>
   *      END SUBROUTINE sub<br/>
   * </div>
   * <br/>
   * END PROGRAM main<br/>
   * </div></code>
   */
  @Override
  public boolean enter(FcontainsStatement visitable) {
    // DONE: FcontainsStatement
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    writeToken(toCaseSensitive("CONTAINS"));

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  /**
   * Decompile "FcycleStatement" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * DO_NAME: DO, variable = 1, 10<br/>
   * <div class="Indent1">
   *      (any statement...)<br/>
   *      <div class="Strong">
   *      CYCLE DO_NAME<br/>
   *      </div>
   * </div>
   * END DO DO_NAME<br/>
   * </div></code>
   */
  @Override
  public boolean enter(FcycleStatement visitable) {
    // DONE: FcycleStatement
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    writeToken(toCaseSensitive("CYCLE"));

    String constuctName = visitable.getConstructName();
    if (StringUtils.isNullOrEmpty(constuctName) == false) {
      writeToken(" ");
      writeToken(constuctName);
    }

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  /**
   * Decompile "FdataDecl" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * <div class="Strong">
   * DATA variable1, variable2 /2*0/, &<br/>
   *      array1 /10*1/, &<br/>
   *      (array2(i), i = 1, 10, 2) /5*1/<br/>
   * </div>
   * </div></code>
   */
  @Override
  public boolean enter(FdataDecl visitable) {

    // XcodeMLVisitor visiter = _context.getVisitor();

    // DONE: FdataDecl
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    writeToken(toCaseSensitive("DATA "));

    List<IXmlNode> list = visitable.getVarListAndValueList();

    // ((varList, valueList)+)
    IXmlNode[] array = (IXmlNode[]) list.toArray(new IXmlNode[0]);
    if (_invokeEnterAndWriteDelim(array, ", ") == false) {
      return false;
    }

    if (!updateCodeLine(visitable)) {
      return false;
    }

    return true;
  }

  /**
   * Decompile child group of "FdataDecl" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * DATA <span class="Strong">variable1, variable2 /1, 2/</span>, &<br/>
   *      <span class="Strong">array1 /10*1/</span>, &<br/>
   *      <span class="Strong">(array2(i), i = 1, 10, 2) /5*1/</span><br/>
   * </div></code>
   */
  @Override
  public boolean enter(FdataDeclSequence visitable) {
    XcodeMLVisitor visiter = _context.getVisitor();
    // DONE: FdataDeclSequence

    if (visiter.invokeEnter(visitable.getVarList()) == false) {
      return false;
    }

    writeToken(" /");

    if (visiter.invokeEnter(visitable.getValueList()) == false) {
      return false;
    }

    writeToken("/");

    return true;
  }

  /**
   * Decompile "FdeallocateStatement" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   *      <div class="Strong">
   *      DEALLOCATE (ptr, ptr_array, STAT = error)<br/>
   *      </div>
   * </div></code>
   */
  @Override
  public boolean enter(FdeallocateStatement visitable) {
    // DONE: FdeallocateStatement
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    writeToken(toCaseSensitive("DEALLOCATE ("));

    List<Alloc> list = visitable.getAlloc();
    Alloc[] array = (Alloc[]) list.toArray(new Alloc[0]);
    if (_invokeEnterAndWriteDelim(array, ", ") == false) {
      return false;
    }

    String statName = visitable.getStatName();
    if (StringUtils.isNullOrEmpty(statName) == false) {
      writeToken(", ");
      writeToken(toCaseSensitive("STAT = "));
      writeToken(statName);
    }

    writeToken(")");

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  /**
   * Decompile "FdoLoop" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   *      array = (/ 1, 2, <span class="Strong">(I, I = 1, 10, 2)</span> /)<br/>
   * </div></code>
   */
  @Override
  public boolean enter(FdoLoop visitable) {
    XcodeMLVisitor visiter = _context.getVisitor();
    // DONE: FdoLoop

    writeToken("(");

    List<Value> list = visitable.getValue();
    Value[] array = (Value[]) list.toArray(new Value[0]);
    if (_invokeEnterAndWriteDelim(array, ", ") == false) {
      return false;
    }

    if (array.length > 0) {
      writeToken(", ");
    }

    Var varElem = visitable.getVar();
    if (visiter.invokeEnter(varElem) == false) {
      return false;
    }

    writeToken(" = ");
    if (visiter.invokeEnter(visitable.getIndexRange()) == false) {
      return false;
    }

    writeToken(")");

    return true;
  }

  /**
   * Decompile "FdoStatement" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * <div class="Strong">
   * DO<br/>
   * <div class="Indent1">
   *      (any statement...)<br/>
   * </div>
   * END DO<br/>
   * </div>
   * <br/>
   * <div class="Strong">
   * DO, variable = 1, 10<br/>
   * <div class="Indent1">
   *      (any statement...)<br/>
   * </div>
   * END DO<br/>
   * </div>
   * </div></code>
   */
  @Override
  public boolean enter(FdoStatement visitable) {
    XcodeMLVisitor visiter = _context.getVisitor();
    // DONE: FdoStatement
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    String constuctName = visitable.getConstructName();
    if (StringUtils.isNullOrEmpty(constuctName) == false) {
      writeToken(constuctName);
      writeToken(": ");
    }

    writeToken(toCaseSensitive("DO"));

    FdoStatementSequence doStatementSequence =
        new FdoStatementSequence(visitable.getVar(), visitable.getIndexRange());
    if (doStatementSequence != null
        && (doStatementSequence.getVar() != null || doStatementSequence.getIndexRange() != null)) {
      writeToken(" ");
      if (visiter.invokeEnter(doStatementSequence) == false) {
        return false;
      }
    }

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  @Override
  public void leave(FdoStatement visitable) {
    writeToken(toCaseSensitive("END DO"));

    String constuctName = visitable.getConstructName();
    if (StringUtils.isNullOrEmpty(constuctName) == false) {
      writeToken(" ");
      writeToken(constuctName);
    }
    updateCodeLine(visitable);

    return;
  }

  /**
   * Decompile child group of "FdoStatement" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * DO, <span class="Strong">variable = 1, 10</span><br/>
   * <div class="Indent1">
   *      (any statement...)<br/>
   * </div>
   * END DO<br/>
   * </div></code>
   */
  @Override
  public boolean enter(FdoStatementSequence visitable) {
    XcodeMLVisitor visiter = _context.getVisitor();
    // DONE: FdoStatementSequence
    if (visiter.invokeEnter(visitable.getVar()) == false) {
      return false;
    }

    writeToken(" = ");

    if (visiter.invokeEnter(visitable.getIndexRange()) == false) {
      return false;
    }

    return true;
  }

  /**
   * Decompile "FdoWhileStatement" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * <div class="Strong">
   * DO, WHILE (variable > 0)<br/>
   * <div class="Indent1">
   *      (any statement...)<br/>
   * </div>
   * END DO<br/>
   * </div>
   * </div></code>
   */
  @Override
  public boolean enter(FdoWhileStatement visitable) {
    XcodeMLVisitor visiter = _context.getVisitor();

    // DONE: FdoWhileStatement
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    String constuctName = visitable.getConstructName();
    if (StringUtils.isNullOrEmpty(constuctName) == false) {
      writeToken(constuctName);
      writeToken(": ");
    }

    writeToken(toCaseSensitive("DO WHILE "));
    if (visiter.invokeEnter(visitable.getCondition()) == false) {
      return false;
    }

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  @Override
  public void leave(FdoWhileStatement visitable) {

    writeToken(toCaseSensitive("END DO"));

    String constuctName = visitable.getConstructName();
    if (StringUtils.isNullOrEmpty(constuctName) == false) {
      writeToken(" ");
      writeToken(constuctName);
    }
    updateCodeLine(visitable);
  }

  /**
   * Decompile "FendFileStatement" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * <div class="Strong">
   * ENDFILE (UNIT=1, ...)<br/>
   * </div>
   * </div></code>
   */
  @Override
  public boolean enter(FendFileStatement visitable) {
    XcodeMLVisitor visiter = _context.getVisitor();

    // DONE: FendFileStatement
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    writeToken(toCaseSensitive("ENDFILE "));
    if (visiter.invokeEnter(visitable.getNamedValueList()) == false) {
      return false;
    }

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  /**
   * Decompile "FentryDecl" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * FUNCTION func(arg) RESULT(retval)
   * <div class="Indent1">
   *      (any declaration...)<br/>
   * </div>
   * <div class="Strong">
   * ENTRY func_entry(arg) RESULT(retval)<br/>
   * </div>
   * <div class="Indent1">
   *      (any statement...)<br/>
   * </div>
   * END FUNCTION func<br/>
   * </div></code>
   */
  @Override
  public boolean enter(FentryDecl visitable) {
    XcodeMLTypeManager typeManager = _context.getTypeManager();
    XcodeMLVisitor visiter = _context.getVisitor();

    // DONE: FentryDecl
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    Name functionNameElem = visitable.getName();
    IXmlTypeTableChoice typeChoice = typeManager.findType(functionNameElem);
    if (typeChoice == null) {
      _context.setLastErrorMessage(
          XmlNodeUtil.formatError(
              visitable, EnumError.XCODEML_TYPE_NOT_FOUND, functionNameElem.getType()));
      return false;
    } else if ((typeChoice instanceof FfunctionType) == false) {
      _context.setLastErrorMessage(
          XmlNodeUtil.formatError(
              visitable,
              EnumError.XCODEML_TYPE_MISMATCH,
              "function definition",
              typeChoice.getClass().getSimpleName(),
              "FfunctionType"));
      return false;
    }

    FfunctionType functionTypeElem = (FfunctionType) typeChoice;
    String returnTypeName = functionTypeElem.getReturnType();
    EnumType typeId = EnumType.getTypeIdFromXcodemlTypeName(returnTypeName);
    if (XmlNodeUtil.isBoolean(functionTypeElem.isIsProgram())) {
      // =======
      // PROGRAM
      // =======
      _context.setLastErrorMessage(
          XmlNodeUtil.formatError(
              visitable,
              EnumError.XCODEML_TYPE_MISMATCH,
              "function definition",
              "PROGRAM",
              "FUNCTION or SUBROUTINE"));
      return false;
    } else {
      // ======================
      // FUNCTION or SUBROUTINE
      // ======================
      writeToken(toCaseSensitive("ENTRY"));
      writeToken(" ");
      writeToken(functionNameElem.getValue());
      writeToken("(");

      if (visiter.invokeEnter(functionTypeElem.getParams()) == false) {
        return false;
      }

      writeToken(")");
      if (typeId != EnumType.VOID) {
        String functionResultName = functionTypeElem.getResultName();
        if (StringUtils.isNullOrEmpty(functionResultName) == false) {
          writeToken(toCaseSensitive(" RESULT(") + functionResultName + ")");
        }
      }
    }

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  /**
   * Decompile "FequivalenceDecl" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * <div class="Strong">
   * EQUIVALENCE (variable1, variable2), (variable3, variable4)<br/>
   * </div>
   * </div></code>
   */
  @Override
  public boolean enter(FequivalenceDecl visitable) {

    // DONE: FequivalenceDecl
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    writeToken(toCaseSensitive("EQUIVALENCE "));

    writeToken("(");
    List<IXmlNode> list = visitable.getVarRefAndVarList();
    IXmlNode[] array = (IXmlNode[]) list.toArray(new IXmlNode[0]);
    if (_invokeEnterAndWriteDelim(array, ", ") == false) {
      return false;
    }
    writeToken(")");

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  /**
   * Decompile child group of "FequivalenceDecl" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * EQUIVALENCE <span class="Strong">(variable1, variable2)</span>, <span class="Strong">(variable3, variable4)</span><br/>
   * </div></code>
   */
  @Override
  public boolean enter(FequivalenceDeclSequence visitable) {
    // DONE: FequivalenceDeclSequence
    XcodeMLVisitor visiter = _context.getVisitor();
    writeToken("(");
    if (visiter.invokeEnter(visitable.getVarRef()) == false) {
      return false;
    }

    writeToken(", ");

    if (visiter.invokeEnter(visitable.getVarList()) == false) {
      return false;
    }

    writeToken(")");

    return true;
  }

  /**
   * Decompile "FexitStatement" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * DO_NAME: DO, variable = 1, 10<br/>
   * <div class="Indent1">
   *      (any statement...)<br/>
   *      <div class="Strong">
   *      EXIT DO_NAME<br/>
   *      </div>
   * </div>
   * END DO DO_NAME<br/>
   * </div></code>
   */
  @Override
  public boolean enter(FexitStatement visitable) {

    // DONE: FexitStatement
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    writeToken(toCaseSensitive("EXIT"));

    String constuctName = visitable.getConstructName();
    if (StringUtils.isNullOrEmpty(constuctName) == false) {
      writeToken(" ");
      writeToken(constuctName);
    }

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  /**
   * Decompile "FformatDecl" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * <div class="Strong">
   * 1000 FORMAT (&lt;any format string&gt;)<br/>
   * </div>
   * </div></code>
   */
  @Override
  public boolean enter(FformatDecl visitable) {

    // DONE: FformatDecl
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    writeToken(toCaseSensitive("FORMAT "));
    writeToken(visitable.getFormat());

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  /**
   * Decompile "FfunctionDecl" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * INTERFACE interface_name<br/>
   * <div class="Indent1">
   *      <div class="Strong">
   *      SUBROUTINE sub1(arg1)<br/>
   *      <div class="Indent1">
   *          INTEGER arg1<br/>
   *      </div>
   *      END SUBROUTINE<br/>
   *      </div>
   *      MODULE PROCEDURE module_sub<br/>
   *      <br/>
   * </div>
   * <div class="Strong">
   * END INTERFACE<br/>
   * </div>
   * </div></code>
   */
  @Override
  public boolean enter(FfunctionDecl visitable) {
    XcodeMLTypeManager typeManager = _context.getTypeManager();
    XcodeMLVisitor visiter = _context.getVisitor();
    // DONE: FfunctionDecl
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    Name functionNameElem = visitable.getName();
    IXmlTypeTableChoice typeChoice = typeManager.findType(functionNameElem);
    if (typeChoice == null) {
      _context.setLastErrorMessage(
          XmlNodeUtil.formatError(
              visitable, EnumError.XCODEML_TYPE_NOT_FOUND, functionNameElem.getType()));
      return false;
    } else if ((typeChoice instanceof FfunctionType) == false) {
      _context.setLastErrorMessage(
          XmlNodeUtil.formatError(
              visitable,
              EnumError.XCODEML_TYPE_MISMATCH,
              "function definition",
              typeChoice.getClass().getSimpleName(),
              "FfunctionType"));
      return false;
    }

    FfunctionType functionTypeElem = (FfunctionType) typeChoice;
    String returnTypeName = functionTypeElem.getReturnType();
    EnumType typeId = EnumType.getTypeIdFromXcodemlTypeName(returnTypeName);
    if (XmlNodeUtil.isBoolean(functionTypeElem.isIsProgram())) {
      // =======
      // PROGRAM
      // =======
      _context.setLastErrorMessage(
          XmlNodeUtil.formatError(
              visitable,
              EnumError.XCODEML_TYPE_MISMATCH,
              "function definition",
              "PROGRAM",
              "FUNCTION or SUBROUTINE"));
      return false;
    } else if (typeId == EnumType.VOID) {
      // ==========
      // SUBROUTINE
      // ==========
      if (XmlNodeUtil.isBoolean(functionTypeElem.isIsRecursive())) {
        writeToken(toCaseSensitive("RECURSIVE"));
        writeToken(" ");
      }
      writeToken(toCaseSensitive("SUBROUTINE"));
      writeToken(" ");
      writeToken(functionNameElem.getValue());
      writeToken("(");

      if (visiter.invokeEnter(functionTypeElem.getParams()) == false) {
        return false;
      }

      writeToken(")");
    } else {
      // ========
      // FUNCTION
      // ========

      // Note:
      // In the function definition, do not output return type.

      /*
       * if (typeId == EnumType.DERIVED) { writeToken(returnTypeName); }
       * else { writeToken(typeId.fortranName()); } writeToken(" ");
       */
      if (XmlNodeUtil.isBoolean(functionTypeElem.isIsRecursive())) {
        writeToken(toCaseSensitive("RECURSIVE"));
        writeToken(" ");
      }
      writeToken(toCaseSensitive("FUNCTION"));
      writeToken(" ");
      writeToken(functionNameElem.getValue());
      writeToken("(");

      if (visiter.invokeEnter(functionTypeElem.getParams()) == false) {
        return false;
      }

      writeToken(")");
      String functionResultName = functionTypeElem.getResultName();
      if (StringUtils.isNullOrEmpty(functionResultName) == false) {
        writeToken(toCaseSensitive(" RESULT(") + functionResultName + ")");
      }
    }

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  @Override
  public void leave(FfunctionDecl visitable) {
    XcodeMLTypeManager typeManager = _context.getTypeManager();
    Name functionNameElem = visitable.getName();
    IXmlTypeTableChoice typeChoice = typeManager.findType(functionNameElem);
    FfunctionType functionTypeElem = (FfunctionType) typeChoice;
    String returnTypeName = functionTypeElem.getReturnType();
    EnumType typeId = EnumType.getTypeIdFromXcodemlTypeName(returnTypeName);

    assert (XmlNodeUtil.isBoolean(functionTypeElem.isIsProgram()) == false);
    if (typeId == EnumType.VOID) {
      // ==========
      // SUBROUTINE
      // ==========
      writeToken(toCaseSensitive("END SUBROUTINE"));
      writeToken(" ");
      writeToken(functionNameElem.getValue());
    } else {
      writeToken(toCaseSensitive("END FUNCTION"));
      writeToken(" ");
      writeToken(functionNameElem.getValue());
    }
    updateCodeLine(visitable);
  }

  /**
   * Decompile "FfunctionDefinition" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * <div class="Strong">
   * PROGRAM main<br/>
   * </div>
   * <div class="Indent1">
   *      INTEGER :: int_variable<br/>
   *      (any statement...)<br/>
   * </div>
   * <br/>
   * CONTAINS
   * <div class="Indent1">
   *      <div class="Strong">
   *      SUBROUTINE sub()<br/>
   *      </div>
   *      (any statement...)<br/>
   *      <div class="Strong">
   *      END SUBROUTINE sub<br/>
   *      </div>
   *      <br/>
   *      <div class="Strong">
   *      FUNCTION func()<br/>
   *      </div>
   *      (any statement...)<br/>
   *      <div class="Strong">
   *      END SUBROUTINE sub<br/>
   *      </div>
   * </div>
   * <br/>
   * <div class="Strong">
   * END PROGRAM main<br/>
   * </div>
   * </div></code>a
   */
  @Override
  public boolean enter(FfunctionDefinition visitable) {
    XcodeMLTypeManager typeManager = _context.getTypeManager();
    XcodeMLVisitor visiter = _context.getVisitor();

    // DONE: FfunctionDefinition
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    Name functionNameElem = visitable.getName();
    IXmlTypeTableChoice typeChoice = typeManager.findType(functionNameElem);
    if (typeChoice == null) {
      _context.setLastErrorMessage(
          XmlNodeUtil.formatError(
              visitable, EnumError.XCODEML_TYPE_NOT_FOUND, functionNameElem.getType()));
      return false;
    } else if ((typeChoice instanceof FfunctionType) == false) {
      _context.setLastErrorMessage(
          XmlNodeUtil.formatError(
              visitable,
              EnumError.XCODEML_TYPE_MISMATCH,
              "function definition",
              typeChoice.getClass().getSimpleName(),
              "FfunctionType"));
      return false;
    }

    FfunctionType functionTypeElem = (FfunctionType) typeChoice;
    String returnTypeName = functionTypeElem.getReturnType();
    EnumType typeId = EnumType.getTypeIdFromXcodemlTypeName(returnTypeName);
    if (XmlNodeUtil.isBoolean(functionTypeElem.isIsProgram())) {
      // =======
      // PROGRAM
      // =======
      writeToken(toCaseSensitive("PROGRAM"));
      writeToken(" ");
      writeToken(functionNameElem.getValue());
    } else if (typeId == EnumType.VOID) {
      // ==========
      // SUBROUTINE
      // ==========
      if (XmlNodeUtil.isBoolean(functionTypeElem.isIsRecursive())) {
        writeToken(toCaseSensitive("RECURSIVE"));
        writeToken(" ");
      }
      writeToken(toCaseSensitive("SUBROUTINE"));
      writeToken(" ");
      writeToken(functionNameElem.getValue());
      writeToken("(");

      if (visiter.invokeEnter(functionTypeElem.getParams()) == false) {
        return false;
      }

      writeToken(")");
    } else {
      // ========
      // FUNCTION
      // ========

      // Note:
      // In the function definition, do not output return type.

      /*
       * if (typeId == EnumType.DERIVED) { writeToken(returnTypeName); }
       * else { writeToken(typeId.fortranName()); } writeToken(" ");
       */
      if (XmlNodeUtil.isBoolean(functionTypeElem.isIsRecursive())) {
        writeToken(toCaseSensitive("RECURSIVE"));
        writeToken(" ");
      }
      writeToken(toCaseSensitive("FUNCTION"));
      writeToken(" ");
      writeToken(functionNameElem.getValue());
      writeToken("(");

      if (visiter.invokeEnter(functionTypeElem.getParams()) == false) {
        return false;
      }

      writeToken(")");
      String functionResultName = functionTypeElem.getResultName();
      if (StringUtils.isNullOrEmpty(functionResultName) == false) {
        writeToken(toCaseSensitive(" RESULT(") + functionResultName + ")");
      }
    }

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  /**
   * Decompile "FifStatement" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * <div class="Strong">
   * IF (variable == 1) THEN<br/>
   * <div class="Indent1">
   *      (any statement...)<br/>
   * </div>
   * ELSE<br/>
   * <div class="Indent1">
   *      (any statement...)<br/>
   * </div>
   * END IF<br/>
   * </div>
   * </div></code>
   */
  @Override
  public boolean enter(FifStatement visitable) {
    XcodeMLVisitor visiter = _context.getVisitor();
    // DONE: FifStatement
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    String constuctName = visitable.getConstructName();
    if (StringUtils.isNullOrEmpty(constuctName) == false) {
      writeToken(constuctName);
      writeToken(": ");
    }

    if (_context.isInvokeNodeOf(Else.class, 2)) {
      boolean isElseIf = false;
      Else elseNode = (Else) _context.getInvokeNode(2);
      // Is the start line number set?
      if (StringUtils.isNullOrEmpty(elseNode.getLineno())) {
        // This is an Else IF statement because the start line number of the Else element two above
        // is not set.
        isElseIf = true;
      }
      if (isElseIf) {
        // ELSE IF
        writeToken(toCaseSensitive("ELSE "));
      }
    }

    writeToken(toCaseSensitive("IF "));

    if (visiter.invokeEnter(visitable.getCondition()) == false) {
      return false;
    }
    writeToken(toCaseSensitive(" THEN"));

    if (!updateCodeLine(visitable)) {
      return false;
    }
    /*
     * if (visiter.invokeEnter(visitable.getThen()) == false) { return
     * false; }
     *
     * Else elseElem = visitable.getElse(); if (elseElem != null) {
     * writeToken("ELSE"); if (StringUtils.isNullOrEmpty(constuctName) ==
     * false) { writeToken(" "); writeToken(constuctName); }
     * updateCodeLine(visitable);
     *
     * if (visiter.invokeEnter(visitable.getElse()) == false) { return
     * false; } }
     */
    return true;
  }

  @Override
  public boolean enter(Else visitable) {
    if (visitable != null) {
      String constuctName = null;
      if (_context.isInvokeNodeOf(FifStatement.class, 1)) {
        FifStatement ifNode = (FifStatement) _context.getInvokeNode(1);
        constuctName = ifNode.getConstructName();
      }

      if (_context.isInvokeNodeOf(FifStatement.class, 1)) {
        writeToken(toCaseSensitive("ELSE"));
      } else if (_context.isInvokeNodeOf(FwhereStatement.class, 1)) {
        writeToken(toCaseSensitive("ELSEWHERE"));
      } else {
        writeToken(toCaseSensitive("ELSE"));
      }

      if (StringUtils.isNullOrEmpty(constuctName) == false) {
        writeToken(" ");
        writeToken(constuctName);
      }
      if (!updateCodeLine(visitable)) {
        return false;
      }
    }

    return true;
  }

  @Override
  public void leave(FifStatement visitable) {

    writeToken(toCaseSensitive("END IF"));

    String constuctName = visitable.getConstructName();
    if (StringUtils.isNullOrEmpty(constuctName) == false) {
      writeToken(" ");
      writeToken(constuctName);
    }
    updateCodeLine(visitable);
  }

  /**
   * Decompile "FinquireStatement" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * <div class="Strong">
   * INQUIRE (UNIT=1, ...)<br/>
   * </div>
   * <div class="Strong">
   * INQUIRE (IOLENGTH=variable) out_variable1, out_variable2<br/>
   * </div>
   * </div></code>
   */
  @Override
  public boolean enter(FinquireStatement visitable) {
    XcodeMLVisitor visiter = _context.getVisitor();
    // DONE: FinquireStatement
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    writeToken(toCaseSensitive("INQUIRE "));
    if (visiter.invokeEnter(visitable.getNamedValueList()) == false) {
      return false;
    }

    writeToken(" ");
    if (visiter.invokeEnter(visitable.getValueList()) == false) {
      return false;
    }

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  /**
   * Decompile "FintConstant" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * PROGRAM main<br/>
   * <div class="Indent1">
   *      INTEGER :: int_variable = <span class="Strong">10</span><br/>
   * </div>
   * END PROGRAM main<br/>
   * </div></code>
   */
  @Override
  public boolean enter(FintConstant visitable) {
    // DONE: FintConstant
    String content = visitable.getValue();
    if (StringUtils.isNullOrEmpty(content)) {
      _context.setLastErrorMessage(
          XmlNodeUtil.formatError(
              visitable, EnumError.XCODEML_SEMANTICS, XmlNodeUtil.getElementName(visitable)));
      return false;
    }

    String typeName = visitable.getType();
    if (StringUtils.isNullOrEmpty(typeName) == false) {
      XcodeMLTypeManager typeManager = _context.getTypeManager();
      String bottomTypeName = typeManager.getBottomTypeName(typeName);
      if (bottomTypeName == null) {
        _context.setLastErrorMessage(
            XmlNodeUtil.formatError(
                visitable,
                EnumError.XCODEML_TYPE_NOT_FOUND,
                XmlNodeUtil.getElementName(visitable),
                typeName));
        return false;
      }

      EnumType typeId = EnumType.getTypeIdFromXcodemlTypeName(bottomTypeName);
      if (typeId != EnumType.DERIVED && typeId != EnumType.INT) {
        _context.setLastErrorMessage(
            XmlNodeUtil.formatError(
                visitable,
                EnumError.XCODEML_TYPE_MISMATCH,
                XmlNodeUtil.getElementName(visitable),
                typeName,
                "Fint"));
        return false;
      }
    }

    String kind = visitable.getKind();
    if (StringUtils.isNullOrEmpty(kind) == false) {
      writeToken(content + "_" + kind);
    } else {
      writeToken(content);
    }

    return true;
  }

  /**
   * Decompile "FinterfaceDecl" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * <div class="Strong">
   * INTERFACE interface_name<br/>
   * </div>
   * <div class="Indent1">
   *      SUBROUTINE sub1(arg1)<br/>
   *      <div class="Indent1">
   *          INTEGER arg1<br/>
   *      </div>
   *      END SUBROUTINE<br/>
   *      MODULE PROCEDURE module_sub<br/>
   *      <br/>
   * </div>
   * <div class="Strong">
   * END INTERFACE<br/>
   * </div>
   * </div></code>
   */
  @Override
  public boolean enter(FinterfaceDecl visitable) {

    // DONE: FinterfaceDecl
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    String interfaceName = visitable.getName();

    writeToken(toCaseSensitive("INTERFACE"));
    if (XmlNodeUtil.isBoolean(visitable.isIsAssignment())) {
      writeToken(toCaseSensitive(" ASSIGNMENT(=)"));
    } else if (XmlNodeUtil.isBoolean(visitable.isIsOperator())) {
      writeToken(toCaseSensitive(" OPERATOR("));
      writeToken(visitable.getName());
      writeToken(")");
    } else {
      if (StringUtils.isNullOrEmpty(interfaceName) == false) {
        writeToken(" ");
        writeToken(visitable.getName());
      }
    }

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  @Override
  public void leave(FinterfaceDecl visitable) {
    writeToken(toCaseSensitive("END INTERFACE"));
    updateCodeLine(visitable);
  }

  /**
   * Decompile "FlogicalConstant" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * PROGRAM main<br/>
   * <div class="Indent1">
   *      LOGICAL log_variable = <span class="Strong">.TRUE.</span><br/>
   * </div>
   * END PROGRAM main<br/>
   * </div></code>
   */
  @Override
  public boolean enter(FlogicalConstant visitable) {
    // DONE: FlogicalConstant
    String content = visitable.getValue();
    // String typeName = visitable.getType();
    String kind = visitable.getKind();
    if (StringUtils.isNullOrEmpty(kind) == false) {
      writeToken(content + "_" + kind);
    } else {
      writeToken(content);
    }

    return true;
  }

  /**
   * Decompile "FmemberRef" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   *      variable = <span class="Strong">struct%member</span><br/>
   * </div></code>
   */
  @Override
  public boolean enter(FmemberRef visitable) {
    // DONE: FmemberRef
    writeToken("%");
    writeToken(visitable.getMember());

    return true;
  }

  /**
   * Decompile "FmoduleDefinition" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * <div class="Strong">
   * MODULE mod<br/>
   * </div>
   * <div class="Indent1">(any statement...)<br/></div>
   * <div class="Strong">
   * END MODULE mod<br/>
   * </div>
   * <br/>
   * PROGRAM main<br/>
   * <div class="Indent1">
   *      INTEGER :: int_variable<br/>
   *      (any statement...)<br/>
   * </div>
   * <br/>
   * CONTAINS
   * <div class="Indent1">
   *      SUBROUTINE sub()<br/>
   *      (any statement...)<br/>
   *      END SUBROUTINE sub<br/>
   *      <br/>
   *      FUNCTION func()<br/>
   *      (any statement...)<br/>
   *      END SUBROUTINE sub<br/>
   * </div>
   * <br/>
   * END PROGRAM main<br/>
   * </div></code>
   */
  @Override
  public boolean enter(FmoduleDefinition visitable) {
    // DONE: FmoduleDefinition
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    writeToken(toCaseSensitive("MODULE"));
    writeToken(" ");
    writeToken(visitable.getName());

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  /**
   * Decompile "FmoduleProcedureDecl" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * INTERFACE interface_name<br/>
   * <div class="Indent1">
   *      SUBROUTINE sub1(arg1)<br/>
   *      <div class="Indent1">
   *          INTEGER arg1<br/>
   *      </div>
   *      END SUBROUTINE<br/>
   *      <div class="Strong">
   *      MODULE PROCEDURE module_sub<br/>
   *      </div>
   *      <br/>
   * </div>
   * <div class="Strong">
   * END INTERFACE<br/>
   * </div>
   * </div></code>
   */
  @Override
  public boolean enter(FmoduleProcedureDecl visitable) {
    // DONE: FmoduleProcedureDecl
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    writeToken(toCaseSensitive("MODULE PROCEDURE "));
    int nameCount = 0;
    for (Name nameElem : visitable.getName()) {
      if (nameCount > 0) {
        writeToken(", ");
      }
      writeToken(nameElem.getValue());
      ++nameCount;
    }

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  /**
   * Decompile "FblockDataDefinition" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * <div class="Strong">
   * BLOCK DATA dat<br/>
   * </div>
   * <div class="Indent1">(any declaration...)<br/></div>
   * <div class="Strong">
   * END BLOCK DATA dat<br/>
   * </div>
   * <br/>
   * </div></code>
   */
  @Override
  public boolean enter(FblockDataDefinition visitable) {
    // DONE: FblockDataDefinition
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    writeToken(toCaseSensitive("BLOCK DATA"));
    writeToken(" ");
    writeToken(visitable.getName());

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  @Override
  public void leave(FblockDataDefinition visitable) {
    writeToken(toCaseSensitive("END BLOCK DATA"));
    writeToken(" ");
    writeToken(visitable.getName());
    updateCodeLine(visitable);

    return;
  }

  /**
   * Decompile "FnamelistDecl" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * <div class="Strong">
   * NAMELIST /NAME1/ variable1, variable2, /NAME2/ variable3, variable4<br/>
   * </div>
   * </div></code>
   */
  @Override
  public boolean enter(FnamelistDecl visitable) {

    // DONE: FnamelistDecl
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    writeToken(toCaseSensitive("NAMELIST "));

    List<VarList> list = visitable.getVarList();
    VarList[] array = (VarList[]) list.toArray(new VarList[0]);
    if (_invokeEnterAndWriteDelim(array, ", ") == false) {
      return false;
    }

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  /**
   * Decompile "FnullifyStatement" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   *      <div class="Strong">
   *      NULLIFY (ptr, ptr_array)<br/>
   *      </div>
   * </div></code>
   */
  @Override
  public boolean enter(FnullifyStatement visitable) {

    // DONE: FnullifyStatement
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    writeToken(toCaseSensitive("NULLIFY ("));

    List<Alloc> list = visitable.getAlloc();
    Alloc[] array = (Alloc[]) list.toArray(new Alloc[0]);
    if (_invokeEnterAndWriteDelim(array, ", ") == false) {
      return false;
    }

    writeToken(")");

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  /**
   * Decompile "FopenStatement" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * <div class="Strong">
   * OPEN (UNIT=1, ...)<br/>
   * </div>
   * </div></code>
   */
  @Override
  public boolean enter(FopenStatement visitable) {

    XcodeMLVisitor visiter = _context.getVisitor();
    // DONE: FopenStatement
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    writeToken(toCaseSensitive("OPEN "));
    if (visiter.invokeEnter(visitable.getNamedValueList()) == false) {
      return false;
    }

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  /**
   * Decompile "FpointerAssignStatement" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   *
   * @example <code><div class="Example">
   * PROGRAM main<br/>
   * <div class="Indent1">
   *      INTEGER,POINTER :: int_pointer<br/>
   *      INTEGER :: int_variable<br/>
   *      int_variable = 0<br/>
   *      <div class="Strong">
   *      int_pointer => int_variable
   *      </div>
   *      (any expression statement ...)<br/>
   * </div>
   * END PROGRAM main<br/>
   * </div></code>
   */
  @Override
  public boolean enter(FpointerAssignStatement visitable) {
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    List<IXmlNode> content = visitable.getContent();
    // Left side
    IXmlNode leftExpr = (content != null && content.size() >= 1) ? content.get(0) : null;
    // Right side
    IXmlNode rightExpr = (content != null && content.size() >= 2) ? content.get(1) : null;
    if (writeBinaryExpr(leftExpr, rightExpr, "=>", false) == false) {
      return false;
    }

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  /**
   * Decompile "FpowerExpr" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   *
   * @example <code><div class="Example">
   * variable = <span class="Strong">(&lt;any expression&gt; ** &lt;any expression&gt;)</span><br/>
   * </div></code>
   */
  @Override
  public boolean enter(FpowerExpr visitable) {
    // DONE: FpowerExpr
    List<IXmlNode> list = visitable.getContent();
    IXmlNode leftExpr = list.get(0) != null ? list.get(0) : null;
    IXmlNode rightExpr = list.get(1) != null ? list.get(1) : null;
    if (writeBinaryExpr(leftExpr, rightExpr, "**", true) == false) {
      return false;
    }

    return true;
  }

  /**
   * Decompile "FpragmaStatement" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * <div class="Strong">
   * !$OMP &lt;any text&gt;<br/>
   * </div>
   * </div></code>
   */
  @Override
  public boolean enter(FpragmaStatement visitable) {

    // DONE: FpragmaStatement
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    String content;
    content = visitable.getValue();

    if (content.startsWith("!$") == false) {
      content = "!$" + content;
    }

    //        writeIsolatedLine(content);

    writeToken(content);

    if (!updateCodeLine(visitable)) {
      return false;
    }

    return true;
  }

  /**
   * Decompile "FprintStatement" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * 1000 FORMAT (&lt;any format string&gt;)<br/>
   * <div class="Strong">
   * PRINT *, "any text", variable1<br/>
   * PRINT 1000, variable2<br/>
   * </div>
   * </div></code>
   */
  @Override
  public boolean enter(FprintStatement visitable) {
    XcodeMLVisitor visiter = _context.getVisitor();

    // DONE: FprintStatement
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    writeToken(toCaseSensitive("PRINT "));
    writeToken(visitable.getFormat());

    ValueList valueList = visitable.getValueList();
    List<Value> list = valueList.getValue();
    if (valueList != null && list != null && list.size() > 0) {
      writeToken(", ");
    }

    if (visiter.invokeEnter(visitable.getValueList()) == false) {
      return false;
    }

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  /**
   * Decompile "FreadStatement" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * <div class="Strong">
   * READ (UNIT=1, ...)<br/>
   * </div>
   * </div></code>
   */
  @Override
  public boolean enter(FreadStatement visitable) {
    XcodeMLVisitor visiter = _context.getVisitor();

    // DONE: FreadStatement
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    writeToken(toCaseSensitive("READ "));
    if (visiter.invokeEnter(visitable.getNamedValueList()) == false) {
      return false;
    }

    writeToken(" ");
    if (visiter.invokeEnter(visitable.getValueList()) == false) {
      return false;
    }

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  /**
   * Decompile "FrealConstant" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * PROGRAM main<br/>
   * <div class="Indent1">
   *      REAL real_variable = <span class="Strong">1.0</span><br/>
   * </div>
   * END PROGRAM main<br/>
   * </div></code>
   */
  @Override
  public boolean enter(FrealConstant visitable) {
    // DONE: FrealConstant
    String content = visitable.getValue();
    // String typeName = visitable.getType();
    String kind = visitable.getKind();
    // gfortran rejects kind with 'd' exponent
    if (StringUtils.isNullOrEmpty(kind) == false && content.toLowerCase().indexOf("d") < 0) {
      writeToken(content + "_" + kind);
    } else {
      writeToken(content);
    }

    return true;
  }

  /**
   * Decompile "FreturnStatement" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * SUBROUTINE sub()<br/>
   * <div class="Indent1">
   *      (any statement...)<br/>
   *      <div class="Strong">
   *      RETURN<br/>
   *      </div>
   * </div>
   * END SUBROUTINE sub<br/>
   * </div></code>
   */
  @Override
  public boolean enter(FreturnStatement visitable) {

    // DONE: FreturnStatement
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    writeToken(toCaseSensitive("RETURN"));

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  /**
   * Decompile "FrewindStatement" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * <div class="Strong">
   * REWIND (UNIT=1, ...)<br/>
   * </div>
   * </div></code>
   */
  @Override
  public boolean enter(FrewindStatement visitable) {
    XcodeMLVisitor visiter = _context.getVisitor();

    // DONE: FrewindStatement
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    writeToken(toCaseSensitive("REWIND "));
    if (visiter.invokeEnter(visitable.getNamedValueList()) == false) {
      return false;
    }

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  /**
   * Decompile "FselectCaseStatement" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * <div class="Strong">
   * SELECT CASE (variable)<br/>
   * </div>
   * CASE (1)<br/>
   * <div class="Indent1">
   *      (any statement...)<br/>
   * </div>
   * CASE (2)<br/>
   * <div class="Indent1">
   *      (any statement...)<br/>
   * </div>
   * CASE DEFAULT<br/>
   * <div class="Indent1">
   *      (any statement...)<br/>
   * </div>
   * <div class="Strong">
   * END SELECT<br/>
   * </div>
   * </div></code>
   */
  @Override
  public boolean enter(FselectCaseStatement visitable) {
    XcodeMLVisitor visiter = _context.getVisitor();

    // DONE: FselectCaseStatement
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    String constuctName = visitable.getConstructName();
    if (StringUtils.isNullOrEmpty(constuctName) == false) {
      writeToken(constuctName);
      writeToken(": ");
    }

    writeToken(toCaseSensitive("SELECT CASE ("));
    if (visiter.invokeEnter(visitable.getValue()) == false) {
      return false;
    }

    writeToken(")");

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  @Override
  public void leave(FselectCaseStatement visitable) {
    writeToken(toCaseSensitive("END SELECT"));

    String constuctName = visitable.getConstructName();
    if (StringUtils.isNullOrEmpty(constuctName) == false) {
      writeToken(" ");
      writeToken(constuctName);
    }
    updateCodeLine(visitable);
  }

  /**
   * Decompile "FstopStatement" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * <div class="Strong">
   * STOP "error."<br/>
   * </div>
   * </div></code>
   */
  @Override
  public boolean enter(FstopStatement visitable) {

    // DONE: FstopStatement
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    writeToken(toCaseSensitive("STOP "));

    String code = visitable.getCode();
    String message = visitable.getMessage();
    if (StringUtils.isNullOrEmpty(code) == false) {
      writeToken(code);
    } else if (StringUtils.isNullOrEmpty(message) == false) {
      writeLiteralString(message);
    }

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  /**
   * Decompile "FpauseStatement" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * <div class="Strong">
   * PAUSE 1234<br/>
   * </div>
   * </div></code>
   */
  @Override
  public boolean enter(FpauseStatement visitable) {

    // DONE: FpauseStatement
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    writeToken(toCaseSensitive("PAUSE "));

    String code = visitable.getCode();
    String message = visitable.getMessage();
    if (StringUtils.isNullOrEmpty(code) == false) {
      writeToken(code);
    } else if (StringUtils.isNullOrEmpty(message) == false) {
      writeLiteralString(message);
    } else {
      writeToken("0");
    }

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  /**
   * Decompile "FstructConstructor" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   *      struct = <span class="Strong">TYPE_NAME(1, 2, "abc")</span><br/>
   * </div></code>
   */
  @Override
  public boolean enter(FstructConstructor visitable) {
    // DONE: FstructConstructor
    XcodeMLTypeManager typeManager = _context.getTypeManager();
    IXmlTypeTableChoice typeChoice = typeManager.findType(visitable.getType());
    FstructType structTypeElem = (FstructType) typeChoice;
    String aliasStructTypeName = typeManager.getAliasTypeName(structTypeElem.getType());

    writeToken(aliasStructTypeName);
    writeToken("(");

    List<IXmlNode> list = visitable.getDefModelExpr();
    IXmlNode[] array = (IXmlNode[]) list.toArray(new IXmlNode[0]);
    if (_invokeEnterAndWriteDelim(array, ", ") == false) {
      return false;
    }

    writeToken(")");

    return true;
  }

  /**
   * Decompile "FstructDecl" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * PROGRAM main<br/>
   * <div class="Indent1">
   *      <div class="Strong">
   *      TYPE derived_type</br>
   *      </div>
   *      <div class="Indent1">
   *      INTEGER :: int_variable
   *      </div>
   *      <div class="Strong">
   *      END TYPE derived_type</br>
   *      </div>
   *      TYPE(derived_type) derived_variable<br/>
   *      (any statement...)<br/>
   * </div>
   * END PROGRAM main<br/>
   * </div></code>
   */
  @Override
  public boolean enter(FstructDecl visitable) {
    // DONE: FstructDecl
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    Name nameElem = visitable.getName();
    String typeId = nameElem.getType();
    XcodeMLTypeManager typeManager = _context.getTypeManager();
    IXmlTypeTableChoice typeChoice = typeManager.findType(typeId);

    FstructType structTypeElem = (FstructType) typeChoice;
    String structTypeName = nameElem.getValue();

    writeToken(toCaseSensitive("TYPE"));

    if (_isUnderModuleDef()) {
      if (XmlNodeUtil.isBoolean(structTypeElem.isIsPrivate())) {
        writeToken(toCaseSensitive(", PRIVATE"));
      } else if (XmlNodeUtil.isBoolean(structTypeElem.isIsPublic())) {
        writeToken(toCaseSensitive(", PUBLIC"));
      }
    }

    writeToken(" :: ");
    writeToken(structTypeName);

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  /**
   * Decompile "FstructDecl" element in XcodeML/F.
   *
   * @param visitable FstructDecl
   * @return true=success
   */
  public boolean enterStructTypeElem(FstructDecl visitable) {
    Name nameElem = visitable.getName();
    String typeId = nameElem.getType();
    XcodeMLTypeManager typeManager = _context.getTypeManager();
    IXmlTypeTableChoice typeChoice = typeManager.findType(typeId);

    FstructType structTypeElem = (FstructType) typeChoice;

    if (_isUnderModuleDef()) {
      if (XmlNodeUtil.isBoolean(structTypeElem.isIsInternalPrivate())) {
        writeToken(toCaseSensitive("PRIVATE"));

        if (!updateCodeLine(visitable)) {
          return false;
        }
      }
    }

    if (XmlNodeUtil.isBoolean(structTypeElem.isIsSequence())) {
      writeToken(toCaseSensitive("SEQUENCE"));

      if (!updateCodeLine(visitable)) {
        return false;
      }
    }

    return true;
  }

  @Override
  public void leave(FstructDecl visitable) {
    Name nameElem = visitable.getName();
    String structTypeName = nameElem.getValue();

    writeToken(toCaseSensitive("END TYPE"));
    writeToken(" ");
    writeToken(structTypeName);
    updateCodeLine(visitable);
  }

  /**
   * Decompile "functionCall" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   *      variable = <span class="Strong">function(arg1, arg2)</span><br/>
   *      <span class="Strong">call subroutine(arg1, arg2)</span><br/>
   * </div></code>
   */
  @Override
  public boolean enter(FunctionCall visitable) {
    XcodeMLVisitor visiter = _context.getVisitor();

    // DONE: FunctionCall
    Name functionNameElem = visitable.getName();
    String functionName = functionNameElem.getValue();

    // Note:
    // If it is built-in function, it is not on the type table.
    if (XmlNodeUtil.isBoolean(visitable.isIsIntrinsic()) == false) {
      XcodeMLTypeManager typeManager = _context.getTypeManager();
      IXmlTypeTableChoice typeChoice = typeManager.findType(functionNameElem);
      if (typeChoice == null) {
        _context.setLastErrorMessage(
            XmlNodeUtil.formatError(
                visitable, EnumError.XCODEML_TYPE_NOT_FOUND, functionNameElem.getType()));
        return false;
      } else if ((typeChoice instanceof FfunctionType) == false) {
        _context.setLastErrorMessage(
            XmlNodeUtil.formatError(
                visitable,
                EnumError.XCODEML_TYPE_MISMATCH,
                "function definition",
                typeChoice.getClass().getSimpleName(),
                "FfunctionType"));
        return false;
      }

      FfunctionType functionTypeElem = (FfunctionType) typeChoice;

      if (XmlNodeUtil.isBoolean(functionTypeElem.isIsProgram())) {
        // =======
        // PROGRAM
        // =======
        _context.setLastErrorMessage(
            XmlNodeUtil.formatError(
                visitable,
                EnumError.XCODEML_TYPE_MISMATCH,
                "function definition",
                "PROGRAM",
                "FUNCTION or SUBROUTINE"));
        return false;
      }
    }

    String returnTypeName = visitable.getType();
    EnumType typeId = EnumType.getTypeIdFromXcodemlTypeName(returnTypeName);
    if (typeId == EnumType.VOID) {
      // ==========
      // SUBROUTINE
      // ==========
      writeToken(toCaseSensitive("CALL "));
    } else {
      // ========
      // FUNCTION
      // ========
    }

    writeToken(functionName);
    writeToken("(");

    if (visiter.invokeEnter(visitable.getArguments()) == false) {
      return false;
    }

    writeToken(")");

    return true;
  }

  /**
   * Decompile "FuseDecl" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * MODULE mod<br/>
   * <div class="Indent1">
   *      TYPE mod_derived_type<br/>
   *      END TYPE mod_derived_type<br/>
   *      (any statement...)<br/>
   * </div>
   * END MODULE mod<br/>
   * <br/>
   * PROGRAM main<br/>
   * <div class="Indent1">
   *      <div class="Strong">
   *      USE mod, derived_type => mod_derived_type<br/>
   *      </div>
   *      TYPE(derived_type) derived_variable<br/>
   *      (any statement...)<br/>
   * </div>
   * END PROGRAM main<br/>
   * </div></code>
   */
  @Override
  public boolean enter(FuseDecl visitable) {
    // XcodeMLVisitor visiter = _context.getVisitor();

    // DONE: FuseDecl
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    writeToken(toCaseSensitive("USE "));
    writeToken(visitable.getName());

    for (Rename renameElem : visitable.getRename()) {
      String localName = renameElem.getLocalName();
      String useName = renameElem.getUseName();
      writeToken(", ");
      if (StringUtils.isNullOrEmpty(localName) == false) {
        writeToken(localName);
        writeToken(" => ");
      }
      if (StringUtils.isNullOrEmpty(useName) == false) {
        writeToken(useName);
      }
    }

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  /**
   * Decompile "FuseOnlyDecl" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * MODULE mod<br/>
   * <div class="Indent1">
   *      TYPE mod_derived_type<br/>
   *      END TYPE mod_derived_type<br/>
   *      (any statement...)<br/>
   * </div>
   * END MODULE mod<br/>
   * <br/>
   * PROGRAM main<br/>
   * <div class="Indent1">
   *      <div class="Strong">
   *      USE mod, ONLY: derived_type => mod_derived_type<br/>
   *      </div>
   *      TYPE(derived_type) derived_variable<br/>
   *      (any statement...)<br/>
   * </div>
   * END PROGRAM main<br/>
   * </div></code>
   */
  @Override
  public boolean enter(FuseOnlyDecl visitable) {

    // DONE: FuseOnlyDecl
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    writeToken(toCaseSensitive("USE "));
    writeToken(visitable.getName());
    writeToken(toCaseSensitive(", ONLY: "));

    int renamableCount = 0;
    for (Renamable renamableElem : visitable.getRenamable()) {
      if (renamableCount > 0) {
        writeToken(", ");
      }
      String localName = renamableElem.getLocalName();
      String useName = renamableElem.getUseName();
      if (StringUtils.isNullOrEmpty(localName) == false) {
        writeToken(localName);
        writeToken(" => ");
      }
      if (StringUtils.isNullOrEmpty(useName) == false) {
        writeToken(useName);
      }
      ++renamableCount;
    }

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  /**
   * Decompile "FwhereStatement" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * <div class="Strong">
   * WHERE (array > 0)<br/>
   * <div class="Indent1">
   *     array = 0<br/>
   * </div>
   * ELSEWHERE<br/>
   * <div class="Indent1">
   *     array = 1<br/>
   * </div>
   * END WHERE<br/>
   * </div>
   * </div></code>
   */
  @Override
  public boolean enter(FwhereStatement visitable) {

    XcodeMLVisitor visiter = _context.getVisitor();
    // DONE: FwhereStatement
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    writeToken(toCaseSensitive("WHERE "));

    if (visiter.invokeEnter(visitable.getCondition()) == false) {
      return false;
    }

    if (!updateCodeLine(visitable)) {
      return false;
    }

    /*************
     *
     * if (visiter.invokeEnter(visitable.getThen()) == false) {
     * return false;
     * }
     *
     * Else elseElem = visitable.getElse();
     * if (elseElem != null) {
     * writeToken("ELSEWHERE");
     *
     * if (!updateCodeLine(visitable)) {
     * return false;
     * }
     * if (visiter.invokeEnter(visitable.getElse()) == false) {
     * return false;
     * }
     * }
     ****/

    return true;
  }

  @Override
  public void leave(FwhereStatement visitable) {
    writeToken(toCaseSensitive("END WHERE"));
    updateCodeLine(visitable);
  }

  /**
   * Decompile "FwriteStatement" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * <div class="Strong">
   * WRITE (UNIT=1, ...)<br/>
   * </div>
   * </div></code>
   */
  @Override
  public boolean enter(FwriteStatement visitable) {
    XcodeMLVisitor visiter = _context.getVisitor();

    // DONE: FwriteStatement
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    writeToken(toCaseSensitive("WRITE "));
    if (visiter.invokeEnter(visitable.getNamedValueList()) == false) {
      return false;
    }

    writeToken(" ");
    if (visiter.invokeEnter(visitable.getValueList()) == false) {
      return false;
    }

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  /**
   * Decompile "gotoStatement" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * <div class="Strong">
   * GOTO 1000<br/>
   * </div>
   * 1000 CONTINUE<br/>
   * <br/>
   * <div class="Strong">
   * GOTO (2000, 2001, 2002), variable<br/>
   * </div>
   * 2000 (any statement...)<br/>
   * 2001 (any statement...)<br/>
   * 2002 (any statement...)<br/>
   * </div></code>
   */
  @Override
  public boolean enter(GotoStatement visitable) {
    XcodeMLVisitor visiter = _context.getVisitor();

    // DONE: GotoStatement
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    writeToken(toCaseSensitive("GOTO "));
    String labelName = visitable.getLabelName();
    if (StringUtils.isNullOrEmpty(labelName) == false) {
      // For line numbers, use numeric characters
      if (StringUtils.isNumeric(labelName)) {
        labelName = Integer.valueOf(labelName).toString();
      }
      writeToken(labelName);
    } else {
      GotoStatementSequence seq =
          new GotoStatementSequence(visitable.getParams(), visitable.getValue());
      if (visiter.invokeEnter(seq) == false) {
        return false;
      }
    }

    if (!updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  /**
   * Decompile child group of "gotoStatement" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * GOTO 1000<br/>
   * 1000 CONTINUE<br/>
   * <br/>
   * GOTO <span class="Strong">(2000, 2001, 2002), variable</span><br/>
   * 2000 (any statement...)<br/>
   * 2001 (any statement...)<br/>
   * 2002 (any statement...)<br/>
   * </div></code>
   */
  @Override
  public boolean enter(GotoStatementSequence visitable) {
    XcodeMLVisitor visiter = _context.getVisitor();

    // DONE: GotoStatementSequence
    writeToken("(");

    if (visiter.invokeEnter(visitable.getParams()) == false) {
      return false;
    }

    writeToken("), ");

    if (visiter.invokeEnter(visitable.getValue()) == false) {
      return false;
    }

    return true;
  }

  /**
   * Decompile "indexRange" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   *      array1 = int_array_variable(<span class="Strong">10</span>,
   *      <span class="Strong">1:10</span>,
   *      <span class="Strong">1:</span>,
   *      <span class="Strong">:</span>)<br/>
   *      array2 = int_array_variable(<span class="Strong">:10:2</span>,
   *      <span class="Strong">1:10:2</span>,
   *      <span class="Strong">1::2</span>,
   *      <span class="Strong">::2</span>)<br/>
   *      array3 = (/ I, I = <span class="Strong">1, 10, 2</span> /)<br/>
   * </div></code>
   */
  @Override
  public boolean enter(IndexRange visitable) {
    XcodeMLVisitor visiter = _context.getVisitor();

    // DONE: IndexRange
    String delim;
    if (_context.isInvokeNodeOf(FdoLoop.class, 1)) {
      // Parent node is FdoLoop
      delim = ", ";
    } else if (_context.isInvokeNodeOf(FdoStatementSequence.class, 1)) {
      // Parent node is FdoStatementSequence
      delim = ", ";
    } else {
      delim = ":";
    }

    if (XmlNodeUtil.isBoolean(visitable.isIsAssumedShape())
        && XmlNodeUtil.isBoolean(visitable.isIsAssumedSize())) {
      // semantics error.
      _context.debugPrintLine(
          "'is_assumed_shape' and 'is_assumed_size' are logically exclusize attributes.");
      _context.setLastErrorMessage(
          XmlNodeUtil.formatError(
              visitable, EnumError.XCODEML_SEMANTICS, XmlNodeUtil.getElementName(visitable)));
      return false;
    }

    if (XmlNodeUtil.isBoolean(visitable.isIsAssumedShape())) {
      LowerBound lowerBound = visitable.getLowerBound();
      if (visiter.invokeEnter(lowerBound) == false) {
        return false;
      }

      writeToken(":");
      return true;
    }

    if (XmlNodeUtil.isBoolean(visitable.isIsAssumedSize())) {
      LowerBound lowerBound = visitable.getLowerBound();
      if (lowerBound != null) {
        if (visiter.invokeEnter(lowerBound) == false) {
          return false;
        }
        writeToken(":");
      }
      writeToken("*");
      return true;
    }

    if (visiter.invokeEnter(visitable.getLowerBound()) == false) {
      return false;
    }

    writeToken(delim);

    if (visiter.invokeEnter(visitable.getUpperBound()) == false) {
      return false;
    }

    Step step = visitable.getStep();
    if (step != null) {
      writeToken(delim);
      if (visiter.invokeEnter(step) == false) {
        return false;
      }
    }

    return true;
  }

  /**
   * Decompile "len" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * PROGRAM main<br/>
   * <div class="Indent1">
   *      CHARACTER(LEN=<span class="Strong">10</span>, KIND=1) :: string_variable
   * </div>
   * END PROGRAM main<br/>
   * </div></code>
   */
  @Override
  public boolean enter(Len visitable) {
    XcodeMLVisitor visiter = _context.getVisitor();

    // DONE: Len
    if (XmlNodeUtil.getXmlNodeChoice(visitable) == null) {

      writeToken("*");
    } else if (visiter.invokeEnter(XmlNodeUtil.getXmlNodeChoice(visitable)) == false) {
      return false;
    }

    return true;
  }

  /**
   * Decompile "logAndExpr" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   *
   * @example <code><div class="Example">
   * variable = <span class="Strong">(&lt;any expression&gt; .AND. &lt;any expression&gt;)</span><br/>
   * </div></code>
   */
  @Override
  public boolean enter(LogAndExpr visitable) {
    // DONE: LogAndExpr
    List<IXmlNode> list = visitable.getContent();
    IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
    IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
    if (writeBinaryExpr(leftExpr, rightExpr, toCaseSensitive(".AND."), true) == false) {
      return false;
    }

    return true;
  }

  /**
   * Decompile "logEQExpr" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   *
   * @example <code><div class="Example">
   * variable = <span class="Strong">(&lt;any expression&gt; .EQ. &lt;any expression&gt;)</span><br/>
   * </div></code>
   */
  @Override
  public boolean enter(LogEQExpr visitable) {
    // DONE: LogEQExpr
    List<IXmlNode> list = visitable.getContent();
    IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
    IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
    if (writeBinaryExpr(leftExpr, rightExpr, "==", true) == false) {
      return false;
    }

    return true;
  }

  /**
   * Decompile "logEQVExpr" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   *
   * @example <code><div class="Example">
   * variable = <span class="Strong">(&lt;any expression&gt; .EQV. &lt;any expression&gt;)</span><br/>
   * </div></code>
   */
  @Override
  public boolean enter(LogEQVExpr visitable) {
    // DONE: LogEQVExpr
    List<IXmlNode> list = visitable.getContent();
    IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
    IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
    if (writeBinaryExpr(leftExpr, rightExpr, toCaseSensitive(".EQV."), true) == false) {
      return false;
    }

    return true;
  }

  /**
   * Decompile "logGEExpr" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   *
   * @example <code><div class="Example">
   * variable = <span class="Strong">(&lt;any expression&gt; &gt;= &lt;any expression&gt;)</span><br/>
   * </div></code>
   */
  @Override
  public boolean enter(LogGEExpr visitable) {
    // DONE: LogGEExpr
    List<IXmlNode> list = visitable.getContent();
    IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
    IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
    if (writeBinaryExpr(leftExpr, rightExpr, ">=", true) == false) {
      return false;
    }

    return true;
  }

  /**
   * Decompile "logGTExpr" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   *
   * @example <code><div class="Example">
   * variable = <span class="Strong">(&lt;any expression&gt; &gt; &lt;any expression&gt;)</span><br/>
   * </div></code>
   */
  @Override
  public boolean enter(LogGTExpr visitable) {
    // DONE: LogGTExpr
    List<IXmlNode> list = visitable.getContent();
    IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
    IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
    if (writeBinaryExpr(leftExpr, rightExpr, ">", true) == false) {
      return false;
    }

    return true;
  }

  /**
   * Decompile "logLEExpr" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   *
   * @example <code><div class="Example">
   * variable = <span class="Strong">(&lt;any expression&gt; &lt;= &lt;any expression&gt;)</span><br/>
   * </div></code>
   */
  @Override
  public boolean enter(LogLEExpr visitable) {
    // DONE: LogLEExpr
    List<IXmlNode> list = visitable.getContent();
    IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
    IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
    if (writeBinaryExpr(leftExpr, rightExpr, "<=", true) == false) {
      return false;
    }

    return true;
  }

  /**
   * Decompile "logLTExpr" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   *
   * @example <code><div class="Example">
   * variable = <span class="Strong">(&lt;any expression&gt; &lt; &lt;any expression&gt;)</span><br/>
   * </div></code>
   */
  @Override
  public boolean enter(LogLTExpr visitable) {
    // DONE: LogLTExpr
    List<IXmlNode> list = visitable.getContent();
    IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
    IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
    if (writeBinaryExpr(leftExpr, rightExpr, "<", true) == false) {
      return false;
    }

    return true;
  }

  /**
   * Decompile "logNEQExpr" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   *
   * @example <code><div class="Example">
   * variable = <span class="Strong">(&lt;any expression&gt; .NEQ. &lt;any expression&gt;)</span><br/>
   * </div></code>
   */
  @Override
  public boolean enter(LogNEQExpr visitable) {
    // DONE: LogNEQExpr
    List<IXmlNode> list = visitable.getContent();
    IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
    IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
    if (writeBinaryExpr(leftExpr, rightExpr, "/=", true) == false) {
      return false;
    }

    return true;
  }

  /**
   * Decompile "logNEQVExpr" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   *
   * @example <code><div class="Example">
   * variable = <span class="Strong">(&lt;any expression&gt; .NEQV. &lt;any expression&gt;)</span><br/>
   * </div></code>
   */
  @Override
  public boolean enter(LogNEQVExpr visitable) {
    // DONE: LogNEQVExpr
    List<IXmlNode> list = visitable.getContent();
    IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
    IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
    if (writeBinaryExpr(leftExpr, rightExpr, toCaseSensitive(".NEQV."), true) == false) {
      return false;
    }

    return true;
  }

  /**
   * Decompile "logNotExpr" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   *
   * @example <code><div class="Example">
   * variable = <span class="Strong">(.NOT. &lt;any expression&gt;)</span><br/>
   * </div></code>
   */
  @Override
  public boolean enter(LogNotExpr visitable) {
    // DONE: LogNotExpr
    if (writeUnaryExpr(XmlNodeUtil.getXmlNodeChoice(visitable), toCaseSensitive(".NOT."), true)
        == false) {
      return false;
    }

    return true;
  }

  /**
   * Decompile "logOrExpr" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   *
   * @example <code><div class="Example">
   * variable = <span class="Strong">(&lt;any expression&gt; .OR. &lt;any expression&gt;)</span><br/>
   * </div></code>
   */
  @Override
  public boolean enter(LogOrExpr visitable) {
    // DONE: LogOrExpr
    List<IXmlNode> list = visitable.getContent();
    IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
    IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
    if (writeBinaryExpr(leftExpr, rightExpr, toCaseSensitive(".OR."), true) == false) {
      return false;
    }

    return true;
  }

  /**
   * Decompile "minusExpr" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   *
   * @example <code><div class="Example">
   * variable = <span class="Strong">(&lt;any expression&gt; - &lt;any expression&gt;)</span><br/>
   * </div></code>
   */
  @Override
  public boolean enter(MinusExpr visitable) {
    // DONE: MinusExpr
    List<IXmlNode> list = visitable.getContent();
    IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
    IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
    if (writeBinaryExpr(leftExpr, rightExpr, "-", true) == false) {
      return false;
    }

    return true;
  }

  /**
   * Decompile "mulExpr" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   *
   * @example <code><div class="Example">
   * variable = <span class="Strong">(&lt;any expression&gt; * &lt;any expression&gt;)</span><br/>
   * </div></code>
   */
  @Override
  public boolean enter(MulExpr visitable) {
    // DONE: MulExpr
    List<IXmlNode> list = visitable.getContent();
    IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
    IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;

    if (writeBinaryExpr(leftExpr, rightExpr, "*", true) == false) {
      return false;
    }

    return true;
  }

  /**
   * Decompile "namedValue" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * OPEN (<span class="Strong">UNIT=1</span>, <span class="Strong">...</span>)<br/>
   * </div></code>
   */
  @Override
  public boolean enter(NamedValue visitable) {
    XcodeMLVisitor visiter = _context.getVisitor();

    IXmlNode defModelExpr = XmlNodeUtil.getXmlNodeChoice(visitable);
    // DONE: NamedValue

    writeToken(visitable.getName());
    writeToken("=");

    if (defModelExpr == null) {
      writeToken(visitable.getValue());
    } else {
      if (visiter.invokeEnter(defModelExpr) == false) {
        return false;
      }
    }

    return true;
  }

  /**
   * Decompile "namedValueList" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * OPEN <span class="Strong">(UNIT=1, ...)</span><br/>
   * </div></code>
   */
  @Override
  public boolean enter(NamedValueList visitable) {
    // DONE: NamedValueList

    writeToken("(");

    List<NamedValue> list = visitable.getNamedValue();
    NamedValue[] array = (NamedValue[]) list.toArray(new NamedValue[0]);
    if (_invokeEnterAndWriteDelim(array, ", ") == false) {
      return false;
    }

    writeToken(")");

    return true;
  }

  /**
   * Decompile "params" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * PROGRAM main<br/>
   * <div class="Indent1">
   *      INTEGER :: int_variable<br/>
   *      (any statement...)<br/>
   * </div>
   * <br/>
   * CONTAINS
   * <div class="Indent1">
   *      SUBROUTINE sub()<br/>
   *      (any statement...)<br/>
   *      END SUBROUTINE sub<br/>
   *      <br/>
   *      FUNCTION func(<span class="Strong">a, b, c</span>)<br/>
   *      (any statement...)<br/>
   *      END SUBROUTINE sub<br/>
   * </div>
   * <br/>
   * END PROGRAM main<br/>
   * </div></code>
   */
  @Override
  public boolean enter(Params visitable) {
    // DONE: Params
    int paramCount = 0;
    for (Name nameElem : visitable.getName()) {
      if (paramCount > 0) {
        writeToken(", ");
      }
      writeToken(nameElem.getValue());
      ++paramCount;
    }

    return true;
  }

  /**
   * Decompile "plusExpr" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   *
   * @example <code><div class="Example">
   * variable = <span class="Strong">(&lt;any expression&gt; + &lt;any expression&gt;)</span><br/>
   * </div></code>
   */
  @Override
  public boolean enter(PlusExpr visitable) {
    // DONE: PlusExpr
    List<IXmlNode> list = visitable.getContent();
    IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
    IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
    if (writeBinaryExpr(leftExpr, rightExpr, "+", true) == false) {
      return false;
    }

    return true;
  }

  /**
   * Decompile "statementLabel" element in XcodeML/F.
   *
   * @param visitable StatementLabel
   * @param nextNode IXmlNode
   * @return true=success
   * @example <code><div class="Example">
   * GOTO 1000
   * <span class="Strong">1000 </span>CONTINUE<br/>
   * (any statement...)<br/>
   * </div></code>
   */
  public boolean enterStatementLabel(StatementLabel visitable, IXmlNode nextNode) {

    // DONE: StatementLabel
    writeLineDirective(visitable.getLineno(), visitable.getFile());
    String label = visitable.getLabelName();
    // For line numbers, use numeric characters
    if (StringUtils.isNumeric(label)) {
      label = Integer.valueOf(label).toString();
    }
    writeToken(label);
    if ((nextNode != null) && (nextNode instanceof StatementLabel)) {
      // Note:
      // If next statement is statementLabel,
      // add continue statement.
      //
      // Cauntion!:
      // This change is due to the change of a declaraton.
      // A statement label of the declaration will be move to
      // the body block, thus XcodeML frontend generates
      // the statement label without a statement.
      // (and generate a declaration without a label).
      // To avoid compile errors occurred by this change,
      // the backend add continue statement to the label.
      writeToken(toCaseSensitive(" CONTINUE"));
      if (!updateCodeLine(visitable)) {
        return false;
      }
    } else {
      // Note:
      // Handling next statement as continuous line.
      writeToken(" ");
    }

    return true;
  }

  /**
   * Decompile "symbols" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   */
  @Override
  public boolean enter(Symbols visitable) {
    // DONE: Symbols
    if (_context.isInvokeAncestorNodeOf(FstructDecl.class) == false) {

    } else {
      _context.debugPrintLine("Write symbol.");
      for (Id idElem : visitable.getId()) {
        String typeName;

        typeName = idElem.getType();

        Name nameElem = idElem.getName();
        if (typeName == null) {
          typeName = nameElem.getType();

          if (typeName == null) {
            _context.setLastErrorMessage(
                XmlNodeUtil.formatError(
                    idElem,
                    EnumError.XCODEML_NEED_ATTR,
                    "type",
                    XmlNodeUtil.getElementName(visitable)));
            return false;
          }
        }

        String symbolName = nameElem.getValue();

        XmlSymbol symbol = _makeSymbol(symbolName, typeName);
        if (symbol == null) {
          _context.setLastErrorMessage(
              XmlNodeUtil.formatError(idElem, EnumError.XCODEML_TYPE_NOT_FOUND, typeName));
          return false;
        }
        if (writeSymbolDecl(symbol, visitable) == false) {
          return false;
        }

        if (!_context.getCodeBuilder().updateCodeLine(visitable)) {
          return false;
        }
      }
    }

    return true;
  }

  /**
   * Decompile "unaryMinusExpr" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   *
   * @example <code><div class="Example">
   * variable = <span class="Strong">(-&lt;any expression&gt;)</span><br/>
   * </div></code>
   */
  @Override
  public boolean enter(UnaryMinusExpr visitable) {
    // DONE: UnaryMinusExpr

    boolean grouping = true;
    IXmlNode child = XmlNodeUtil.getXmlNodeChoice(visitable);

    // if (_isConstantExpr(visitable.rGetParentRNode()) &&
    // _isConstantExpr(child, visitable)) {
    if (_isConstantExpr(child, visitable)) {
      grouping = false;
    }

    if (writeUnaryExpr(child, "-", grouping) == false) {
      return false;
    }

    return true;
  }

  /**
   * Decompile "userBinaryExpr" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   *
   * @example <code><div class="Example">
   * variable = <span class="Strong">(&lt;any expression&gt; &lt;user defined expr&gt; &lt;any expression&gt;)</span><br/>
   * </div></code>
   */
  @Override
  public boolean enter(UserBinaryExpr visitable) {
    // DONE: UserBinaryExpr
    String name = visitable.getName();
    List<IXmlNode> list = visitable.getContent();
    IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
    IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
    if (writeBinaryExpr(leftExpr, rightExpr, name, true) == false) {
      return false;
    }

    return true;
  }

  /**
   * Decompile "userUnaryExpr" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   *
   * @example <code><div class="Example">
   * variable = <span class="Strong">(&lt;user defined expr&gt;&lt;any expression&gt;)</span><br/>
   * </div></code>
   */
  @Override
  public boolean enter(UserUnaryExpr visitable) {
    // DONE: UserUnaryExpr
    String name = visitable.getName();
    if (writeUnaryExpr(XmlNodeUtil.getXmlNodeChoice(visitable), name, true) == false) {
      return false;
    }

    return true;
  }

  /**
   * Decompile "value" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   */
  @Override
  public boolean enter(Value visitable) {
    XcodeMLVisitor visiter = _context.getVisitor();

    // DONE: Value
    if (visitable.getRepeatCount() != null) {

      if (visiter.invokeEnter(visitable.getRepeatCount()) == false) return false;
      writeToken("*");
    }

    return true;
  }

  /**
   * Decompile "valueList" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   */
  @Override
  public boolean enter(ValueList visitable) {
    // DONE: ValueList

    boolean token = false;
    if (_context.isInvokeNodeOf(FdataDeclSequence.class, 1)) {
      token = true;
    } else if (_context.isInvokeNodeOf(FdataDecl.class, 1)) {
      token = true;
    }

    if (token) {
      writeToken(" /");
    }

    List<Value> list = visitable.getValue();
    Value[] array = (Value[]) list.toArray(new Value[0]);
    if (_invokeEnterAndWriteDelim(array, ", ") == false) {
      return false;
    }

    if (token) {
      writeToken("/");
    }

    return true;
  }

  /**
   * Decompile "Ffunction" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   */
  @Override
  public boolean enter(Ffunction visitable) {
    // DONE: Ffunction
    writeToken(visitable.getValue());

    return true;
  }

  /**
   * Decompile "Var" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   */
  @Override
  public boolean enter(Var visitable) {
    // DONE: Var

    writeToken(visitable.getValue());

    return true;
  }

  /**
   * Decompile "varDecl" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   *
   * @example <code><div class="Example">
   * PROGRAM main<br/>
   * <div class="Indent1">
   *      <div class="Strong">
   *      INTEGER :: int_variable<br/>
   *      TYPE(USER_TYPE) :: derived_variable<br/>
   *      (any variant declaration...)<br/>
   *      </div>
   *      int_variable = 0<br/>
   *      (any statement...)<br/>
   * </div>
   * END PROGRAM main<br/>
   * </div></code>
   */
  @Override
  public boolean enter(VarDecl visitable) {
    XcodeMLVisitor visiter = _context.getVisitor();

    // DONE: VarDecl
    writeLineDirective(visitable.getLineno(), visitable.getFile());

    Name nameElem = visitable.getName();
    XmlSymbol symbol = _makeSymbol(nameElem);
    if (symbol == null) {
      _context.setLastErrorMessage(
          XmlNodeUtil.formatError(nameElem, EnumError.XCODEML_NAME_NOT_FOUND, nameElem.getValue()));
      return false;
    }
    if (writeSymbolDecl(symbol, visitable) == false) {
      return false;
    }

    Value valueElem = visitable.getValue();
    if (valueElem != null) {
      writeToken(" = ");
      if (visiter.invokeEnter(valueElem) == false) {
        return false;
      }
    }

    if (!_context.getCodeBuilder().updateCodeLine(visitable)) {
      return false;
    }
    return true;
  }

  /**
   * Decompile "varList" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   */
  @Override
  public boolean enter(VarList visitable) {
    // DONE: VarList

    String name = visitable.getName();

    if (_context.isInvokeNodeOf(FcommonDecl.class, 1)) {
      // Parent node is FcommonDecl
      writeToken("/");
      if (StringUtils.isNullOrEmpty(name) == false) {
        writeToken(name);
      }
      writeToken("/ ");
    } else if (_context.isInvokeNodeOf(FnamelistDecl.class, 1)) {
      // Parent node is FnamelistDecl
      if (StringUtils.isNullOrEmpty(name)) {
        _context.setLastErrorMessage(
            XmlNodeUtil.formatError(
                visitable,
                EnumError.XCODEML_NEED_ATTR,
                "name",
                XmlNodeUtil.getElementName(visitable)));
        return false;
      }

      writeToken("/");
      writeToken(name);
      writeToken("/ ");
    } else if (_context.isInvokeNodeOf(FdataDeclSequence.class, 1)) {
      // Parent node is FdataDeclSequence
    } else if (_context.isInvokeNodeOf(FequivalenceDeclSequence.class, 1)) {
      // Parent node is FequivalenceDeclSequence
    } else if (_context.isInvokeNodeOf(FdataDecl.class, 1)) {
      // Parent node is FdataDecl
    } else if (_context.isInvokeNodeOf(FequivalenceDecl.class, 1)) {
      // Parent node is FequivalenceDeclSequence
    } else {
      assert (false);
    }

    List<IXmlNode> list = visitable.getVarRefOrFdoLoop();
    IXmlNode[] array = (IXmlNode[]) list.toArray(new IXmlNode[0]);
    if (_invokeEnterAndWriteDelim(array, ", ") == false) {
      return false;
    }

    return true;
  }

  @Override
  public void leave(FfunctionDefinition visitable) {

    XcodeMLTypeManager typeManager = _context.getTypeManager();

    Name functionNameElem = visitable.getName();

    IXmlTypeTableChoice typeChoice = typeManager.findType(functionNameElem);
    FfunctionType functionTypeElem = (FfunctionType) typeChoice;
    String returnTypeName = functionTypeElem.getReturnType();
    EnumType typeId = EnumType.getTypeIdFromXcodemlTypeName(returnTypeName);

    // ========
    // Prologue
    // ========
    typeManager.leaveScope();

    if (XmlNodeUtil.isBoolean(functionTypeElem.isIsProgram())) {
      // =======
      // PROGRAM
      // =======
      writeToken(toCaseSensitive("END PROGRAM"));
      writeToken(" ");
      writeToken(functionNameElem.getValue());
    } else if (typeId == EnumType.VOID) {
      // ==========
      // SUBROUTINE
      // ==========
      writeToken(toCaseSensitive("END SUBROUTINE"));
      writeToken(" ");
      writeToken(functionNameElem.getValue());
    } else {
      writeToken(toCaseSensitive("END FUNCTION"));
      writeToken(" ");
      writeToken(functionNameElem.getValue());
    }
    updateCodeLine(visitable);
  }

  @Override
  public void leave(FmoduleDefinition visitable) {

    XcodeMLTypeManager typeManager = _context.getTypeManager();

    // ========
    // Prologue
    // ========
    typeManager.leaveScope();

    writeToken(toCaseSensitive("END MODULE"));
    writeToken(" ");
    writeToken(visitable.getName());
    updateCodeLine(visitable);
  }

  /**
   * Get the substring position from the code being assembled.
   *
   * @return Substring position
   */
  public int getCurrentColumn() {
    return m_lineBuf.length();
  }

  /**
   * Get the substring from the code being assembled. <br>
   * Get from the string started by stealStart to the current string.
   *
   * @param start_idx Start position
   * @return substring
   */
  public String stealLineBuf(int start_idx) {
    if (m_lineBuf.length() <= 0) return null;
    if (m_lineBuf.length() < start_idx) return null;
    return m_lineBuf.substring(start_idx);
  }

  /**
   * Get the argument list as a string.
   *
   * @param args argument element
   * @return argument list
   */
  public String[] getArgumentList(Arguments args) {
    List<IXmlNode> list = args.getFintConstantOrFrealConstantOrFcomplexConstant();
    if (list == null) return null;

    ArrayList<String> arg_list = new ArrayList<String>();
    XcodeMLVisitor visiter = _context.getVisitor();
    for (IXmlNode node : list) {
      // Dummy execution ON
      m_dummyBuf = new StringBuilder();
      if (visiter.invokeEnter(node) == false) {
        // Dummy execution OFF
        m_dummyBuf = null;
        return null;
      }
      if (m_dummyBuf == null) {
        assert (false);
      }
      // Add argument
      arg_list.add(m_dummyBuf.toString());

      // Dummy execution ON
      m_dummyBuf = new StringBuilder();
    }

    // Dummy execution OFF
    m_dummyBuf = null;

    return (String[]) arg_list.toArray(new String[arg_list.size()]);
  }

  /**
   * Assemble the cord
   *
   * @param node element
   * @return code string
   */
  public String getCodeString(IXmlNode node) {
    XcodeMLVisitor visiter = _context.getVisitor();
    // Dummy execution ON
    m_dummyBuf = new StringBuilder();
    if (visiter.invokeEnter(node) == false) {
      // Dummy execution OFF
      m_dummyBuf = null;
      return null;
    }
    String line = m_dummyBuf.toString();

    // Dummy execution OFF
    m_dummyBuf = null;

    return line;
  }

  /**
   * Get a line of source code
   *
   * @return Code line list
   */
  public CodeLine[] getCodeLineList() {
    if (m_sourceList == null || m_sourceList.size() <= 0) return null;
    return (CodeLine[]) m_sourceList.toArray(new CodeLine[0]);
  }
}
