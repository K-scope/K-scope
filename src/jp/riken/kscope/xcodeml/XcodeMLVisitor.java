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

import java.util.List;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.xcodeml.util.XmlNodeUtil;
import jp.riken.kscope.xcodeml.xml.DefModelArraySubscriptSequence;
import jp.riken.kscope.xcodeml.xml.EnumError;
import jp.riken.kscope.xcodeml.xml.EnumType;
import jp.riken.kscope.xcodeml.xml.FdataDeclSequence;
import jp.riken.kscope.xcodeml.xml.FdoStatementSequence;
import jp.riken.kscope.xcodeml.xml.FequivalenceDeclSequence;
import jp.riken.kscope.xcodeml.xml.GotoStatementSequence;
import jp.riken.kscope.xcodeml.xml.IXmlNode;
import jp.riken.kscope.xcodeml.xml.IXmlTypeTableChoice;
import jp.riken.kscope.xcodeml.xml.gen.*;

/**
 * XML node search class
 *
 * @author RIKEN
 */
public class XcodeMLVisitor extends XcodeMLVisitorImpl {

  /** XML parsing execution environment */
  private XcodeMLContext _context;
  /** XML validation class */
  private XcodeMLValidator _validator;
  /** Next XML node */
  private IXmlNode _nextNode;

  /**
   * Constructor
   *
   * @param context XcodeML parser context class
   */
  public XcodeMLVisitor(XcodeMLContext context) {
    _context = context;
    _validator = new XcodeMLValidator();
    _context.setVisitor(this);
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
  @SuppressWarnings("unused")
  private boolean _isUnderModuleDef() {
    return _context.isInvokeNodeOf(FmoduleDefinition.class, 2);
  }

  /**
   * Preprocessing of enter method.
   *
   * @param visitable Instance of IXmlNode.
   * @return true/false
   */
  private boolean _preEnter(IXmlNode visitable) {
    if (_context.isDebugMode()) {
      _context.debugPrintLine(
          String.format("%1024s", "").subSequence(0, (_context.getInvokeNodeStackSize() - 1) * 2)
              + "<"
              + visitable.getClass().getSimpleName()
              + ">");
    }
    return true;
  }

  /**
   * Postprocessing of enter method.
   *
   * @param visitable Instance of IXmlNode.
   * @return true/false
   */
  private boolean _postEnter(IXmlNode visitable) {
    if (_context.isDebugMode()) {
      _context.debugPrintLine(
          String.format("%1024s", " ").subSequence(0, (_context.getInvokeNodeStackSize() - 1) * 2)
              + "</"
              + visitable.getClass().getSimpleName()
              + ">");
    }
    visitable.leave(this);

    return true;
  }

  /**
   * Checks if object represents a constant expression.
   *
   * @param node Instance of IXmlNode
   * @return true if node represents a constant expression.
   */
  @SuppressWarnings("unused")
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
   * Call enter method of node.
   *
   * @param nodeArray IXmlNode array.
   * @return true/false
   */
  private boolean _invokeEnter(IXmlNode[] nodeArray) {
    IXmlNode currentNode = null;

    if (nodeArray == null) {
      // Succeed forcibly.
      return true;
    }

    for (IXmlNode node : nodeArray) {
      if (_validator.validAttr(node) == false) {
        _context.debugPrintLine("Detected insufficient attributes");
        _context.setLastErrorMessage(_validator.getErrDesc());
        return false;
      }

      // Set the current node to _nextNode.
      _nextNode = node;

      if (currentNode != null) {
        // Process the previous node.
        if (invokeEnter(currentNode) == false) {
          return false;
        }
        this._context.setPreviousNode(currentNode);
      }

      // Set the current node to currentNode and process in the next loop.
      currentNode = node;
    }

    _nextNode = null;
    if (invokeEnter(currentNode) == false) {
      return false;
    }
    this._context.setPreviousNode(currentNode);

    return true;
  }

  /**
   * Call enter method of child node.
   *
   * @param nodees Parent IXmlNode.
   * @return true/false
   */
  private boolean _invokeChildEnter(IXmlNode[] nodees) {
    if (nodees == null) {
      // Succeed forcibly.
      return true;
    }
    return _invokeEnter(nodees);
  }

  /**
   * Call enter method of node.
   *
   * @param node IXmlNode array.
   * @return true/false
   */
  public boolean invokeEnter(IXmlNode node) {
    if (node == null) {
      // Succeed forcibly.
      return true;
    }

    assert (node instanceof IXmlNode);
    IXmlNode visitable = node;

    boolean result = true;
    _context.pushInvokeNode(node);

    result = _preEnter(visitable);
    if (result) {
      result = (node).enter(this);
    }
    if (result) {
      result = _postEnter(visitable);
    }

    _context.popInvokeNode();

    // Even if an error occurs on the way, it will be processed to the end.
    if (result == false) {
      System.err.println("Error:invokeEnter : error node[" + node.toString() + "]");
    }
    //      return result;
    return true;
  }

  /**
   * Decompile 'XcodeProgram' element in XcodeML/F.
   *
   * @return true/false
   */
  @Override
  public boolean enter(XcodeProgram visitable) {
    // DONE: XcodeProgram
    XcodeMLTypeManager typeManager = _context.getTypeManager();

    // for global symbol
    typeManager.enterScope();

    if (_invokeChildEnter(XmlNodeUtil.getXmlChildNodes(visitable)) == false) {
      return false;
    }

    typeManager.leaveScope();

    return true;
  }

  /**
   * Decompile "typeTable" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   */
  @Override
  public boolean enter(TypeTable visitable) {
    // DONE: TypeTable
    List<IXmlNode> list = visitable.getFbasicTypeOrFfunctionTypeOrFstructType();
    if (list != null) {
      IXmlNode[] array = list.toArray(new IXmlNode[0]);
      if (_invokeEnter(array) == false) {
        return false;
      }
    }

    return true;
  }

  /** Decompile "FbasicType" element in XcodeML/F. */
  @Override
  public boolean enter(FbasicType visitable) {
    // DONE: FbasicType
    // Note:
    // Because handle it at a upper level element,
    // warn it when this method was called it.
    assert (_context.isInvokeAncestorNodeOf(TypeTable.class));

    XcodeMLTypeManager typeManager = _context.getTypeManager();
    typeManager.addType(visitable);

    return true;
  }

  /** Decompile child group of "FbasicType" element in XcodeML/F. */
  @Override
  public boolean enter(DefModelArraySubscriptSequence visitable) {
    CodeBuilder writer = _context.getCodeBuilder();
    return writer.enter(visitable);
  }

  /** Decompile child group of "FbasicType" element in XcodeML/F. */
  @Override
  public boolean enter(CoShape visitable) {
    CodeBuilder writer = _context.getCodeBuilder();
    return writer.enter(visitable);
  }

  /** Decompile "FfunctionType" element in XcodeML/F. */
  @Override
  public boolean enter(FfunctionType visitable) {
    // DONE: FfunctionType
    if (_context.isInvokeAncestorNodeOf(TypeTable.class)) {
      XcodeMLTypeManager typeManager = _context.getTypeManager();
      typeManager.addType(visitable);
    } else {
      // Note:
      // Because handle it at a upper level element,
      // warn it when this method was called it.
      assert (false);
    }

    return true;
  }

  /**
   * Decompile "FstructType" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   */
  @Override
  public boolean enter(FstructType visitable) {
    // DONE: FstructType
    if (_context.isInvokeAncestorNodeOf(TypeTable.class)) {
      XcodeMLTypeManager typeManager = _context.getTypeManager();
      typeManager.addType(visitable);
    } else {
      if (invokeEnter(visitable.getSymbols()) == false) {
        return false;
      }
    }

    return true;
  }

  /** Decompile "globalSymbols" element in XcodeML/F. */
  @Override
  public boolean enter(GlobalSymbols visitable) {
    // DONE: GlobalSymbols
    XcodeMLTypeManager typeManager = _context.getTypeManager();
    for (Id idElem : visitable.getId()) {
      typeManager.addSymbol(idElem);
    }

    return true;
  }

  /**
   * Decompile "globalDeclarations" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   */
  @Override
  public boolean enter(GlobalDeclarations visitable) {
    // DONE: GlobalDeclarations
    if (_invokeChildEnter(XmlNodeUtil.getXmlChildNodes(visitable)) == false) {
      return false;
    }

    return true;
  }

  /**
   * Decompile "alloc" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   *      ALLOCATE (<span class="Strong">ptr</span>, <span class="Strong">ptr_array(10)</span>, STAT = error)<br/>
   *      NULLIFY (<span class="Strong">ptr</span>, <span class="Strong">ptr_array</span>)<br/>
   *      DEALLOCATE (<span class="Strong">ptr</span>, <span class="Strong">ptr_array</span>, STAT = error)<br/>
   * </div></code>
   */
  @Override
  public boolean enter(Alloc visitable) {
    CodeBuilder writer = _context.getCodeBuilder();

    // DONE: Alloc
    if (invokeEnter(XmlNodeUtil.getXmlNodeChoice(visitable)) == false) {
      return false;
    }

    // Parent node is FallocateStatement?
    if (_context.isInvokeNodeOf(FallocateStatement.class, 1)) {
      // IDefModelArraySubscriptChoice[] arraySubscriptChoice =
      // visitable.getDefModelArraySubscript();
      DefModelArraySubscriptSequence defModel =
          new DefModelArraySubscriptSequence(visitable.getIndexRangeOrArrayIndex());
      IXmlNode[] arraySubscriptChoice = defModel.getIndexRangeOrArrayIndex();
      if ((arraySubscriptChoice != null) && (arraySubscriptChoice.length > 0)) {
        if (writer.writeIndexRangeArray(arraySubscriptChoice) == false) {
          return false;
        }
      }
    }

    CoShape coShape = visitable.getCoShape();
    if (coShape != null) {
      if (!invokeEnter(coShape)) return false;
    }

    return true;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
  }

  /**
   * Decompile "arrayIndex" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   *      array = int_array_variable(<span class="Strong">10</span>,
   *      1:10, 1:, :)<br/>
   * </div></code>
   */
  @Override
  public boolean enter(ArrayIndex visitable) {
    // DONE: ArrayIndex
    if (invokeEnter(XmlNodeUtil.getXmlNodeChoice(visitable)) == false) {
      return false;
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
    boolean result = true;
    // DONE: FassignStatement
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return true;
  }

  /**
   * Decompile "body" element in XcodeML/F.
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
   *      (any statement...)<br/>
   *      </div>
   * </div>
   * END PROGRAM main<br/>
   * </div></code>
   */
  @Override
  public boolean enter(Body visitable) {
    // DONE: Body
    List<IXmlNode> list = visitable.getFifStatementOrFdoStatementOrFdoWhileStatement();
    IXmlNode[] array = list.toArray(new IXmlNode[0]);
    if (_invokeEnter(array) == false) {
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
    // DONE: Condition
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
  }

  /**
   * Decompile "Declarations" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   */
  @Override
  public boolean enter(Declarations visitable) {
    // DONE: Declarations
    if (_invokeChildEnter(XmlNodeUtil.getXmlChildNodes(visitable)) == false) {
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
  }

  /**
   * Decompile "else" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * IF (variable == 1) THEN<br/>
   * <div class="Indent1">
   *      (any statement...)<br/>
   * </div>
   * ELSE<br/>
   * <div class="Indent1">
   *      <div class="Strong">
   *      (any statement...)<br/>
   *      </div>
   * </div>
   * END IF<br/>
   * </div></code>
   */
  @Override
  public boolean enter(Else visitable) {
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    // Body
    if (invokeEnter(visitable.getBody()) == false) {
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
  }

  /**
   * Decompile "FallocateStatement" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   *      <div class="Strong">
   *      ALLOCATE (ptr, ptr_array(10), STAT = error)<br/>
   *      </div>
   * </div></code>
   */
  @Override
  public boolean enter(FallocateStatement visitable) {
    // DONE: FallocateStatement
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
  }

  /**
   * Decompile "FarrayConstructor" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   *      array = <span class="Strong">(/ 1, 2, (I, I = 1, 10, 2) /)</span><br/>
   * </div></code>
   */
  @Override
  public boolean enter(FarrayConstructor visitable) {
    // DONE: FarrayConstructor
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
  }

  /**
   * Decompile "FarrayRef" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   *      array = <span class="Strong">int_array_variable(10, 1:10, 1:, :)</span><br/>
   * </div></code>
   */
  @Override
  public boolean enter(FarrayRef visitable) {
    // DONE: FarrayRef
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
  }

  /**
   * Decompile "FcoArrayRef" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   *      array = <span class="Strong">int_coarray_variable[10, 1:10, 1:, *]</span><br/>
   * </div></code>
   */
  @Override
  public boolean enter(FcoArrayRef visitable) {
    // DONE: FcoArrayRef
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    if (invokeEnter(visitable.getBody()) == false) {
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
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
    // DONE: FcharacterRef
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
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
    // DONE: FcomplexConstant
    // IDefModelExprChoice realPart = visitable.getDefModelExpr1();
    // IDefModelExprChoice imaginalPart = visitable.getDefModelExpr2();
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    List<FfunctionDefinition> list = visitable.getFfunctionDefinition();
    FfunctionDefinition[] array = list.toArray(new FfunctionDefinition[0]);
    if (_invokeEnter(array) == false) {
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
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
    // DONE: FdataDeclSequence
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
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
    // DONE: FdoLoop
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
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
    // DONE: FdoStatement
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    if (invokeEnter(visitable.getBody()) == false) {
      return false;
    }

    return true;
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
    // DONE: FdoStatementSequence
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
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
    // DONE: FdoWhileStatement
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    if (invokeEnter(visitable.getBody()) == false) {
      return false;
    }

    return true;
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
    // DONE: FendFileStatement
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    // ======
    // Inside
    // ======
    if (invokeEnter(visitable.getDeclarations()) == false) {
      return false;
    }

    return true;
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
   * </div></code>
   */
  @Override
  public boolean enter(FfunctionDefinition visitable) {
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    // ========
    // Epilogue
    // ========
    XcodeMLTypeManager typeManager = _context.getTypeManager();
    typeManager.enterScope();

    // ======
    // Inside
    // ======
    if (invokeEnter(visitable.getSymbols()) == false) {
      return false;
    }

    if (invokeEnter(visitable.getDeclarations()) == false) {
      return false;
    }

    if (invokeEnter(visitable.getBody()) == false) {
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
    // DONE: FifStatement
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    if (invokeEnter(visitable.getThen()) == false) {
      return false;
    }

    Else elseElem = visitable.getElse();
    if (elseElem != null) {
      if (invokeEnter(visitable.getElse()) == false) {
        return false;
      }
    }
    return result;
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
    // DONE: FinquireStatement
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    /****
     * List<IXmlNode> list = visitable
     * .getFfunctionDeclOrFmoduleProcedureDecl();
     * IXmlNode[] array = (IXmlNode[]) list.toArray(new IXmlNode[0]);
     * if (_invokeEnter(array) == false) {
     * return false;
     * }
     ***/

    return true;
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
      if (typeId != EnumType.DERIVED && typeId != EnumType.LOGICAL) {
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

    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
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
    if (invokeEnter(visitable.getVarRef()) == false) {
      return false;
    }

    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    // ========
    // Epilogue
    // ========
    XcodeMLTypeManager typeManager = _context.getTypeManager();
    typeManager.enterScope();

    // ======
    // Inside
    // ======
    if (invokeEnter(visitable.getSymbols()) == false) {
      return false;
    }

    if (invokeEnter(visitable.getDeclarations()) == false) {
      return false;
    }

    if (invokeEnter(visitable.getFcontainsStatement()) == false) {
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    XcodeMLTypeManager typeManager = _context.getTypeManager();
    typeManager.enterScope();

    // ======
    // Inside
    // ======
    if (invokeEnter(visitable.getSymbols()) == false) {
      return false;
    }

    if (invokeEnter(visitable.getDeclarations()) == false) {
      return false;
    }

    return true;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
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
    // DONE: FprintStatement
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
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
    // DONE: FreadStatement
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
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
      if (typeId != EnumType.DERIVED && typeId != EnumType.REAL) {
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

    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
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
    // DONE: FrewindStatement
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
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
    // DONE: FselectCaseStatement
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    List<FcaseLabel> list = visitable.getFcaseLabel();
    FcaseLabel[] array = list.toArray(new FcaseLabel[0]);
    if (_invokeEnter(array) == false) {
      return false;
    }

    return true;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
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
    if (typeChoice == null) {
      _context.setLastErrorMessage(
          XmlNodeUtil.formatError(
              visitable, EnumError.XCODEML_TYPE_NOT_FOUND, visitable.getType()));
      return false;
    } else if ((typeChoice instanceof FstructType) == false) {
      _context.setLastErrorMessage(
          XmlNodeUtil.formatError(
              visitable,
              EnumError.XCODEML_TYPE_MISMATCH,
              "struct definition",
              typeChoice.getClass().getSimpleName(),
              "FstructType"));
      return false;
    }

    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
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
    Name nameElem = visitable.getName();
    if (_validator.validAttr(nameElem) == false) {
      _context.debugPrintLine("Detected insufficient attributes");
      _context.setLastErrorMessage(_validator.getErrDesc());
      return false;
    }

    String typeId = nameElem.getType();
    XcodeMLTypeManager typeManager = _context.getTypeManager();
    IXmlTypeTableChoice typeChoice = typeManager.findType(typeId);
    if (typeChoice == null) {
      _context.setLastErrorMessage(
          XmlNodeUtil.formatError(visitable, EnumError.XCODEML_TYPE_NOT_FOUND, nameElem.getType()));
      return false;
    } else if ((typeChoice instanceof FstructType) == false) {
      _context.setLastErrorMessage(
          XmlNodeUtil.formatError(
              visitable,
              EnumError.XCODEML_TYPE_MISMATCH,
              "struct definition",
              typeChoice.getClass().getSimpleName(),
              "FstructType"));
      return false;
    }

    FstructType structTypeElem = (FstructType) typeChoice;
    String structTypeName = nameElem.getValue();

    typeManager.putAliasTypeName(typeId, structTypeName);

    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    // PRIVATE, SEQUENCE statement inside the TYPE statement
    writer.enterStructTypeElem(visitable);

    if (invokeEnter(structTypeElem) == false) {
      return false;
    }

    return true;
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
    // DONE: FunctionCall
    Name functionNameElem = visitable.getName();
    if (functionNameElem == null) {
      _context.debugPrintLine("Detected a function call without the name element.");
      _context.setLastErrorMessage(
          XmlNodeUtil.formatError(
              visitable, EnumError.XCODEML_SEMANTICS, XmlNodeUtil.getElementName(visitable)));
      return false;
    }

    String functionName = functionNameElem.getValue();
    if (StringUtils.isNullOrEmpty(functionName)) {
      _context.debugPrintLine("Function name is empty.");
      _context.setLastErrorMessage(
          XmlNodeUtil.formatError(
              functionNameElem,
              EnumError.XCODEML_SEMANTICS,
              XmlNodeUtil.getElementName(functionNameElem)));
      return false;
    }

    // Note:
    // If it is built-in function, it is not on the type table.
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();

    // Get the current start character position to get the substring of FunctionCall.
    int start_idx = writer.getCurrentColumn();

    result = writer.enter(visitable);
    if (!result) return result;

    // FunctionCall substring
    String call = writer.stealLineBuf(start_idx);

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enterFunctionCall(visitable, call);
    if (!result) return result;

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
    // DONE: FuseDecl
    for (Rename renameElem : visitable.getRename()) {
      if (_validator.validAttr(renameElem) == false) {
        _context.debugPrintLine("Detected insufficient attributes");
        _context.setLastErrorMessage(_validator.getErrDesc());
        return false;
      }
    }

    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
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
    for (Renamable renamableElem : visitable.getRenamable()) {
      if (_validator.validAttr(renamableElem) == false) {
        _context.debugPrintLine("Detected insufficient attributes");
        _context.setLastErrorMessage(_validator.getErrDesc());
        return false;
      }
    }

    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
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
    // DONE: FwhereStatement
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    if (invokeEnter(visitable.getThen()) == false) {
      return false;
    }

    Else elseElem = visitable.getElse();
    if (elseElem != null) {
      if (invokeEnter(visitable.getElse()) == false) {
        return false;
      }
    }

    return result;
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
    // DONE: FwriteStatement
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
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
    // DONE: GotoStatement
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
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
    // DONE: GotoStatementSequence
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
  }

  /**
   * Decompile "id" element in XcodeML/F.
   *
   * @deprecated Because handle it at a upper level element, warn it when this method was called it.
   */
  @Override
  public boolean enter(Id visitable) {
    // DONE: Id
    assert (false);
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
    // DONE: IndexRange
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
  }

  /**
   * Decompile "kind" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * PROGRAM main<br/>
   * <div class="Indent1">
   *      INTEGER(KIND=<span class="Strong">8</span>) :: i
   * </div>
   * END PROGRAM main<br/>
   * </div></code>
   */
  @Override
  public boolean enter(Kind visitable) {
    // DONE: Kind
    if (invokeEnter(XmlNodeUtil.getXmlNodeChoice(visitable)) == false) {
      return false;
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
    // DONE: Len
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
  }

  /**
   * Decompile "lowerBound" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   *      array = int_array_variable(10,
   *      <span class="Strong">1</span>:10,
   *      <span class="Strong">1</span>:, :)<br/>
   * </div></code>
   */
  @Override
  public boolean enter(LowerBound visitable) {
    // DONE: LowerBound
    if (invokeEnter(XmlNodeUtil.getXmlNodeChoice(visitable)) == false) {
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
  }

  /**
   * Decompile "name" element in XcodeML/F.
   *
   * @deprecated Because handle it at a upper level element, warn it when this method was called it.
   */
  @Override
  public boolean enter(Name visitable) {
    // DONE: Name
    assert (false);
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
    // DONE: NamedValue
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
  }

  /**
   * Decompile "renamable" element in XcodeML/F.
   *
   * @deprecated Because handle it at a upper level element, warn it when this method was called it.
   */
  @Override
  @Deprecated
  public boolean enter(Renamable visitable) {
    // DONE: Renamable
    assert (false);
    return true;
  }

  /**
   * Decompile "rename" element in XcodeML/F.
   *
   * @deprecated Because handle it at a upper level element, warn it when this method was called it.
   */
  @Override
  @Deprecated
  public boolean enter(Rename visitable) {
    // DONE: Rename
    assert (false);
    return true;
  }

  /**
   * Decompile "statementLabel" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * GOTO 1000
   * <span class="Strong">1000 </span>CONTINUE<br/>
   * (any statement...)<br/>
   * </div></code>
   */
  @Override
  public boolean enter(StatementLabel visitable) {
    // DONE: StatementLabel
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enterStatementLabel(visitable, _nextNode);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enterStatementLabel(visitable, _nextNode);
    if (!result) return result;

    return result;
  }

  /**
   * Decompile "step" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   *      array = int_array_variable(10,
   *      1:10:<span class="Strong">2</span>,
   *      ::<span class="Strong">2</span>, :)<br/>
   * </div></code>
   */
  @Override
  public boolean enter(Step visitable) {
    // DONE: Step
    if (invokeEnter(XmlNodeUtil.getXmlNodeChoice(visitable)) == false) {
      return false;
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
      _context.debugPrintLine("Add to symbol table.");
      XcodeMLTypeManager typeManager = _context.getTypeManager();
      for (Id idElem : visitable.getId()) {
        typeManager.addSymbol(idElem);
      }

      // _context.debugPrint(typeManager.toString());
    } else {
      boolean result = true;
      CodeBuilder writer = _context.getCodeBuilder();
      result = writer.enter(visitable);
      if (!result) return result;

      DbUpdater updater = _context.getDbUpdater();
      result = updater.enter(visitable);
      if (!result) return result;
    }

    return true;
  }

  /**
   * Decompile "then" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   * IF (variable == 1) THEN<br/>
   * <div class="Indent1">
   *      <div class="Strong">
   *      (any statement...)<br/>
   *      </div>
   * </div>
   * ELSE<br/>
   * <div class="Indent1">
   *      (any statement...)<br/>
   * </div>
   * END IF<br/>
   * </div></code>
   */
  @Override
  public boolean enter(Then visitable) {
    boolean result = true;
    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    // DONE: Then
    if (invokeEnter(visitable.getBody()) == false) {
      return false;
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
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
  }

  /**
   * Decompile "lowerBound" element in XcodeML/F.
   *
   * @example <code><div class="Example">
   *      array = int_array_variable(<span class="Strong">10</span>,
   *      1:<span class="Strong">10</span>,
   *      1:, :)<br/>
   * </div></code>
   */
  @Override
  public boolean enter(UpperBound visitable) {
    // DONE: UpperBound
    if (invokeEnter(XmlNodeUtil.getXmlNodeChoice(visitable)) == false) {
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
    if (StringUtils.isNullOrEmpty(name)) {
      _context.setLastErrorMessage(
          XmlNodeUtil.formatError(
              visitable,
              EnumError.XCODEML_NEED_ATTR,
              "name",
              XmlNodeUtil.getElementName(visitable)));
      return false;
    }

    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
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
    if (StringUtils.isNullOrEmpty(name)) {
      _context.setLastErrorMessage(
          XmlNodeUtil.formatError(
              visitable,
              EnumError.XCODEML_NEED_ATTR,
              "name",
              XmlNodeUtil.getElementName(visitable)));
      return false;
    }

    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
  }

  /**
   * Decompile "value" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   */
  @Override
  public boolean enter(Value visitable) {
    // DONE: Value
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    if (invokeEnter(XmlNodeUtil.getXmlNodeChoice(visitable)) == false) {
      return false;
    }

    return true;
  }

  /** Decompile "RepeatCount" element in XcodeML/F. */
  @Override
  public boolean enter(RepeatCount visitable) {
    return invokeEnter(XmlNodeUtil.getXmlNodeChoice(visitable));
  }

  /**
   * Decompile "valueList" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   */
  @Override
  public boolean enter(ValueList visitable) {
    // DONE: ValueList
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
  }

  /**
   * Decompile "Ffunction" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   */
  @Override
  public boolean enter(Ffunction visitable) {
    // DONE: Ffunction
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
  }

  /**
   * Decompile "Var" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   */
  @Override
  public boolean enter(Var visitable) {
    // DONE: Var
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
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
    // DONE: VarDecl
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    DbUpdater updater = _context.getDbUpdater();
    result = updater.enter(visitable);
    if (!result) return result;

    return result;
  }

  /**
   * Decompile "varList" element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   */
  @Override
  public boolean enter(VarList visitable) {
    // DONE: VarList
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    result = writer.enter(visitable);
    if (!result) return result;

    return result;
  }

  /**
   * Decompile 'varRef' element in XcodeML/F.
   *
   * <p>The decompilation result depends on a child element.
   */
  @Override
  public boolean enter(VarRef visitable) {
    // DONE: VarRef
    if (invokeEnter(XmlNodeUtil.getXmlNodeChoice(visitable)) == false) {
      return false;
    }

    return true;
  }

  @Override
  public void leave(FfunctionDefinition visitable) {
    CodeBuilder writer = _context.getCodeBuilder();
    writer.leave(visitable);

    DbUpdater updater = _context.getDbUpdater();
    updater.leave(visitable);

    return;
  }

  @Override
  public void leave(FmoduleDefinition visitable) {
    CodeBuilder writer = _context.getCodeBuilder();
    writer.leave(visitable);

    DbUpdater updater = _context.getDbUpdater();
    updater.leave(visitable);

    return;
  }

  @Override
  public void leave(FdoStatement visitable) {
    boolean result = true;
    CodeBuilder writer = _context.getCodeBuilder();
    writer.leave(visitable);
    if (!result) return;

    DbUpdater updater = _context.getDbUpdater();
    updater.leave(visitable);

    return;
  }

  @Override
  public void leave(FdoWhileStatement visitable) {
    CodeBuilder writer = _context.getCodeBuilder();
    writer.leave(visitable);

    DbUpdater updater = _context.getDbUpdater();
    updater.leave(visitable);

    return;
  }

  @Override
  public void leave(FfunctionDecl visitable) {
    CodeBuilder writer = _context.getCodeBuilder();
    writer.leave(visitable);

    DbUpdater updater = _context.getDbUpdater();
    updater.leave(visitable);
  }

  @Override
  public void leave(FifStatement visitable) {
    CodeBuilder writer = _context.getCodeBuilder();
    writer.leave(visitable);

    DbUpdater updater = _context.getDbUpdater();
    updater.leave(visitable);
  }

  @Override
  public void leave(FinterfaceDecl visitable) {
    CodeBuilder writer = _context.getCodeBuilder();
    writer.leave(visitable);

    DbUpdater updater = _context.getDbUpdater();
    updater.leave(visitable);
  }

  @Override
  public void leave(FblockDataDefinition visitable) {
    XcodeMLTypeManager typeManager = _context.getTypeManager();
    typeManager.leaveScope();

    CodeBuilder writer = _context.getCodeBuilder();
    writer.leave(visitable);

    DbUpdater updater = _context.getDbUpdater();
    updater.leave(visitable);

    return;
  }

  @Override
  public void leave(FselectCaseStatement visitable) {
    CodeBuilder writer = _context.getCodeBuilder();
    writer.leave(visitable);

    DbUpdater updater = _context.getDbUpdater();
    updater.leave(visitable);

    return;
  }

  @Override
  public void leave(FstructDecl visitable) {
    CodeBuilder writer = _context.getCodeBuilder();
    writer.leave(visitable);

    DbUpdater updater = _context.getDbUpdater();
    updater.leave(visitable);

    return;
  }

  @Override
  public void leave(FwhereStatement visitable) {
    CodeBuilder writer = _context.getCodeBuilder();
    writer.leave(visitable);

    DbUpdater updater = _context.getDbUpdater();
    updater.leave(visitable);

    return;
  }

  @Override
  public void leave(Then visitable) {
    DbUpdater updater = _context.getDbUpdater();
    updater.leave(visitable);
    return;
  }

  @Override
  public void leave(Else visitable) {
    DbUpdater updater = _context.getDbUpdater();
    updater.leave(visitable);
    return;
  }

  @Override
  public void leave(FcaseLabel visitable) {
    DbUpdater updater = _context.getDbUpdater();
    updater.leave(visitable);
    return;
  }
}
