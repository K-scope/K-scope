package jp.riken.kscope.xcodeml.clang;

import java.util.List;

import javax.xml.bind.annotation.XmlElement;

import jp.riken.kscope.exception.XcodeMLException;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.xcodeml.EnumError;
import jp.riken.kscope.xcodeml.clang.XcodeMLContext;
import jp.riken.kscope.xcodeml.clang.utils.XmlNodeUtil;
import jp.riken.kscope.xcodeml.clang.xml.IXmlNode;
import jp.riken.kscope.xcodeml.clang.XcodeMLValidator;
import jp.riken.kscope.xcodeml.clang.XcodeMLTypeManager;
import jp.riken.kscope.xcodeml.clang.xml.gen.Arguments;
import jp.riken.kscope.xcodeml.clang.xml.gen.ArrayType;
import jp.riken.kscope.xcodeml.clang.xml.gen.AssignExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.BasicType;
import jp.riken.kscope.xcodeml.clang.xml.gen.Body;
import jp.riken.kscope.xcodeml.clang.xml.gen.CastExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.CompoundStatement;
import jp.riken.kscope.xcodeml.clang.xml.gen.Declarations;
import jp.riken.kscope.xcodeml.clang.xml.gen.DivExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.DoStatement;
import jp.riken.kscope.xcodeml.clang.xml.gen.EnumType;
import jp.riken.kscope.xcodeml.clang.xml.gen.ExprStatement;
import jp.riken.kscope.xcodeml.clang.xml.gen.FloatConstant;
import jp.riken.kscope.xcodeml.clang.xml.gen.ForStatement;
import jp.riken.kscope.xcodeml.clang.xml.gen.Function;
import jp.riken.kscope.xcodeml.clang.xml.gen.FunctionDefinition;
import jp.riken.kscope.xcodeml.clang.xml.gen.FunctionType;
import jp.riken.kscope.xcodeml.clang.xml.gen.GlobalSymbols;
import jp.riken.kscope.xcodeml.clang.xml.gen.IfStatement;
import jp.riken.kscope.xcodeml.clang.xml.gen.Init;
import jp.riken.kscope.xcodeml.clang.xml.gen.IntConstant;
import jp.riken.kscope.xcodeml.clang.xml.gen.Iter;
import jp.riken.kscope.xcodeml.clang.xml.gen.LogAndExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.LogEQExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.LogGEExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.LogGTExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.LogLEExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.LogLTExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.LogNEQExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.LogNotExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.LogOrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.LonglongConstant;
import jp.riken.kscope.xcodeml.clang.xml.gen.MinusExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.ModExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.MoeConstant;
import jp.riken.kscope.xcodeml.clang.xml.gen.MulExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.Params;
import jp.riken.kscope.xcodeml.clang.xml.gen.PlusExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.PointerType;
import jp.riken.kscope.xcodeml.clang.xml.gen.PostDecrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.PostIncrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.PreDecrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.PreIncrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.StringConstant;
import jp.riken.kscope.xcodeml.clang.xml.gen.StructType;
import jp.riken.kscope.xcodeml.clang.xml.gen.UnionType;
import jp.riken.kscope.xcodeml.clang.xml.gen.XcodeProgram;
import jp.riken.kscope.xcodeml.clang.xml.gen.TypeTable;
import jp.riken.kscope.xcodeml.clang.xml.gen.Id;
import jp.riken.kscope.xcodeml.clang.CodeBuilder;
import jp.riken.kscope.xcodeml.clang.DbUpdater;
import jp.riken.kscope.xcodeml.clang.xml.gen.VarDecl;
import jp.riken.kscope.xcodeml.clang.xml.gen.Symbols;
import jp.riken.kscope.xcodeml.clang.xml.gen.DoStatement;
import jp.riken.kscope.xcodeml.clang.xml.gen.FunctionCall;
import jp.riken.kscope.xcodeml.clang.xml.gen.Name;
import jp.riken.kscope.xcodeml.clang.xml.gen.Var;
import jp.riken.kscope.xcodeml.clang.xml.gen.Condition;
import jp.riken.kscope.xcodeml.clang.xml.gen.Else;
import jp.riken.kscope.xcodeml.clang.xml.gen.Then;


/**
 * XMLノード探索クラス
 * @author RIKEN
 */
public class XcodeMLVisitor extends XcodeMLVisitorImpl {

    /** XMLパース実行環境 */
    private XcodeMLContext _context;
    /** XML検証クラス */
    private XcodeMLValidator _validator;
    /** 次XMLノード */
    private IXmlNode _nextNode;

    /**
     * コンストラクタ
     * @param context		XcodeMLパーサコンテキストクラス
     */
    public XcodeMLVisitor(XcodeMLContext context) {
        _context = context;
        _validator = new XcodeMLValidator();
        _context.setVisitor(this);
    }

    /**
     * Call enter method of node.
     *
     * @param node
     *            IXmlNode array.
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

        // 途中エラー発生しても最後まで処理する。
        if (result == false) {
            System.err.println("Error:invokeEnter : error node[" + node.toString() + "]");
        }

        return true;
    }


    /**
     * Preprocessing of enter method.
     *
     * @param visitable
     *            Instance of IXmlNode.
     * @return true/false
     */
    private boolean _preEnter(IXmlNode visitable) {
        if (_context.isDebugMode()) {
            _context.debugPrintLine(String.format("%1024s", "").subSequence(0,
                    (_context.getInvokeNodeStackSize() - 1) * 2)
                    + "<" + visitable.getClass().getSimpleName() + ">");
        }
        return true;
    }

    /**
     * Postprocessing of enter method.
     *
     * @param visitable
     *            Instance of IXmlNode.
     * @return true/false
     */
    private boolean _postEnter(IXmlNode visitable) {
        if (_context.isDebugMode()) {
            _context.debugPrintLine(String.format("%1024s", " ").subSequence(0,
                    (_context.getInvokeNodeStackSize() - 1) * 2)
                    + "</" + visitable.getClass().getSimpleName() + ">");
        }
        visitable.leave(this);

        return true;
    }

    /**
     * Call enter method of node.
     *
     * @param nodeArray
     *            IXmlNode array.
     * @return true/false
     */
    private boolean _invokeEnters(IXmlNode[] nodeArray) {
        IXmlNode currentNode = null;

        if (nodeArray == null) {
            // Succeed forcibly.
            return true;
        }

        for (IXmlNode node : nodeArray) {
            if (_validator.validate(node) == false) {
                _context.debugPrintLine("Detected insufficient attributes");
                _context.setLastErrorMessage(_validator.getErrDesc());
                return false;
            }

            // 現在ノードを_nextNodeにセットする。
            _nextNode = node;

            if (currentNode != null) {
                // １つ手前のノードを処理する。
                if (invokeEnter(currentNode) == false) {
                    return false;
                }
                this._context.setPreviousNode(currentNode);
            }

            // 現在ノードをcurrentNodeにセットして次のループで処理する。
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
     * Decompile 'XcodeProgram' element in XcodeML/C.
     *
     * @return true/false
     */
    @Override
    public boolean enter(XcodeProgram visitable) {
        // DONE: XcodeProgram
        XcodeMLTypeManager typeManager = _context.getTypeManager();

        // for global symbol
        typeManager.enterScope();

        if (_invokeEnters(XmlNodeUtil.getXmlChildNodes(visitable)) == false) {
            return false;
        }

        // データベース登録
        boolean result = true;
        DbUpdater updater = _context.getDbUpdater();
        result = updater.enter(visitable);
        if (!result)
            return result;

        typeManager.leaveScope();

        return true;
    }

    /**
     * Decompile "typeTable" element in XcodeML/C.
     * <p>
     * The decompilation result depends on a child element.
     * </p>
     *
     */
    @Override
    public boolean enter(TypeTable visitable) {
        // DONE: TypeTable
        // Typeテーブルの初期化を行う
        XcodeMLTypeManager typeManager = _context.getTypeManager();
        typeManager.clearType();

        List<IXmlNode> list = visitable.getTypes();
        if (list != null) {
            IXmlNode[] array = list.toArray(new IXmlNode[0]);
            if (_invokeEnters(array) == false) {
                return false;
            }
        }

        return true;
    }

    /**
     * Decompile "BasicType" element in XcodeML/C.
     */
    @Override
    public boolean enter(BasicType visitable) {
        assert (_context.isInvokeAncestorNodeOf(TypeTable.class));

        // 基本データ型を登録する
        XcodeMLTypeManager typeManager = this._context.getTypeManager();
        try {
            typeManager.addType(visitable);
        } catch (XcodeMLException e) {
            this._context.debugPrintLine(e.getMessage());
            this._context.setLastErrorMessage(e.getMessage());
        }

        return true;
    }

    /**
     * Decompile "ArrayType" element in XcodeML/C.
     */
    @Override
    public boolean enter(ArrayType visitable) {
        assert (_context.isInvokeAncestorNodeOf(TypeTable.class));

        // 配列型を登録する
        XcodeMLTypeManager typeManager = this._context.getTypeManager();
        try {
            typeManager.addType(visitable);
        } catch (XcodeMLException e) {
            this._context.debugPrintLine(e.getMessage());
            this._context.setLastErrorMessage(e.getMessage());
        }

        return true;
    }

    /**
     * Decompile "EnumType" element in XcodeML/C.
     */
    @Override
    public boolean enter(EnumType visitable) {
        assert (_context.isInvokeAncestorNodeOf(TypeTable.class));

        // 構造体型を登録する
        XcodeMLTypeManager typeManager = this._context.getTypeManager();
        try {
            typeManager.addType(visitable);
        } catch (XcodeMLException e) {
            this._context.debugPrintLine(e.getMessage());
            this._context.setLastErrorMessage(e.getMessage());
        }

        return true;
    }

    /**
     * Decompile "FunctionType" element in XcodeML/C.
     */
    @Override
    public boolean enter(FunctionType visitable) {
        assert (_context.isInvokeAncestorNodeOf(TypeTable.class));

        // 構造体型を登録する
        XcodeMLTypeManager typeManager = this._context.getTypeManager();
        try {
            typeManager.addType(visitable);
        } catch (XcodeMLException e) {
            this._context.debugPrintLine(e.getMessage());
            this._context.setLastErrorMessage(e.getMessage());
        }

        return true;
    }

    /**
     * Decompile "FunctionType" element in XcodeML/C.
     */
    @Override
    public boolean enter(PointerType visitable) {
        assert (_context.isInvokeAncestorNodeOf(TypeTable.class));

        // 構造体型を登録する
        XcodeMLTypeManager typeManager = this._context.getTypeManager();
        try {
            typeManager.addType(visitable);
        } catch (XcodeMLException e) {
            this._context.debugPrintLine(e.getMessage());
            this._context.setLastErrorMessage(e.getMessage());
        }

        return true;
    }

    /**
     * Decompile "StructType" element in XcodeML/C.
     */
    @Override
    public boolean enter(StructType visitable) {
        assert (_context.isInvokeAncestorNodeOf(TypeTable.class));

        // 構造体型を登録する
        XcodeMLTypeManager typeManager = this._context.getTypeManager();
        try {
            typeManager.addType(visitable);
        } catch (XcodeMLException e) {
            this._context.debugPrintLine(e.getMessage());
            this._context.setLastErrorMessage(e.getMessage());
        }

        return true;
    }

    /**
     * Decompile "UnionType" element in XcodeML/C.
     */
    @Override
    public boolean enter(UnionType visitable) {
        assert (_context.isInvokeAncestorNodeOf(TypeTable.class));

        // 構造体型を登録する
        XcodeMLTypeManager typeManager = this._context.getTypeManager();
        try {
            typeManager.addType(visitable);
        } catch (XcodeMLException e) {
            this._context.debugPrintLine(e.getMessage());
            this._context.setLastErrorMessage(e.getMessage());
        }

        return true;
    }


    /**
     * Decompile "globalSymbols" element in XcodeML/C.
     * <p>
     * The decompilation result depends on a child element.
     * </p>
     *
     */
    @Override
    public boolean enter(GlobalSymbols visitable) {
        // DONE: GlobalSymbols
        // Symbolテーブルの初期化を行う
        XcodeMLTypeManager typeManager = _context.getTypeManager();
        typeManager.clearSymbol();

        List<Id> list = visitable.getId();
        if (list != null) {
            Id[] array = list.toArray(new Id[0]);
            if (_invokeEnters(array) == false) {
                return false;
            }
        }

        return true;
    }

    /**
     * Decompile "UnionType" element in XcodeML/C.
     */
    @Override
    public boolean enter(Id visitable) {
        assert (_context.isInvokeAncestorNodeOf(GlobalSymbols.class));

        // シンボルを登録する
        XcodeMLTypeManager typeManager = this._context.getTypeManager();
        try {
            typeManager.addSymbol(visitable);
        } catch (XcodeMLException e) {
            this._context.debugPrintLine(e.getMessage());
            this._context.setLastErrorMessage(e.getMessage());
        }

        return true;
    }

    /**
     * Decompile "VarDecl" element in XcodeML/C.
     */
    @Override
    public boolean enter(VarDecl visitable) {
        // DONE: VarDecl
        boolean result = true;

        // コード作成
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        // データベース登録
        DbUpdater updater = _context.getDbUpdater();
        result = updater.enter(visitable);
        if (!result)
            return result;

        return result;
    }

    /**
     * Decompile "FunctionDefinition" element in XcodeML/C.
     */
    @Override
    public boolean enter(FunctionDefinition visitable) {
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        DbUpdater updater = _context.getDbUpdater();
        result = updater.enter(visitable);
        if (!result)
            return result;

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

        if (invokeEnter(visitable.getBody()) == false) {
            return false;
        }

        if (invokeEnter(visitable.getGccAttributes()) == false) {
            return false;
        }

        return true;
    }


    /**
     * Decompile "symbols" element in XcodeML/C.
     * <p>
     * The decompilation result depends on a child element.
     * </p>
     *
     */
    @Override
    public boolean enter(Symbols visitable) {
        // DONE: Symbols
        for (IXmlNode elem : visitable.getIdOrPragmaOrText()) {
            invokeEnter(elem);
        }

        return true;
    }

    /**
     * Decompile "params" element in XcodeML/C.
     */
    @Override
    public boolean enter(Params visitable) {
        // DONE: Params
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        return result;
    }

    /**
     * Decompile "body" element in XcodeML/C.
     */
    @Override
    public boolean enter(Body visitable) {
        // DONE: Body
        List<IXmlNode> statements = visitable.getStatements();
        if (!_invokeEnters((IXmlNode[])statements.toArray(new IXmlNode[0]))) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "compoundStatement" element in XcodeML/C.
     */
    @Override
    public boolean enter(CompoundStatement visitable) {
        // DONE: CompoundStatement
        Symbols symbols = visitable.getSymbols();
        Declarations declarations = visitable.getDeclarations();
        Body body = visitable.getBody();

        if (!this.invokeEnter(symbols)) {
            return false;
        }
        if (!this.invokeEnter(declarations)) {
            return false;
        }
        if (!this.invokeEnter(body)) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "exprStatement" element in XcodeML/C.
     */
    @Override
    public boolean enter(ExprStatement visitable) {
        // DONE: ExprStatement

        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        DbUpdater updater = _context.getDbUpdater();
        result = updater.enter(visitable);
        if (!result)
            return result;

        return true;
    }

    @Override
    public void leave(ExprStatement visitable) {
        CodeBuilder writer = _context.getCodeBuilder();
        writer.leave(visitable);

        DbUpdater updater = _context.getDbUpdater();
        updater.leave(visitable);

    }

    /**
     * Decompile "AssignExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(AssignExpr visitable) {
        // DONE: AssignExpr
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        // 1つ上のノードがExprStatementの場合のみDB更新
        if (_context.isInvokeNodeOf(ExprStatement.class, 1)) {
            writer.updateCodeLine();

            DbUpdater updater = _context.getDbUpdater();
            result = updater.enter(visitable);
            if (!result)
                return result;
        }

        return true;
    }

    /**
     * Decompile "Declarations" element in XcodeML/C.
     */
    @Override
    public boolean enter(Declarations visitable) {
        List<IXmlNode> nodes = visitable.getFunctionDefinitionOrVarDeclOrFunctionDecl();

        if (!_invokeEnters((IXmlNode[])nodes.toArray(new IXmlNode[0]))) {
            return false;
        }

        return true;
    }

    @Override
    public void leave(FunctionDefinition visitable) {
        CodeBuilder writer = _context.getCodeBuilder();
        writer.leave(visitable);

        DbUpdater updater = _context.getDbUpdater();
        updater.leave(visitable);

        return;
    }


    /**
     * Decompile "DoStatement" element in XcodeML/F.
     */
    @Override
    public boolean enter(DoStatement visitable) {
        // DONE: FdoStatement
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        DbUpdater updater = _context.getDbUpdater();
        result = updater.enter(visitable);
        if (!result)
            return result;

        if (invokeEnter(visitable.getBody()) == false) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "ForStatement" element in XcodeML/C.
     */
    @Override
    public boolean enter(ForStatement visitable) {
        // DONE: ForStatement
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        DbUpdater updater = _context.getDbUpdater();
        result = updater.enter(visitable);
        if (!result)
            return result;

        if (invokeEnter(visitable.getBody()) == false) {
            return false;
        }

        return true;
    }


    @Override
    public void leave(ForStatement visitable) {
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        writer.leave(visitable);
        if (!result)
            return;

        DbUpdater updater = _context.getDbUpdater();
        updater.leave(visitable);

        return;
    }

    @Override
    public void leave(DoStatement visitable) {
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        writer.leave(visitable);
        if (!result)
            return;

        DbUpdater updater = _context.getDbUpdater();
        updater.leave(visitable);

        return;
    }

    /**
     * Decompile "IfStatement" element in XcodeML/C.
     */
    @Override
    public boolean enter(IfStatement visitable) {
        // DONE: FdoStatement
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        DbUpdater updater = _context.getDbUpdater();
        result = updater.enter(visitable);
        if (!result)
            return result;

        // thenブロック
        if (invokeEnter(visitable.getThen()) == false) {
            return false;
        }

        // elseブロック
        Else elseElem = visitable.getElse();
        if (elseElem != null) {
            if (invokeEnter(visitable.getElse()) == false) {
                return false;
            }
        }

        return true;
    }


    @Override
    public boolean enter(Condition visitable) {
        // DONE: Condition
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        DbUpdater updater = _context.getDbUpdater();
        result = updater.enter(visitable);
        if (!result)
            return result;

        return result;
    }

    @Override
    public void leave(IfStatement visitable) {
        CodeBuilder writer = _context.getCodeBuilder();
        writer.leave(visitable);

        DbUpdater updater = _context.getDbUpdater();
        updater.leave(visitable);

    }

    /**
     * Decompile "functionCall" element in XcodeML/C.
     */
    @Override
    public boolean enter(FunctionCall visitable) {
        // DONE: FunctionCall
        Function function = visitable.getFunction();

        if (function == null) {
            _context.debugPrintLine("Detected a function call without the name element.");
            _context.setLastErrorMessage(XmlNodeUtil.formatError(visitable,
                    EnumError.XCODEML_SEMANTICS,
                    XmlNodeUtil.getElementName(visitable)));
            return false;
        }
        if (function.getFuncAddr() != null) {
            String functionName = function.getFuncAddr().getValue();
            if (StringUtils.isNullOrEmpty(functionName)) {
                _context.debugPrintLine("Function name is empty.");
                _context.setLastErrorMessage(XmlNodeUtil.formatError(
                        function, EnumError.XCODEML_SEMANTICS,
                        XmlNodeUtil.getElementName(function)));
                return false;
            }

            // Note:
            // If it is built-in function, it is not on the type table.
            boolean result = true;
            CodeBuilder writer = _context.getCodeBuilder();

            result = writer.enter(visitable);
            if (!result)
                return result;

            // 1つ上のノードがExprStatementの場合のみDB更新
            if (!_context.isInvokeNodeOf(ExprStatement.class, 1)) {
                return true;
            }

            writer.updateCodeLine();
            DbUpdater updater = _context.getDbUpdater();
            result = updater.enter(visitable);
            if (!result)
                return result;
        }

        return true;
    }

    /**
     * Decompile "then" element in XcodeML/C.
     */
    @Override
    public boolean enter(Then visitable) {
        boolean result = true;
        DbUpdater updater = _context.getDbUpdater();
        result = updater.enter(visitable);
        if (!result)
            return result;

        // DONE: Then
        IXmlNode node = XmlNodeUtil.getXmlNodeChoice(visitable);
        if (invokeEnter(node) == false) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "Else" element in XcodeML/C.
     */
    @Override
    public boolean enter(Else visitable) {
        // DONE: Else
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        DbUpdater updater = _context.getDbUpdater();
        result = updater.enter(visitable);
        if (!result)
            return result;

        IXmlNode node = XmlNodeUtil.getXmlNodeChoice(visitable);
        if (invokeEnter(node) == false) {
            return false;
        }

        return true;
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

    /**
     * Decompile "StringConstant" element in XcodeML/C.
     */
    @Override
    public boolean enter(StringConstant visitable) {
        // DONE: StringConstant
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        return result;
    }


    /**
     * Decompile "IntConstant" element in XcodeML/C.
     */
    @Override
    public boolean enter(IntConstant visitable) {
        // DONE: IntConstant
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        return result;
    }

    /**
     * Decompile "FloatConstant" element in XcodeML/C.
     */
    @Override
    public boolean enter(FloatConstant visitable) {

        // DONE: FloatConstant
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        return result;
    }

    /**
     * Decompile "LonglongConstant" element in XcodeML/C.
     */
    @Override
    public boolean enter(LonglongConstant visitable) {

        // DONE: LonglongConstant
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        return result;
    }

    /**
     * Decompile "MoeConstant" element in XcodeML/C.
     */
    @Override
    public boolean enter(MoeConstant visitable) {

        // DONE: MoeConstant
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        return result;
    }

    /**
     * Decompile "plusExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(PlusExpr visitable) {
        // DONE: PlusExpr
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        return result;
    }

    /**
     * Decompile "ModExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(ModExpr visitable) {
        // DONE: ModExpr
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        return result;
    }

    /**
     * Decompile "MinusExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(MinusExpr visitable) {
        // DONE: MinusExpr
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        return result;
    }

    /**
     * Decompile "MulExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(MulExpr visitable) {
        // DONE: PlusExpr
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        return result;
    }

    /**
     * Decompile "DivExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(DivExpr visitable) {
        // DONE: DivExpr
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        return result;
    }

    /**
     * Decompile "Var" element in XcodeML/C.
     */
    @Override
    public boolean enter(Var visitable) {
        // DONE: Var
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        return result;
    }

    /**
     * Decompile "Arguments" element in XcodeML/C.
     */
    @Override
    public boolean enter(Arguments visitable) {
        // DONE: Arguments
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        return result;
    }

    /**
     * Decompile "LogOrExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(LogOrExpr visitable) {
        // DONE: LogOrExpr
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        return result;
    }

    /**
     * Decompile "LogGTExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(LogGTExpr visitable) {
        // DONE: LogGTExpr
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        return result;
    }

    /**
     * Decompile "LogEQExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(LogEQExpr visitable) {
        // DONE: LogEQExpr
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        return result;
    }

    /**
     * Decompile "LogNEQExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(LogNEQExpr visitable) {
        // DONE: LogNEQExpr
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        return result;
    }

    /**
     * Decompile "LogGEExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(LogGEExpr visitable) {
        // DONE: LogGEExpr
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        return result;
    }

    /**
     * Decompile "LogLTExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(LogLTExpr visitable) {
        // DONE: LogLTExpr
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        return result;
    }

    /**
     * Decompile "LogLEExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(LogLEExpr visitable) {
        // DONE: LogLEExpr
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        return result;
    }

    /**
     * Decompile "LogAndExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(LogAndExpr visitable) {
        // DONE: LogAndExpr
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        return result;
    }

    /**
     * Decompile "LogNotExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(LogNotExpr visitable) {
        // DONE: LogNotExpr
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        return result;
    }

    /**
     * Decompile "Init" element in XcodeML/C.
     */
    @Override
    public boolean enter(Init visitable) {
        // DONE: Init
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        DbUpdater updater = _context.getDbUpdater();
        result = updater.enter(visitable);
        if (!result)
            return result;

        return result;
    }


    /**
     * Decompile "Iter" element in XcodeML/C.
     */
    @Override
    public boolean enter(Iter visitable) {
        // DONE: Iter
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        DbUpdater updater = _context.getDbUpdater();
        result = updater.enter(visitable);
        if (!result)
            return result;

        return result;
    }

    /**
     * Decompile "PostIncrExpr" element in XcodeML/C.
     * (example)   a++;
     */
    @Override
    public boolean enter(PostIncrExpr visitable) {
        // DONE: PostIncrExpr
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        // 1つ上のノードがExprStatementの場合のみDB更新
        if (!_context.isInvokeNodeOf(ExprStatement.class, 1)) {
            return true;
        }

        writer.updateCodeLine();
        DbUpdater updater = _context.getDbUpdater();
        result = updater.enter(visitable);
        if (!result) return result;

        return result;
    }

    /**
     * Decompile "PostDecrExpr" element in XcodeML/C.
     * (example)   a--;
     */
    @Override
    public boolean enter(PostDecrExpr visitable) {
        // DONE: PostDecrExpr
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        // 1つ上のノードがExprStatementの場合のみDB更新
        if (!_context.isInvokeNodeOf(ExprStatement.class, 1)) {
            return true;
        }

        writer.updateCodeLine();
        DbUpdater updater = _context.getDbUpdater();
        result = updater.enter(visitable);
        if (!result) return result;

        return result;
    }

    /**
     * Decompile "PreIncrExpr" element in XcodeML/C.
     * (example)   ++a;
     */
    @Override
    public boolean enter(PreIncrExpr visitable) {
        // DONE: PreIncrExpr
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        // 1つ上のノードがExprStatementの場合のみDB更新
        if (!_context.isInvokeNodeOf(ExprStatement.class, 1)) {
            return true;
        }

        writer.updateCodeLine();
        DbUpdater updater = _context.getDbUpdater();
        result = updater.enter(visitable);
        if (!result) return result;

        return result;
    }

    /**
     * Decompile "PreDecrExpr" element in XcodeML/C.
     * (example)   --a;
     */
    @Override
    public boolean enter(PreDecrExpr visitable) {
        // DONE: PreDecrExpr
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result)
            return result;

        // 1つ上のノードがExprStatementの場合のみDB更新
        if (!_context.isInvokeNodeOf(ExprStatement.class, 1)) {
            return true;
        }

        writer.updateCodeLine();
        DbUpdater updater = _context.getDbUpdater();
        result = updater.enter(visitable);
        if (!result) return result;

        return result;
    }

    /**
     * Decompile "CastExpr" element in XcodeML/C.
     * (example)   --a;
     */
    @Override
    public boolean enter(CastExpr visitable) {
        // DONE: CastExpr
        boolean result = true;
        CodeBuilder writer = _context.getCodeBuilder();
        result = writer.enter(visitable);
        if (!result) return result;

        return result;
    }

}
