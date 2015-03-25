/*
 * K-scope
 * Copyright 2012-2015 RIKEN, Japan
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
package jp.riken.kscope.xcodeml.clang;

import java.io.File;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.language.Expression;
import jp.riken.kscope.language.Variable;
import jp.riken.kscope.language.VariableDefinition;
import jp.riken.kscope.language.fortran.VariableType;
import jp.riken.kscope.utils.FileUtils;
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.xcodeml.EnumError;
import jp.riken.kscope.xcodeml.clang.XcodeMLContext;
import jp.riken.kscope.xcodeml.clang.XcodeMLVisitorImpl;
import jp.riken.kscope.xcodeml.clang.XcodeMLVisitor;
import jp.riken.kscope.xcodeml.clang.parser.ExpressionParser;
import jp.riken.kscope.xcodeml.clang.parser.VariableDefinitionParser;
import jp.riken.kscope.xcodeml.clang.parser.VariableParser;
import jp.riken.kscope.xcodeml.clang.parser.VariableTypeParser;
import jp.riken.kscope.xcodeml.clang.utils.XmlNodeUtil;
import jp.riken.kscope.xcodeml.clang.xml.IBaseStatement;
import jp.riken.kscope.xcodeml.clang.xml.IXmlTypeTableChoice;
import jp.riken.kscope.xcodeml.clang.xml.gen.AddrOfExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.Arguments;
import jp.riken.kscope.xcodeml.clang.xml.gen.ArrayAddr;
import jp.riken.kscope.xcodeml.clang.xml.gen.ArrayRef;
import jp.riken.kscope.xcodeml.clang.xml.gen.ArrayType;
import jp.riken.kscope.xcodeml.clang.xml.gen.AsgBitAndExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.AsgBitOrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.AsgBitXorExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.AsgDivExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.AsgLshiftExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.AsgMinusExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.AsgModExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.AsgMulExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.AsgPlusExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.AsgRshiftExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.AssignExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.BaseType;
import jp.riken.kscope.xcodeml.clang.xml.gen.BasicType;
import jp.riken.kscope.xcodeml.clang.xml.gen.BitAndExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.BitNotExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.BitOrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.BitXorExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.BreakStatement;
import jp.riken.kscope.xcodeml.clang.xml.gen.CaseLabel;
import jp.riken.kscope.xcodeml.clang.xml.gen.CastExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.CommaExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.CompoundStatement;
import jp.riken.kscope.xcodeml.clang.xml.gen.CondExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.Condition;
import jp.riken.kscope.xcodeml.clang.xml.gen.ContinueStatement;
import jp.riken.kscope.xcodeml.clang.xml.gen.DefaultLabel;
import jp.riken.kscope.xcodeml.clang.xml.gen.DivExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.DoStatement;
import jp.riken.kscope.xcodeml.clang.xml.gen.Else;
import jp.riken.kscope.xcodeml.clang.xml.gen.EnumType;
import jp.riken.kscope.xcodeml.clang.xml.gen.ExprStatement;
import jp.riken.kscope.xcodeml.clang.xml.gen.FloatConstant;
import jp.riken.kscope.xcodeml.clang.xml.gen.ForStatement;
import jp.riken.kscope.xcodeml.clang.xml.gen.Function;
import jp.riken.kscope.xcodeml.clang.xml.gen.FunctionCall;
import jp.riken.kscope.xcodeml.clang.xml.gen.FunctionDefinition;
import jp.riken.kscope.xcodeml.clang.xml.gen.GotoStatement;
import jp.riken.kscope.xcodeml.clang.xml.gen.Id;
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
import jp.riken.kscope.xcodeml.clang.xml.gen.LshiftExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.MemberAddr;
import jp.riken.kscope.xcodeml.clang.xml.gen.MemberArrayAddr;
import jp.riken.kscope.xcodeml.clang.xml.gen.MemberArrayRef;
import jp.riken.kscope.xcodeml.clang.xml.gen.MemberRef;
import jp.riken.kscope.xcodeml.clang.xml.gen.MinusExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.ModExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.MoeConstant;
import jp.riken.kscope.xcodeml.clang.xml.gen.MulExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.Name;
import jp.riken.kscope.xcodeml.clang.xml.gen.PlusExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.PointerRef;
import jp.riken.kscope.xcodeml.clang.xml.gen.PointerType;
import jp.riken.kscope.xcodeml.clang.xml.gen.PostDecrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.PostIncrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.Pragma;
import jp.riken.kscope.xcodeml.clang.xml.gen.PreDecrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.PreIncrExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.ReturnStatement;
import jp.riken.kscope.xcodeml.clang.xml.gen.RshiftExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.SizeOfExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.StringConstant;
import jp.riken.kscope.xcodeml.clang.xml.gen.StructType;
import jp.riken.kscope.xcodeml.clang.xml.gen.SwitchStatement;
import jp.riken.kscope.xcodeml.clang.xml.gen.UnaryMinusExpr;
import jp.riken.kscope.xcodeml.clang.xml.gen.UnionType;
import jp.riken.kscope.xcodeml.clang.xml.gen.Value;
import jp.riken.kscope.xcodeml.clang.xml.gen.VarAddr;
import jp.riken.kscope.xcodeml.clang.xml.gen.VarDecl;
import jp.riken.kscope.xcodeml.clang.xml.gen.WhileStatement;
import jp.riken.kscope.xcodeml.clang.xml.IXmlNode;
import jp.riken.kscope.xcodeml.clang.xml.gen.Params;
import jp.riken.kscope.xcodeml.clang.xml.gen.Var;

/**
 * ソースコード生成クラス
 * @author RIKEN
 */
public class CodeBuilder extends XcodeMLVisitorImpl {

    /** スペース文字 */
    public static final String DEFAULT_SPC_CHARS = " ";
    /** インデント文字列 */
    public static final String DEFAULT_INDENT_CHARS = " ";
    /** テキスト出力ストリーム */
    private PrintWriter _out;
    /** コード行バッファ */
    private StringBuilder m_lineBuf = new StringBuilder();
    /** コード行リスト */
    private ArrayList<CodeLine> m_sourceList = new ArrayList<CodeLine>();
    /** XMLパース実行状態 */
    private XcodeMLContext _context;
    /** ダミー実行用バッファー（文字列の組み立て機能のみ利用） */
    private StringBuilder m_dummyBuf = null;

    /**
     * コンストラクタ
     * @param context		XcodeMLパーサ実行状況クラス
     */
    public CodeBuilder(XcodeMLContext context) {
        _context = context;
        _out = null;
    }

    /**
     * ソースコード行を取得する
     * @return		コード行リスト
     */
    public CodeLine[] getCodeLineList() {
        if (m_sourceList == null || m_sourceList.size() <= 0)
            return null;
        return (CodeLine[]) m_sourceList.toArray(new CodeLine[0]);
    }

    /**
     * Flush writer.
     */
    public void flush() {
        if (_out != null) {
            _out.flush();
        }
    }

    /**
     * Close writer.
     */
    public void close() {
        if (_out != null) {
            _out.close();
        }
    }

    /**
     * コード行バッファをクリアする
     */
    public void clearLineBuffer() {
        this.m_lineBuf = new StringBuilder();
        this.m_dummyBuf = null;
    }

    @Override
    public boolean enter(VarDecl visitable) {
        XcodeMLVisitor visiter = _context.getVisitor();

        // DONE: VarDecl
        writeLineDirective(visitable.getLineno(), visitable.getFile());

        // 変数宣言文
        VariableDefinitionParser varParser = new VariableDefinitionParser(this._context.getTypeManager(), null);
        VariableDefinition varDef = varParser.parseVariableDefinition(visitable);
        if (varDef == null) return true;
        writeToken(varDef.toStringClang());

        Value valueElem = visitable.getValue();
        if (valueElem != null) {
            writeToken(" = ");
            if (visiter.invokeEnter(valueElem) == false) {
                return false;
            }
        }

        updateCodeLine(visitable);

        return true;
    }

    /**
     * 変数宣言文を出力する。
     * @param visitable		変数宣言文要素:VarDecl
     * @return		true=成功
     */
    private boolean writeVarDecl(VarDecl visitable) {
        if (visitable == null) return false;

        XcodeMLVisitor visiter = _context.getVisitor();
        XcodeMLTypeManager typeManager = _context.getTypeManager();

        String name = null;
        Id symbol = null;
        Name nameElem = visitable.getName();
        if (nameElem != null) name = nameElem.getValue();
        if (name != null) {
            // シンボルの検索
            symbol = typeManager.findSymbol(name);
        }
        if (symbol == null) {
            _context.setLastErrorMessage(XmlNodeUtil.formatError(nameElem,
                    EnumError.XCODEML_NAME_NOT_FOUND, nameElem.getValue()));
            return false;
        }

        _writeSymbol(symbol);

        return true;
    }

    /**
     * データID要素からデータ型を出力する。
     * @param types		データID要素
     * @return		true=成功
     */
    private boolean _writeSymbol(Id symbol) {
        if (symbol == null) return false;
        if (symbol.getName() == null) return false;

        // 変数名
        String symbol_name = symbol.getName().getValue();

        // データ型の取得
        XcodeMLTypeManager typeManager = _context.getTypeManager();
        IXmlTypeTableChoice[] types = typeManager.findTypes(symbol);

        if (types == null) {
            // シンボルからのみ組み立てる。
            _writePrimitiveVarDecl(symbol);
            return true;
        }


        IXmlTypeTableChoice top_type = types[0];
        IXmlTypeTableChoice last_type = types[types.length-1];

        // const,volatile,restrict,pointer
        boolean is_pointer = false;
        boolean is_const = false;
        boolean is_volatile = false;
        boolean is_restrict = false;
        for (IXmlTypeTableChoice type : types) {
            if (type instanceof PointerType) {
                is_pointer = true;
            }
            BaseType base_type = (BaseType)type;
            if (StringUtils.toBoolean(base_type.getIsConst()))  is_const = true;
            if (StringUtils.toBoolean(base_type.getIsVolatile()))  is_volatile = true;
            if (StringUtils.toBoolean(base_type.getIsRestrict()))  is_restrict = true;
        }

        // 'extern','static', 'register'のみ出力する.
        _writeSclass(symbol);

        // const
        if (is_const) {
            writeToken("const");
            writeToken(" ");
        }
        // volatile
        if (is_volatile) {
            writeToken("volatile");
            writeToken(" ");
        }

        // struct, union, enum
        String struct_type = null;
        if (last_type instanceof StructType) struct_type = "struct";
        else if (last_type instanceof UnionType) struct_type = "union";
        else if (last_type instanceof EnumType) struct_type = "enum";
        if (struct_type != null) {
            writeToken(struct_type);
            writeToken(" ");
            // タグ名の検索
            String tag_type = last_type.getType();
            Id tagname = typeManager.findTagname(tag_type);
            if (tagname != null && tagname.getName() != null) {
                writeToken(tagname.getName().getValue());
                writeToken(" ");
            }
        }

        // restrict
        if (is_restrict) {
            writeToken("restrict");
            writeToken(" ");
        }

        // pointer
        if (is_pointer) {
            writeToken("*");
        }

        // データ型
        if (last_type instanceof BasicType) {
            String data_type = ((BasicType)last_type).getName();
            writeToken(data_type);
            writeToken(" ");
        }

        // 変数名
        writeToken(symbol_name);

        // 配列
        for (IXmlTypeTableChoice type : types) {
            if (type instanceof ArrayType) {
                String array_size = ((ArrayType)type).getArraySizeAttribute();
                if (StringUtils.isNullOrEmpty(array_size)) {
                    writeToken("[");
                    writeToken(array_size);
                    writeToken("]");
                }
            }
        }

        return true;
    }


    /**
     * Write simple primitive symbol declaration.
     *
     * @param symbol
     *            Instance of XmlSymbol.
     */
    private boolean _writePrimitiveVarDecl(Id symbol) {
        if (symbol == null) return false;
        if (symbol.getName() == null) return  false;

        // 'extern','static', 'register'のみ出力する.
        _writeSclass(symbol);
        // データ型
        String type = symbol.getType();
        String c_type = EnumPrimitiveType.getClangDataType(type);

        writeToken(c_type);
        writeToken(" ");
        writeToken(symbol.getName().getValue());

        return true;
    }

    /**
     * シンボルのsclassを出力する.
     * 'extern','static', 'register'のみ出力する.
     * @param symbol	シンボル
     */
    private boolean _writeSclass(Id symbol) {
        if (symbol == null) return false;
        String sclass = symbol.getSclass();
        // 'auto', 'param', 'extern', 'extern_def', 'static', 'register', 'label', 'tagname', 'moe', 'typedef_name'
        // 'extern','static', 'register'のみ出力する.
        boolean is_write = false;
        if ("extern".equals(sclass)) is_write = true;
        else if ("static".equals(sclass)) is_write = true;
        else if ("register".equals(sclass)) is_write = true;
        if (is_write) {
            writeToken(sclass);
            writeToken(" ");
        }

        return true;
    }

    /**
     * Write line directive.
     *
     * @param lineNumber
     *            Line number.
     * @param filePath
     *            File path.
     */
    public void writeLineDirective(String lineNumber, String filePath) {
        if (_context.isOutputLineDirective() && lineNumber != null) {
            if (filePath == null)
                writeIsolatedLine(String.format("# %s", lineNumber));
            else
                writeIsolatedLine(String.format("# %s \"%s\"", lineNumber,
                        filePath));
        }
    }

    /**
     * 行番号, FpragmaStatement出力を行う。
     *
     * コード行クラスには追加しない。 デバッグ用のファイル出力のみを行う。
     *
     * @param s
     *            String to write
     */
    public void writeIsolatedLine(String s) {
        // Note: Omit count up of _columnNumber
        if (_out != null) {
            _out.println(s);
        }
        updateCodeLine();
    }

    /**
     * Setup new line.
     */
    public void updateCodeLine() {
        updateCodeLine(null);
    }

    /**
     * コード行を出力する。
     *
     * @param  node    出力ノード
     * @return  成否
     */
    public boolean updateCodeLine(IXmlNode node) {
        // ダミー実行中は更新しない。
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

//        _context.setLastErrorMessage("fault:updateCodeLine class=" + node.getClass().getName());
//        return false;
        return true;

    }


    /**
     * コード行を出力する。
     *
     * @param  node    出力ノード
     * @param filename			ソースファイル名
     * @param startlineno		開始行番号
     * @param endlineno			終了行番号
     * @return  成否
     */
    private boolean updateCodeLine(
                        IXmlNode node,
                        String filename,
                        String startlineno,
                        String endlineno) {

        // ダミー実行中は更新しない。
        if (m_dummyBuf != null) {
            return true;
        }
        if (m_lineBuf.length() > 0) {
            if (_out != null) {
                _out.println(m_lineBuf);
                flush();
            }
            CodeLine code = this.createCodeLine(
                                    filename,
                                    startlineno,
                                    endlineno,
                                    m_lineBuf.toString());
            assert (code != null);

            m_sourceList.add(code);
            m_lineBuf = new StringBuilder();

            return true;
        }

        return true;

    }

    /**
     * コード行クラスを生成する。
     *
     * @param filename			ソースファイル名
     * @param startlineno		開始行番号
     * @param endlineno			終了行番号
     * @param statement			コードステートメント
     * @return			生成コード行クラス
     */
    private CodeLine createCodeLine(String filename, String startlineno,
            String endlineno, String statement) {

        // 削除 XMLからのパースで開始行番号がない要素(VarDecl)がある。
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

        // ソースファイル、ソースコード行
        File srcFile = FileUtils.joinFilePath(_context.getSourceXmlFile()
                .getFile().getParentFile(), filename);
        SourceFile codeFile = null;
        if (srcFile != null) {
            if (_context.getBaseFolder() != null) {
                String path = FileUtils.getRelativePath(srcFile, _context.getBaseFolder());
                if (path != null) {
                    codeFile = new SourceFile(path);
                }
            }
            else {
                codeFile = new SourceFile(srcFile);
            }
        }

        CodeLine lineInfo = new CodeLine(codeFile, statement, start_no, end_no, filename);

        return lineInfo;
    }

    /**
     * コード行クラスを生成する。
     *
     * @param node			XMLノード
     * @return コード行クラス
     */
    public CodeLine createCodeLine(IXmlNode node) {
        // 現在要素の親要素からファイル名、開始行番号、終了行番号を取得する。
        IBaseStatement base_node = _context.getInvokeBaseStatement(node);
        if (base_node == null)
            return null;

        String filename = base_node.getFile();
        String startlineno = base_node.getLineno();
        String endlineno = base_node.getEndlineno();

        return createCodeLine(filename, startlineno, endlineno,
                m_lineBuf.toString());
    }


    /**
     * コード行クラスを生成する。
     *
     * @param node		XMLノード
     * @param line		コード行文字列
     * @return コード行クラス
     */
    public CodeLine createCodeLine(IXmlNode node, String line) {
        // 現在要素の親要素からファイル名、開始行番号、終了行番号を取得する。
        IBaseStatement base_node = _context.getInvokeBaseStatement(node);
        if (base_node == null)
            return null;

        String filename = base_node.getFile();
        String startlineno = base_node.getLineno();
        String endlineno = base_node.getEndlineno();

        if (filename == null)
            return null;

        return createCodeLine(filename, startlineno, endlineno, line);
    }

    /**
     * Write in the designated string as token.
     *
     * @param s
     *            String to write
     */
    public void writeToken(String s) {
        if (s == null || s.length() == 0) {
            return;
        }

        _writeCharacterArray(s.toCharArray());
    }


    /**
     * 文字リストを出力する.
     * @param chArray		文字リスト
     */
    private void _writeCharacterArray(char[] chArray) {
        if (m_dummyBuf != null) {
            // ダミー実行中
            m_dummyBuf.insert(m_dummyBuf.length(), chArray, 0, chArray.length);
        } else {
            // 通常実行中
            m_lineBuf.insert(m_lineBuf.length(), chArray, 0, chArray.length);
        }
    }

    /**
     * 最終コード行を取得する。
     * @return		最終コード行
     */
    public CodeLine getLastCodeLine() {
        if (m_sourceList.size() == 0)
            return null;
        return m_sourceList.get(m_sourceList.size() - 1);
    }

    /**
     * Decompile "FfunctionDefinition" element in XcodeML/C.
     */
    @Override
    public boolean enter(FunctionDefinition visitable) {
        XcodeMLTypeManager typeManager = _context.getTypeManager();
        XcodeMLVisitor visiter = _context.getVisitor();

        // DONE: FunctionDefinition
        writeLineDirective(visitable.getLineno(), visitable.getFile());

        String func_name = null;
        if (visitable.getName() != null) {
            func_name = visitable.getName().getValue();
        }
        if (func_name == null) return false;

        Id func_id = typeManager.findSymbol(func_name);
        _writeSymbol(func_id);
        writeToken("(");

        if (visiter.invokeEnter(visitable.getParams()) == false) {
            return false;
        }
        writeToken(")");

        if (!updateCodeLine(visitable)) {
            return false;
        }
        return true;
    }

    /**
     * Decompile "params" element in XcodeML/C.
     */
    @Override
    public boolean enter(Params visitable) {
        // DONE: Params
        XcodeMLTypeManager typeManager = _context.getTypeManager();
        int paramCount = 0;
        for (Name nameElem : visitable.getName()) {
            if (nameElem == null) continue;
            if (paramCount > 0) {
                writeToken(", ");
            }
            String name = nameElem.getValue();

            Id func_id = typeManager.findSymbol(name);
            _writeSymbol(func_id);
            writeToken(" ");
            writeToken(nameElem.getValue());

            ++paramCount;
        }

        return true;
    }

    @Override
    public void leave(FunctionDefinition visitable) {

        XcodeMLTypeManager typeManager = _context.getTypeManager();
        typeManager.leaveScope();
        writeToken("}");
        updateCodeLine(visitable);

    }

    /**
     * Decompile "DoStatement" element in XcodeML/C.
     */
    @Override
    public boolean enter(DoStatement visitable) {
        XcodeMLVisitor visiter = _context.getVisitor();
        // DONE: FdoStatement
        writeLineDirective(visitable.getLineno(), visitable.getFile());

        writeToken("do...while");
        writeToken("(");

        // condition
        if (visiter.invokeEnter(visitable.getCondition()) == false) {
            return false;
        }

        writeToken(")");
        if (!updateCodeLine(visitable)) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "FifStatement" element in XcodeML/C.
     */
    @Override
    public boolean enter(IfStatement visitable) {
        XcodeMLVisitor visiter = _context.getVisitor();
        // DONE: FifStatement
        writeLineDirective(visitable.getLineno(), visitable.getFile());

        // if文
        writeToken("if ");

        writeToken("(");
        // 条件式
        if (visiter.invokeEnter(visitable.getCondition()) == false) {
            return false;
        }
        writeToken(")");

        if (!updateCodeLine(visitable)) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "Condition" element in XcodeML/C.
     */
    @Override
    public boolean enter(Condition visitable) {
        XcodeMLVisitor visiter = _context.getVisitor();
        // DONE: Condition

        IXmlNode node = XmlNodeUtil.getXmlNodeChoice(visitable);
        if (visiter.invokeEnter(node) == false) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "Else" element in XcodeML/C.
     */
    @Override
    public boolean enter(Else visitable) {
        if (visitable != null) {
            // 子要素を取得する
            IXmlNode node = XmlNodeUtil.getXmlNodeChoice(visitable);
            String filename = null;
            String start_lineno = null;
            String end_lineno = null;
            if (node instanceof IBaseStatement) {
                filename = ((IBaseStatement)node).getFile();
                start_lineno = ((IBaseStatement)node).getLineno();
                end_lineno = ((IBaseStatement)node).getEndlineno();
            }
            writeToken("else");

            if (filename == null) {
                if (!updateCodeLine(visitable)) {
                    return false;
                }
            }
            else {
                if (!updateCodeLine(visitable, filename, start_lineno, end_lineno)) {
                    return false;
                }
            }
        }

        return true;
    }

    /**
     * Decompile "ForStatement" element in XcodeML/C.
     */
    @Override
    public boolean enter(ForStatement visitable) {
        XcodeMLVisitor visiter = _context.getVisitor();
        // DONE: ForStatement
        writeLineDirective(visitable.getLineno(), visitable.getFile());

        writeToken("for ");
        writeToken("(");

        // init
        if (visiter.invokeEnter(visitable.getInit()) == false) {
            return false;
        }
        writeToken("; ");
        // condition
        if (visiter.invokeEnter(visitable.getCondition()) == false) {
            return false;
        }
        writeToken("; ");
        // iterator
        if (visiter.invokeEnter(visitable.getIter()) == false) {
            return false;
        }

        writeToken(")");
        if (!updateCodeLine(visitable)) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "ExprStatement" element in XcodeML/C.
     */
    @Override
    public boolean enter(ExprStatement visitable) {
        XcodeMLVisitor visiter = _context.getVisitor();
        // DONE: ExprStatement
        writeLineDirective(visitable.getLineno(), visitable.getFile());

        IXmlNode node = XmlNodeUtil.getXmlNodeChoice(visitable);
        if (visiter.invokeEnter(node) == false) {
            return false;
        }

        if (!updateCodeLine(visitable)) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "AssignExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(AssignExpr visitable) {
        // DONE: AssignExpr
        List<IXmlNode> content = visitable.getContent();
        // 左辺
        IXmlNode leftExpr = (content != null && content.size() >= 1) ? content.get(0) : null;
        // 右辺
        IXmlNode rightExpr = (content != null && content.size() >= 2) ? content.get(1) : null;

        assert (leftExpr != null && rightExpr != null); // 左辺、右辺は必須

        if (writeBinaryExpr(leftExpr, rightExpr, "=", true) == false) {
            return false;
        }

        return true;
    }

    /**
     * Write binary expression.
     *
     * @param leftExpr
     *            Instance of IDefModelExprChoice which expresses a Lvalue.
     * @param rightExpr
     *            Instance of IDefModelExprChoice which expresses a Rvalue.
     * @param operation
     *            Operation string.
     * @param grouping
     *            Grouping flag.
     * @return true/false
     */
    public boolean writeBinaryExpr(IXmlNode leftExpr, IXmlNode rightExpr,
            String operation, boolean grouping) {
        XcodeMLVisitor visiter = _context.getVisitor();

        // 1つ上の親要素を取得する。
        boolean parentheses = this.needParentheses(operation);

        if (grouping && parentheses) {
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

        if (grouping && parentheses) {
            writeToken(")");
        }

        return true;
    }

    /**
     * Write unary expression.
     *
     * @param expr
     *            Instance of IDefModelExprChoice.
     * @param operation
     *            Operation string.
     * @param grouping
     *            Grouping flag.
     * @return true/false
     */
    public boolean writeUnaryExpr(IXmlNode expr, String operation,
            boolean grouping) {
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
     * 括弧が必要であるかチェックする
     * @param   operation		オペレータ文字列
     * @return		true = 括弧が必要
     */
    private boolean needParentheses(String operation) {

        // 1つ上の親要素を取得する。
        IXmlNode parentNode = _context.getInvokeNode(1);
        boolean isStatement = XmlNodeUtil.isStatement(parentNode);
        if (isStatement) return false;

        IXmlNode parentNode2 = _context.getInvokeNode(2);
        isStatement = XmlNodeUtil.isStatement(parentNode2);
        if (isStatement) return false;

        return true;
    }

    /**
     * Decompile "StringConstant" element in XcodeML/C.
     */
    @Override
    public boolean enter(StringConstant visitable) {
        // DONE: StringConstant
        String content = visitable.getValue();
        content = "\"" + content + "\"";
        writeToken(content);

        return true;
    }

    /**
     * Decompile "IntConstant" element in XcodeML/C.
     */
    @Override
    public boolean enter(IntConstant visitable) {
        // DONE: IntConstant
        String content = visitable.getValue();
        writeToken(content);

        return true;
    }

    /**
     * Decompile "FloatConstant" element in XcodeML/C.
     */
    @Override
    public boolean enter(FloatConstant visitable) {
        // DONE: FloatConstant
        String content = visitable.getValue();
        writeToken(content);

        return true;
    }

    /**
     * Decompile "LonglongConstant" element in XcodeML/C.
     */
    @Override
    public boolean enter(LonglongConstant visitable) {
        // DONE: LonglongConstant
        String content = visitable.getValue();
        writeToken(content);

        return true;
    }

    /**
     * Decompile "MoeConstant" element in XcodeML/C.
     */
    @Override
    public boolean enter(MoeConstant visitable) {
        // DONE: MoeConstant
        String content = visitable.getValue();
        writeToken(content);

        return true;
    }

    /**
     * Decompile "plusExpr" element in XcodeML/C.
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
     * Decompile "ModExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(ModExpr visitable) {
        // DONE: ModExpr
        List<IXmlNode> list = visitable.getContent();
        IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
        IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
        if (writeBinaryExpr(leftExpr, rightExpr, "%", true) == false) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "MinusExpr" element in XcodeML/C.
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
     * Decompile "MulExpr" element in XcodeML/C.
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
     * Decompile "DivExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(DivExpr visitable) {
        // DONE: DivExpr
        List<IXmlNode> list = visitable.getContent();
        IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
        IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
        if (writeBinaryExpr(leftExpr, rightExpr, "/", true) == false) {
            return false;
        }

        return true;
    }


    /**
     * Decompile "Var" element in XcodeML/C.
     */
    @Override
    public boolean enter(Var visitable) {
        // DONE: Var
        writeToken(visitable.getValue());

        return true;
    }


    /**
     * Decompile "DoStatement" element in XcodeML/C.
     */
    @Override
    public void leave(DoStatement visitable) {
        writeToken("}");

        updateCodeLine(visitable);

        return;
    }

    /**
     * Decompile "ExprStatement" element in XcodeML/C.
     */
    @Override
    public void leave(ExprStatement visitable) {
        this.clearLineBuffer();
        return;
    }

    /**
     * Decompile "FunctionCall" element in XcodeML/C.
     */
    @Override
    public boolean enter(FunctionCall visitable) {
        // DONE: FunctionCall
        XcodeMLVisitor visiter = _context.getVisitor();
        Function function = visitable.getFunction();
        if (function == null) return false;

        String functionName = null;
        if (function.getFuncAddr() != null) {
            functionName = function.getFuncAddr().getValue();

            writeToken(functionName);
        }
        else {
            IXmlNode node = XmlNodeUtil.getXmlNodeChoice(function);
            if (visiter.invokeEnter(node) == false) {
                return false;
            }
        }

        writeToken("(");

        if (visiter.invokeEnter(visitable.getArguments()) == false) {
            return false;
        }
        writeToken(")");
        
        return true;
    }

    /**
     * Decompile "Arguments" element in XcodeML/C.
     */
    @Override
    public boolean enter(Arguments visitable) {
        XcodeMLVisitor visiter = _context.getVisitor();
        boolean result = true;
        List<IXmlNode> expressions = visitable.getExpressions();
        if (expressions == null) return true;

        for (int i=0; i<expressions.size(); i++) {
            if (visiter.invokeEnter(expressions.get(i)) == false) {
                return false;
            }
            if (i<expressions.size()-1) {
                writeToken(", ");
            }
        }

        return true;
    }


    /**
     * Decompile "LogOrExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(LogOrExpr visitable) {
        // DONE: LogOrExpr
        List<IXmlNode> list = visitable.getContent();
        IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
        IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
        if (writeBinaryExpr(leftExpr, rightExpr, "||", true) == false) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "LogGTExpr" element in XcodeML/C.
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
     * Decompile "LogEQExpr" element in XcodeML/C.
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
     * Decompile "LogNEQExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(LogNEQExpr visitable) {
        // DONE: LogNEQExpr
        List<IXmlNode> list = visitable.getContent();
        IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
        IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
        if (writeBinaryExpr(leftExpr, rightExpr, "!=", true) == false) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "LogGEExpr" element in XcodeML/C.
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
     * Decompile "LogLTExpr" element in XcodeML/C.
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
     * Decompile "LogLEExpr" element in XcodeML/C.
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
     * Decompile "LogAndExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(LogAndExpr visitable) {
        // DONE: LogAndExpr
        List<IXmlNode> list = visitable.getContent();
        IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
        IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
        if (writeBinaryExpr(leftExpr, rightExpr, "&&", true) == false) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "LogNotExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(LogNotExpr visitable) {
        // DONE: LogNotExpr
        IXmlNode node = XmlNodeUtil.getXmlNodeChoice(visitable);
        // NOTは優先順位が高いので()は付けない
        if (writeUnaryExpr(node, "!", false) == false) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "Init" element in XcodeML/C.
     */
    @Override
    public boolean enter(Init visitable) {
        // DONE: Init
        XcodeMLVisitor visiter = _context.getVisitor();

        IXmlNode node = XmlNodeUtil.getXmlNodeChoice(visitable);
        if (visiter.invokeEnter(node) == false) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "Iter" element in XcodeML/C.
     */
    @Override
    public boolean enter(Iter visitable) {
        // DONE: Iter
        XcodeMLVisitor visiter = _context.getVisitor();

        IXmlNode node = XmlNodeUtil.getXmlNodeChoice(visitable);
        if (visiter.invokeEnter(node) == false) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "PostIncrExpr" element in XcodeML/C.
     * (example)   a++;
     */
    @Override
    public boolean enter(PostIncrExpr visitable) {
        // DONE: PostIncrExpr
        XcodeMLVisitor visiter = _context.getVisitor();
        IXmlNode node = XmlNodeUtil.getXmlNodeChoice(visitable);
        String operation = "++";

        if (visiter.invokeEnter(node) == false) {
            return false;
        }
        writeToken(operation);

        return true;
    }

    /**
     * Decompile "PostDecrExpr" element in XcodeML/C.
     * (example)   a--;
     */
    @Override
    public boolean enter(PostDecrExpr visitable) {
        // DONE: PostDecrExpr
        XcodeMLVisitor visiter = _context.getVisitor();
        IXmlNode node = XmlNodeUtil.getXmlNodeChoice(visitable);
        String operation = "--";

        if (visiter.invokeEnter(node) == false) {
            return false;
        }
        writeToken(operation);

        return true;
    }

    /**
     * Decompile "PreIncrExpr" element in XcodeML/C.
     * (example)   ++a;
     */
    @Override
    public boolean enter(PreIncrExpr visitable) {
        // DONE: PreIncrExpr
        XcodeMLVisitor visiter = _context.getVisitor();
        IXmlNode node = XmlNodeUtil.getXmlNodeChoice(visitable);
        String operation = "++";

        writeToken(operation);
        if (visiter.invokeEnter(node) == false) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "PreDecrExpr" element in XcodeML/C.
     * (example)   --a;
     */
    @Override
    public boolean enter(PreDecrExpr visitable) {
        // DONE: PreDecrExpr
        XcodeMLVisitor visiter = _context.getVisitor();
        IXmlNode node = XmlNodeUtil.getXmlNodeChoice(visitable);
        String operation = "--";

        writeToken(operation);
        if (visiter.invokeEnter(node) == false) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "CastExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(CastExpr visitable) {
        // DONE: CastExpr
        XcodeMLVisitor visiter = _context.getVisitor();

        // キャストデータ型
        String type = visitable.getType();
        // キャストデータ
        IXmlNode node = XmlNodeUtil.getXmlNodeChoice(visitable);

        // 変数データ型のパース
        VariableTypeParser typeParser = new VariableTypeParser(_context.getTypeManager(), null);
        VariableType varType = typeParser.parseVariableType(type);
        String var_str = varType.toStringClang();

        // NULL表現であるかチェックする
        if ("void*".equals(var_str) && visitable.getIntConstant() != null) {
            String value = visitable.getIntConstant().getValue();
            if ("0".equals(value)) {
                // NULLである
                this.writeToken("NULL");
                return true;
            }
        }

        this.writeToken("(");
        // Var:式文字列
        this.writeToken(var_str);
        this.writeToken(")");

        // キャストデータ
        if (visiter.invokeEnter(node) == false) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "SwitchStatement" element in XcodeML/C.
     */
    @Override
    public boolean enter(SwitchStatement visitable) {

        XcodeMLVisitor visiter = _context.getVisitor();

        // DONE: SwitchStatement
        writeLineDirective(visitable.getLineno(), visitable.getFile());


        writeToken("switch ");
        writeToken("(");
        if (visiter.invokeEnter(visitable.getValue()) == false) {
            return false;
        }

        writeToken(")");

        if (!updateCodeLine(visitable)) {
            return false;
        }
        return true;
    }

    /**
     * Decompile "CaseLabel" element in XcodeML/C.
     */
    @Override
    public boolean enter(CaseLabel visitable) {
        XcodeMLVisitor visiter = _context.getVisitor();

        // DONE: SwitchStatement
        writeLineDirective(visitable.getLineno(), visitable.getFile());

        writeToken("case ");
        if (visiter.invokeEnter(visitable.getValue()) == false) {
            return false;
        }

        writeToken(":");

        if (!updateCodeLine(visitable)) {
            return false;
        }
        return true;
    }

    /**
     * Decompile "IfStatement" element in XcodeML/C.
     */
    @Override
    public void leave(IfStatement visitable) {
        writeToken("}");
        updateCodeLine(visitable);
    }

    /**
     * Decompile "SwitchStatement" element in XcodeML/C.
     */
    @Override
    public void leave(SwitchStatement visitable) {
        writeToken("}");
        updateCodeLine(visitable);
    }

    /**
     * Decompile "ForStatement" element in XcodeML/C.
     */
    @Override
    public void leave(ForStatement visitable) {
        writeToken("}");
        updateCodeLine(visitable);
    }

    /**
     * Decompile "DefaultLabel" element in XcodeML/C.
     */
    @Override
    public boolean enter(DefaultLabel visitable) {
        XcodeMLVisitor visiter = _context.getVisitor();

        // DONE: DefaultLabel
        writeLineDirective(visitable.getLineno(), visitable.getFile());

        writeToken("default:");

        if (!updateCodeLine(visitable)) {
            return false;
        }
        return true;
    }

    /**
     * Decompile "BreakStatement" element in XcodeML/C.
     */
    @Override
    public boolean enter(BreakStatement visitable) {
        XcodeMLVisitor visiter = _context.getVisitor();

        // DONE: BreakStatement
        writeLineDirective(visitable.getLineno(), visitable.getFile());

        writeToken("break");

        if (!updateCodeLine(visitable)) {
            return false;
        }
        return true;
    }

    /**
     * Decompile "ContinueStatement" element in XcodeML/C.
     */
    @Override
    public boolean enter(ContinueStatement visitable) {
        XcodeMLVisitor visiter = _context.getVisitor();

        // DONE: BreakStatement
        writeLineDirective(visitable.getLineno(), visitable.getFile());

        writeToken("continue");

        if (!updateCodeLine(visitable)) {
            return false;
        }
        return true;
    }

    /**
     * Decompile "WhileStatement" element in XcodeML/C.
     */
    @Override
    public boolean enter(WhileStatement visitable) {

        XcodeMLVisitor visiter = _context.getVisitor();
        // DONE: ForStatement
        writeLineDirective(visitable.getLineno(), visitable.getFile());

        writeToken("while ");
        writeToken("(");

        // condition
        if (visiter.invokeEnter(visitable.getCondition()) == false) {
            return false;
        }

        writeToken(")");
        if (!updateCodeLine(visitable)) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "WhileStatement" element in XcodeML/C.
     */
    @Override
    public void leave(WhileStatement visitable) {
        writeToken("}");
        updateCodeLine(visitable);
    }

    /**
     * Decompile "CompoundStatement" element in XcodeML/C.
     */
    @Override
    public boolean enter(CompoundStatement visitable) {
        XcodeMLVisitor visiter = _context.getVisitor();
        // DONE: ForStatement
        writeLineDirective(visitable.getLineno(), visitable.getFile());

        // 複文（空文）
        writeToken("{");
        if (!updateCodeLine(visitable)) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "CompoundStatement" element in XcodeML/C.
     */
    @Override
    public void leave(CompoundStatement visitable) {
        // 複文（空文）
        writeToken("}");
        updateCodeLine(visitable);
    }

    /**
     * Decompile "LshiftExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(LshiftExpr visitable) {
        // DONE: LshiftExpr
        List<IXmlNode> list = visitable.getContent();
        IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
        IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
        if (writeBinaryExpr(leftExpr, rightExpr, "<<", true) == false) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "RshiftExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(RshiftExpr visitable) {
        // DONE: RshiftExpr
        List<IXmlNode> list = visitable.getContent();
        IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
        IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
        if (writeBinaryExpr(leftExpr, rightExpr, ">>", true) == false) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "BitAndExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(BitAndExpr visitable) {
        // DONE: BitAndExpr
        List<IXmlNode> list = visitable.getContent();
        IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
        IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
        if (writeBinaryExpr(leftExpr, rightExpr, "&", true) == false) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "BitOrExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(BitOrExpr visitable) {
        // DONE: BitOrExpr
        List<IXmlNode> list = visitable.getContent();
        IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
        IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
        if (writeBinaryExpr(leftExpr, rightExpr, "|", true) == false) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "BitXorExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(BitXorExpr visitable) {
        // DONE: BitXorExpr
        List<IXmlNode> list = visitable.getContent();
        IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
        IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
        if (writeBinaryExpr(leftExpr, rightExpr, "^", true) == false) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "BitNotExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(BitNotExpr visitable) {
        // DONE: BitNotExpr
        IXmlNode node = XmlNodeUtil.getXmlNodeChoice(visitable);
        // NOTは優先順位が高いので()は付けない
        if (writeUnaryExpr(node, "~", false) == false) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "AsgPlusExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(AsgPlusExpr visitable) {
        // DONE: AsgPlusExpr
        List<IXmlNode> list = visitable.getContent();
        IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
        IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
        if (writeBinaryExpr(leftExpr, rightExpr, "+=", true) == false) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "AsgModExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(AsgModExpr visitable) {
        // DONE: AsgModExpr
        List<IXmlNode> list = visitable.getContent();
        IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
        IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
        if (writeBinaryExpr(leftExpr, rightExpr, "%=", true) == false) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "AsgBitOrExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(AsgBitOrExpr visitable) {
        // DONE: AsgBitOrExpr
        List<IXmlNode> list = visitable.getContent();
        IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
        IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
        if (writeBinaryExpr(leftExpr, rightExpr, "|=", true) == false) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "AsgBitXorExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(AsgBitXorExpr visitable) {
        // DONE: AsgBitXorExpr
        List<IXmlNode> list = visitable.getContent();
        IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
        IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
        if (writeBinaryExpr(leftExpr, rightExpr, "^=", true) == false) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "AsgMulExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(AsgMulExpr visitable) {
        // DONE: AsgMulExpr
        List<IXmlNode> list = visitable.getContent();
        IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
        IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
        if (writeBinaryExpr(leftExpr, rightExpr, "*=", true) == false) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "AsgRshiftExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(AsgRshiftExpr visitable) {
        // DONE: AsgRshiftExpr
        List<IXmlNode> list = visitable.getContent();
        IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
        IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
        if (writeBinaryExpr(leftExpr, rightExpr, ">>=", true) == false) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "AsgBitAndExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(AsgBitAndExpr visitable) {
        // DONE: AsgBitAndExpr
        List<IXmlNode> list = visitable.getContent();
        IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
        IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
        if (writeBinaryExpr(leftExpr, rightExpr, "&=", true) == false) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "AsgDivExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(AsgDivExpr visitable) {
        // DONE: AsgDivExpr
        List<IXmlNode> list = visitable.getContent();
        IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
        IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
        if (writeBinaryExpr(leftExpr, rightExpr, "/=", true) == false) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "AsgMinusExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(AsgMinusExpr visitable) {
        // DONE: AsgMinusExpr
        List<IXmlNode> list = visitable.getContent();
        IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
        IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
        if (writeBinaryExpr(leftExpr, rightExpr, "-=", true) == false) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "AsgLshiftExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(AsgLshiftExpr visitable) {
        // DONE: AsgLshiftExpr
        List<IXmlNode> list = visitable.getContent();
        IXmlNode leftExpr = list.size() >= 1 ? list.get(0) : null;
        IXmlNode rightExpr = list.size() >= 2 ? list.get(1) : null;
        if (writeBinaryExpr(leftExpr, rightExpr, "<<=", true) == false) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "unaryMinusExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(UnaryMinusExpr visitable) {
        // DONE: UnaryMinusExpr
        IXmlNode node = XmlNodeUtil.getXmlNodeChoice(visitable);
        if (writeUnaryExpr(node, "-", false) == false) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "SizeOfExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(SizeOfExpr visitable) {
        // DONE: SizeOfExpr
        XcodeMLVisitor visiter = _context.getVisitor();

        this.writeToken("sizeof(");

        // sizeof変数
        IXmlNode node = XmlNodeUtil.getXmlNodeChoice(visitable);
        if (visiter.invokeEnter(node) == false) {
            return false;
        }

        this.writeToken(")");

        return true;
    }

    /**
     * Decompile "CommaExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(CommaExpr visitable) {
        // DONE: CommaExpr
        XcodeMLVisitor visiter = _context.getVisitor();

        this.writeToken("(");

        // カンマ区切り式
        List<IXmlNode> expressions = visitable.getExpressions();
        if (expressions == null) return true;

        for (int i=0; i<expressions.size(); i++) {
            if (visiter.invokeEnter(expressions.get(i)) == false) {
                return false;
            }
            if (i<expressions.size()-1) {
                writeToken(", ");
            }
        }

        this.writeToken(")");

        return true;
    }

    /**
     * Decompile "CondExpr" element in XcodeML/C.
     */
    @Override
    public boolean enter(CondExpr visitable) {
        // DONE: CondExpr
        XcodeMLVisitor visiter = _context.getVisitor();

        // カンマ区切り式
        List<IXmlNode> content = visitable.getContent();
        if (content == null) return false;
        if (content.size() < 3) return false;

        if (visiter.invokeEnter(content.get(0)) == false) {
            return false;
        }
        this.writeToken(" ? ");
        if (visiter.invokeEnter(content.get(1)) == false) {
            return false;
        }
        this.writeToken(" : ");
        if (visiter.invokeEnter(content.get(2)) == false) {
            return false;
        }

        return true;
    }


    /**
     * Decompile "ArrayRef" element in XcodeML/C.
     */
    @Override
    public boolean enter(ArrayRef visitable) {
        // DONE: ArrayRef
        XcodeMLVisitor visiter = _context.getVisitor();

        ArrayAddr array_addr = visitable.getArrayAddrInArrayRef();
        List<IXmlNode> expressions = visitable.getExpressions();
        if (array_addr == null) return false;

        if (visiter.invokeEnter(array_addr) == false) {
            return false;
        }
        for (IXmlNode exp : expressions) {
            this.writeToken("[");
            if (visiter.invokeEnter(exp) == false) {
                return false;
            }
            this.writeToken("]");
        }

        return true;
    }


    /**
     * Decompile "ArrayAddr" element in XcodeML/C.
     */
    @Override
    public boolean enter(ArrayAddr visitable) {
        // DONE: ArrayRef
        // 変数名
        String value = visitable.getValue();
        // データ型
        String type = visitable.getType();

        this.writeToken(value);

        return true;
    }

    /**
     * Decompile "pointerRef" element in XcodeML/C.
     */
    @Override
    public boolean enter(PointerRef visitable) {
        // DONE: pointerRef
        XcodeMLVisitor visiter = _context.getVisitor();

        // ポインタ変数
        IXmlNode node = XmlNodeUtil.getXmlNodeChoice(visitable);
        if (node == null) return false;

        this.writeToken("*");
        if (visiter.invokeEnter(node) == false) {
            return false;
        }

        return true;
    }


    /**
     * Decompile "value" element in XcodeML/C.
     */
    @Override
    public boolean enter(Value visitable) {
        // DONE: pointerRef
        XcodeMLVisitor visiter = _context.getVisitor();

        if (visitable.getExpressionsOrValues() != null) {
            List<IXmlNode> list = visitable.getExpressionsOrValues();

            if (list.size() > 1) {
                // {}で囲む
                writeToken("{");
            }
            for (int i=0; i<list.size(); i++) {
                if (visiter.invokeEnter(list.get(i)) == false) {
                    return false;
                }
                if (i<list.size()-1) {
                    writeToken(", ");
                }
            }
            if (list.size() > 1) {
                // {}で囲む
                writeToken("}");
            }
        }
        else if (visitable.getDesignatedValue() != null) {
            if (visiter.invokeEnter(visitable.getDesignatedValue()) == false) {
                return false;
            }
        }

        return true;
    }

    /**
     * Decompile "VarAddr" element in XcodeML/C.
     */
    @Override
    public boolean enter(VarAddr visitable) {
        // DONE: VarAddr
        String value = visitable.getValue();

        boolean addr_expr = true;
        // 1つ上のノードがPointerRefであるか？
        if (this._context.isInvokeNodeOf(PointerRef.class, 1)) addr_expr = false;
        if (this._context.isInvokeNodeOf(MemberRef.class, 1)) addr_expr = false;
        if (this._context.isInvokeNodeOf(MemberAddr.class, 1)) addr_expr = false;
        if (this._context.isInvokeNodeOf(MemberArrayRef.class, 1)) addr_expr = false;
        if (this._context.isInvokeNodeOf(MemberArrayAddr.class, 1)) addr_expr = false;

        // アドレス演算子を付ける
        if (addr_expr) {
            writeToken("&");
        }

        writeToken(value);
        return true;
    }

    /**
     * Decompile "MemberRef" element in XcodeML/C.
     */
    @Override
    public boolean enter(MemberRef visitable) {
        // DONE: MemberRef
        XcodeMLVisitor visiter = _context.getVisitor();

        // 構造体メンバ
        String member = visitable.getMember();

        IXmlNode node = XmlNodeUtil.getXmlNodeChoice(visitable);
        if (visiter.invokeEnter(node) == false) {
            return false;
        }
        // 構造体メンバ参照
        if (XmlNodeUtil.isPointerStruct(visitable)) {
            writeToken("->");
        }
        else {
            writeToken(".");
        }
        writeToken(member);

        return true;
    }

    /**
     * Decompile "MemberAddr" element in XcodeML/C.
     */
    @Override
    public boolean enter(MemberAddr visitable) {
        // DONE: MemberAddr
        XcodeMLVisitor visiter = _context.getVisitor();

        // 構造体メンバ
        String member = visitable.getMember();

        // 1つ上のノードが構造体参照であるか？
        boolean addr_expr = true;
        if (this._context.isInvokeNodeOf(PointerRef.class, 1)) addr_expr = false;
        if (this._context.isInvokeNodeOf(MemberRef.class, 1)) addr_expr = false;
        if (this._context.isInvokeNodeOf(MemberAddr.class, 1)) addr_expr = false;
        if (this._context.isInvokeNodeOf(MemberArrayRef.class, 1)) addr_expr = false;
        if (this._context.isInvokeNodeOf(MemberArrayAddr.class, 1)) addr_expr = false;
        if (addr_expr) {
            writeToken("&");
        }
        IXmlNode node = XmlNodeUtil.getXmlNodeChoice(visitable);
        if (visiter.invokeEnter(node) == false) {
            return false;
        }
        // 構造体メンバ参照
        if (XmlNodeUtil.isPointerStruct(visitable)) {
            writeToken("->");
        }
        else {
            writeToken(".");
        }
        writeToken(member);

        return true;
    }

    /**
     * Decompile "MemberArrayRef" element in XcodeML/C.
     */
    @Override
    public boolean enter(MemberArrayRef visitable) {
        // DONE: MemberArrayRef
        XcodeMLVisitor visiter = _context.getVisitor();

        // 構造体メンバ
        String member = visitable.getMember();

        IXmlNode node = XmlNodeUtil.getXmlNodeChoice(visitable);
        if (visiter.invokeEnter(node) == false) {
            return false;
        }
        // 構造体メンバ参照
        if (XmlNodeUtil.isPointerStruct(visitable)) {
            writeToken("->");
        }
        else {
            writeToken(".");
        }

        writeToken(member);

        return true;
    }

    /**
     * Decompile "MemberArrayAddr" element in XcodeML/C.
     */
    @Override
    public boolean enter(MemberArrayAddr visitable) {
        // DONE: MemberArrayAddr
        XcodeMLVisitor visiter = _context.getVisitor();

        // 構造体メンバ
        String member = visitable.getMember();

        // 1つ上のノードが構造体参照であるか？
        boolean addr_expr = true;
        if (this._context.isInvokeNodeOf(PointerRef.class, 1)) addr_expr = false;
        if (this._context.isInvokeNodeOf(MemberRef.class, 1)) addr_expr = false;
        if (this._context.isInvokeNodeOf(MemberAddr.class, 1)) addr_expr = false;
        if (this._context.isInvokeNodeOf(MemberArrayRef.class, 1)) addr_expr = false;
        if (this._context.isInvokeNodeOf(MemberArrayAddr.class, 1)) addr_expr = false;
        if (addr_expr) {
            writeToken("&");
        }
        IXmlNode node = XmlNodeUtil.getXmlNodeChoice(visitable);
        if (visiter.invokeEnter(node) == false) {
            return false;
        }
        // 構造体メンバ参照
        if (XmlNodeUtil.isPointerStruct(visitable)) {
            writeToken("->");
        }
        else {
            writeToken(".");
        }
        writeToken(member);

        return true;
    }


    /**
     * Decompile "MemberArrayAddr" element in XcodeML/C.
     */
    @Override
    public boolean enter(AddrOfExpr visitable) {
        // DONE: AddrOfExpr
        XcodeMLVisitor visiter = _context.getVisitor();

        IXmlNode node = XmlNodeUtil.getXmlNodeChoice(visitable);

        // 1つ上のノードが変数参照であるか？
        boolean addr_expr = true;
        if (this._context.isInvokeNodeOf(PointerRef.class, 1)) addr_expr = false;
        if (this._context.isInvokeNodeOf(MemberRef.class, 1)) addr_expr = false;
        if (this._context.isInvokeNodeOf(MemberAddr.class, 1)) addr_expr = false;
        if (this._context.isInvokeNodeOf(MemberArrayRef.class, 1)) addr_expr = false;
        if (this._context.isInvokeNodeOf(MemberArrayAddr.class, 1)) addr_expr = false;
        if (addr_expr) {
            writeToken("&");
        }

        if (visiter.invokeEnter(node) == false) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "ReturnStatement" element in XcodeML/C.
     */
    @Override
    public boolean enter(ReturnStatement visitable) {
        XcodeMLVisitor visiter = _context.getVisitor();
        // DONE: ReturnStatement
        writeLineDirective(visitable.getLineno(), visitable.getFile());

        // return文
        writeToken("return");

        // リターン式
        IXmlNode node = XmlNodeUtil.getXmlNodeChoice(visitable);
        if (node != null) {
            writeToken(" ");
            if (visiter.invokeEnter(node) == false) {
                return false;
            }
        }

        if (!updateCodeLine(visitable)) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "GotoStatement" element in XcodeML/C.
     */
    @Override
    public boolean enter(GotoStatement visitable) {
        XcodeMLVisitor visiter = _context.getVisitor();
        // DONE: GotoStatement
        writeLineDirective(visitable.getLineno(), visitable.getFile());

        // goto文
        writeToken("goto ");

        // name
        IXmlNode node = null;
        node = visitable.getName();
        if (node == null) {
            // goto式
            node = XmlNodeUtil.getXmlNodeChoice(visitable);
            if (visiter.invokeEnter(node) == false) {
                return false;
            }
        }
        else {
            String label = visitable.getName().getValue();
            writeToken(label);
        }

        if (!updateCodeLine(visitable)) {
            return false;
        }

        return true;
    }

    /**
     * Decompile "Pragma" element in XcodeML/C.
     */
    @Override
    public boolean enter(Pragma visitable) {
        XcodeMLVisitor visiter = _context.getVisitor();
        // DONE: Pragma
        writeLineDirective(visitable.getLineno(), visitable.getFile());

        // pragma文
        writeToken(visitable.getValue());

        if (!updateCodeLine(visitable)) {
            return false;
        }

        return true;
    }



}
