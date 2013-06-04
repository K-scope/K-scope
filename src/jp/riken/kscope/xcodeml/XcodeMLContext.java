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
import java.util.Iterator;
import java.util.LinkedList;

import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.xcodeml.util.XcodeMLOption;
import jp.riken.kscope.xcodeml.xml.EnumError;
import jp.riken.kscope.xcodeml.xml.IDefBaseStatement;
import jp.riken.kscope.xcodeml.xml.IXmlNode;
import jp.riken.kscope.xcodeml.xml.gen.StatementLabel;

/**
 * XcodeMLパーサコンテキストクラス.<br/>
 * XcodeMLをパースする上での必要情報を管理する.
 *
 * @author riken
 */
public class XcodeMLContext {

    /**
     * IXmlNodeスタッククラス
     * @author riken
     */
    public class InvokeNodeStack extends LinkedList<IXmlNode> {
        /** シリアル番号 */
        private static final long serialVersionUID = 1L;

        /**
         * スタック済みノードを文字列出力する
         * @return			文字列出力
         */
        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append("[Invoke Node Stack]\n");
            for (IXmlNode node : this.toArray(new IXmlNode[0])) {
                sb.append(node.getClass().getSimpleName());
                sb.append("\n");
            }
            return sb.toString();
        }
    }

    /** エラーメッセージ */
    private String m_lastErrorMessage = null;
    /** データ型テーブル管理クラス */
    private XcodeMLTypeManager m_typeManager;
    /** エラー例外 */
    private Exception m_lastCause;
    /** ソースコード生成クラス */
    private CodeBuilder m_codebuilder;
    /** フォートランデータベース登録クラス */
    private DbUpdater m_updater = null;;
    /** 読込XMLファイル */
    private SourceFile m_sourceXmlFile;
    /** XMLノードスタック */
    private InvokeNodeStack _invokeNodeStack;
    /** 直前XMLノード */
    private IXmlNode _previousNode;
    /** XMLノード走査クラス */
    private XcodeMLVisitor _visitor;
    /** ソースファイルの基準フォルダ */
    private File baseFoleder;

    /**
     * コンストラクタ
     */
    public XcodeMLContext() {
        m_typeManager = new XcodeMLTypeManager();
        _invokeNodeStack = new InvokeNodeStack();
    }

    /**
     * Judge debug mode.
     *
     * @return true/false.
     */
    public boolean isDebugMode() {
        return XcodeMLOption.isDebugOutput();
    }

    /**
     * Return cause of last error.
     * @return     発生例外
     */
    public Exception getLastCause() {
        return m_lastCause;
    }

    /**
     * Judge output line directive.
     *
     * @return true/false.
     */
    public boolean isOutputLineDirective() {
        return !XcodeMLOption.isSuppressLineDirective();
    }

    /**
     * Set output line directive flag.
     *
     * @param outputLineDirective
     *            Output line directive flag.
     */
    public void setOutputLineDirective(boolean outputLineDirective) {
        XcodeMLOption.setIsSuppressLineDirective(!outputLineDirective);
    }

    /**
     * Get writer for Fortran.
     *
     * @return Instance of XmfWriter.
     */
    public DbUpdater getDbUpdater() {
        return m_updater;
    }

    /**
     * Set writer for Fortran.
     *
     * @param writer
     *            XfTypeManager.
     */
    public void setDbUpdater(DbUpdater writer) {
        m_updater = writer;
    }

    /**
     * Get type manager.
     *
     * @return Instance of XfTypeManager.
     */
    public XcodeMLTypeManager getTypeManager() {
        return m_typeManager;
    }

    /**
     * エラー発生しているかチェックする
     * @return		true=エラー発生
     */
    public boolean hasError() {
        if (m_lastErrorMessage != null) {
            return true;
        }
        return false;
    }

    /**
     * エラーメッセージを設定する
     * @param message		エラーメッセージ
     */
    public void setLastErrorMessage(String message) {
        m_lastErrorMessage = message;
        m_lastCause = new Exception();
    }

    /**
     * エラーメッセージを取得する
     * @return		エラーメッセージ
     */
    public String getLastErrorMessage() {
        if (hasError() == false) {
            return EnumError.SUCCESS.message();
        }
        return m_lastErrorMessage;
    }

    /**
     * デバッグ出力を行う
     * @param message		出力メッセージ
     */
    public void debugPrint(String message) {
        if (isDebugMode() != false) {
            System.out.print("Debug: ");
            System.out.print(message);
        }
    }

    /**
     * 書式付きデバッグ出力を行う
     * @param format		書式
     * @param args			出力メッセージ
     */
    public void debugPrint(String format, Object... args) {
        if (isDebugMode() != false) {
            System.out.print("Debug: ");
            System.out.format(format, args);
        }
    }

    /**
     * デバッグ出力を行う
     * @param message		出力メッセージ
     */
    public void debugPrintLine(String message) {
        if (isDebugMode() != false) {
            System.out.print("Debug: ");
            System.out.println(message);
        }
    }

    /**
     * ソースコード生成クラスを取得する
     * @return		ソースコード生成クラス
     */
    public CodeBuilder getCodeBuilder() {
        return m_codebuilder;
    }

    /**
     * ソースコード生成クラスを設定する
     * @param writer		ソースコード生成クラス
     */
    public void setCodeBuilder(CodeBuilder writer) {
        m_codebuilder = writer;
    }

    /**
     * 読込XMLファイルを取得する
     * @return		読込XMLファイル
     */
    public SourceFile getSourceXmlFile() {
        return m_sourceXmlFile;
    }

    /**
     * 読込XMLファイルを設定する
     * @param sourceXmlFile		読込XMLファイル
     */
    public void setSourceXmlFile(SourceFile sourceXmlFile) {
        m_sourceXmlFile = sourceXmlFile;
    }

    /**
     * XMLノードスタックを取得する
     * @return		XMLノードスタック
     */
    public InvokeNodeStack getInvokeNodeStack() {
        return _invokeNodeStack;
    }

    /**
     * XMLノードスタック数を取得する
     * @return		XMLノードスタック数
     */
    public int getInvokeNodeStackSize() {
        return _invokeNodeStack.size();
    }

    /**
     * 指定インデックスのXMLノードを取得する
     * @param idx		取得インデックス
     * @return		XMLノード
     */
    public IXmlNode getInvokeNode(int idx) {

        if (idx < 0) {
            throw new IllegalArgumentException();
        }

        if (idx >= getInvokeNodeStackSize()) {
            return null;
        }

        return _invokeNodeStack.get(idx);
    }

    /**
     * XMLノードスタックから最初のノードを取得する.(削除なし)
     * @return		最初ノード
     */
    public IXmlNode peekInvokeNode() {
        return _invokeNodeStack.peek();
    }

    /**
     * XMLノードスタックの最終ノードにXMLノードを追加する
     * @param node		追加XMLノード
     */
    public void pushInvokeNode(IXmlNode node) {
        _invokeNodeStack.push(node);
    }

    /**
     * XMLノードスタックから最初のノードを取得する.(削除あり)
     * @return		最初ノード
     */
    public IXmlNode popInvokeNode() {
        return _invokeNodeStack.pop();
    }

    /**
     * 指定XMLノードクラスがXMLノードスタックに含まれているかチェックする
     * @param clazz		指定XMLノードクラス
     * @return			true=存在する
     */
    public boolean isInvokeAncestorNodeOf(Class<? extends IXmlNode> clazz) {

        IXmlNode node = null;
        for (Iterator<IXmlNode> it = _invokeNodeStack.iterator(); it.hasNext();) {
            node = it.next();
            if (clazz.equals(node.getClass())) {
                return true;
            }
        }
        return false;
    }

    /**
     * 指定XMLノードクラスがXMLノードスタックの指定インデックスと一致するかチェックする
     * @param clazz			指定XMLノードクラス
     * @param parentRank		指定インデックス
     * @return		true=一致する
     */
    public boolean isInvokeNodeOf(Class<? extends IXmlNode> clazz,
            int parentRank) {
        IXmlNode node = getInvokeNode(parentRank);
        if (node == null)
            return false;
        return clazz.equals(node.getClass());
    }

    /**
     * DefBaseStatement(ファイル名、行番号)クラスを取得する。
     *
     * @param visitable
     *            現在のXMLノードクラス
     * @return DefBaseStatement(ファイル名、行番号)クラス
     */
    public IDefBaseStatement getInvokeBaseStatement(IXmlNode visitable) {

        // 現在XMLノードがDefBaseStatement(ファイル名、行番号)クラスであるか判断する。
        if (visitable instanceof IDefBaseStatement) {
            return (IDefBaseStatement) visitable;
        }

        // 現在要素の親要素からファイル名、開始行番号、終了行番号を取得する。
        IXmlNode node = null;
        IDefBaseStatement base_node = null;
        for (Iterator<IXmlNode> it = _invokeNodeStack.iterator(); it.hasNext();) {
            node = it.next();
            if (node instanceof IDefBaseStatement) {
                base_node = (IDefBaseStatement) node;
                break;
            }
        }

        return base_node;
    }

    /**
     * @return m_visitor XMLノード走査クラス
     */
    public XcodeMLVisitor getVisitor() {
        return _visitor;
    }

    /**
     * @param visitor
     *            XMLノード走査クラスをセットする。
     */
    public void setVisitor(XcodeMLVisitor visitor) {
        _visitor = visitor;
    }

    /**
     * 事前ノードを設定する。
     *
     * @param node
     *            事前XMLノード
     */
    public void setPreviousNode(IXmlNode node) {
        this._previousNode = node;
    }

    /**
     * StatementLabelノードを取得する。 事前XMLノードがStatementLabelであれば、事前XMLノードを返す。
     *
     * @return StatementLabelノード
     */
    public StatementLabel getStatementLabelNode() {
        if (_previousNode instanceof StatementLabel) {
            return (StatementLabel) _previousNode;
        }
        return null;
    }

    /**
     * StatementLabelノードを取得する。 事前XMLノードがStatementLabelであれば、事前XMLノードを返す。
     *
     * @return StatementLabelノード
     */
    public String getStatementLabel() {
        if (_previousNode instanceof StatementLabel) {
            return ((StatementLabel) _previousNode).getLabelName();
        }
        return null;
    }

    /**
     * ソースファイルの基準フォルダを取得する
     * @return   ソースファイルの基準フォルダ
     */
    public File getBaseFolder() {
        return this.baseFoleder;
    }

    /**
     * ソースファイルの基準フォルダを設定する
     * @param folder		ソースファイルの基準フォルダ
     */
    public void setBaseFolder(File folder) {
        this.baseFoleder = folder;
    }
}



