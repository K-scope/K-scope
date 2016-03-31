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
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Stack;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import jp.riken.kscope.Message;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.ErrorInfo;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.exception.LanguageException;
import jp.riken.kscope.exception.XcodeMLException;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.utils.FileUtils;
import jp.riken.kscope.xcodeml.xml.IXmlNode;
import jp.riken.kscope.xcodeml.xml.gen.FcontainsStatement;
import jp.riken.kscope.xcodeml.xml.gen.FfunctionDefinition;
import jp.riken.kscope.xcodeml.xml.gen.FmoduleDefinition;
import jp.riken.kscope.xcodeml.xml.gen.GlobalSymbols;
import jp.riken.kscope.xcodeml.xml.gen.Name;
import jp.riken.kscope.xcodeml.xml.gen.ObjectFactory;
import jp.riken.kscope.xcodeml.xml.gen.TypeTable;
import jp.riken.kscope.xcodeml.xml.gen.XcodeProgram;

/**
 * XcodeML構文解析クラス
 *
 * XcodeML出力のXMLファイルから一括でバインディングを行い、XcodeProgramクラスを生成する。
 * 生成XcodeProgramクラスから、コード行を作成し、データベースへ登録する。
 *
 * @author RIKEN
 */
public class XcodeMLParserStax extends XcodeMLParserCls {

    /** StAX 用ファクトリ */
    private XMLInputFactory factory = null;

    /** JAXB 用ファクトリ */
    private JAXBContext context = null;
    /** JAXB アンマーシャラー */
    private Unmarshaller unmarshaller = null;
    /** 現在処理中ノード */
    protected Stack<IXmlNode> m_nodeStack = null;
    /** Fortranデータベース */
    protected Fortran m_dbFortran = null;
    /** XcodeMLパース設定 */
    protected XcodeMLContext m_xmodContext = null;
    /** XcodeMLパーサ */
    protected XcodeMLVisitor m_xmodVisitor = null;
    // modify at 2013/03/01 by @hira FileからSourceFileに変更
    /** 元ソースファイル(Fortranソースファイル) */
    protected SourceFile languageFile = null;

    /**
     * コンストラクタ
     */
    public XcodeMLParserStax() {
        super();

        try {
            // StAX用ファクトリの生成
            factory = XMLInputFactory.newInstance();

            // JAXB用ファクトリの生成
            context = JAXBContext.newInstance(ObjectFactory.class);
            // アンマーシャラー生成
            unmarshaller = context.createUnmarshaller();

            // XcodeMLパース設定
            m_xmodContext = new XcodeMLContext();
            // デバッグ出力
            // XcodeMLOption.setDebugOutput(true);

//            String outputFilePath = "xcodeml_debug.txt";
//            PrintWriter writer = new PrintWriter(new BufferedWriter(
//                    new FileWriter(outputFilePath)));

            CodeBuilder fwriter = new CodeBuilder(m_xmodContext);
//            fwriter.setWriter(writer);

            m_xmodContext.setCodeBuilder(fwriter);

            m_xmodVisitor = new XcodeMLVisitor(m_xmodContext);

            // 現在のFfunctionDefinition、FmoduleDefinitionのスタックリスト
            m_nodeStack = new Stack<IXmlNode>();

        } catch (FactoryConfigurationError e) {
            e.printStackTrace();
        } catch (JAXBException e) {
            e.printStackTrace();
        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    /**
     * ソースファイルからファイルを読み込み、事前処理を行う。
     *
     * @param file
     *            ソースファイル
     *
     */
    @Override
    public void readFile(SourceFile file) {
        m_sourceFile = file;
    }

    /**
     * 生成XcodeProgramクラスから、コード行を作成し、データベースへ登録する。
     *
     * @param ft
     *            解析結果格納フォートランデータベース
     * @throws InterruptedException            割り込みエラー
     */
    @Override
    public void parseFile(Fortran ft) throws InterruptedException {

        firePropertyChange("prograss_start", null, null);
        firePropertyChange("prograss_maxvalue", null, 100);
        firePropertyChange("status_sub_message", null, "parse start " + m_sourceFile.getFile().getName());

        XMLStreamReader reader = null;
        InputStream stream = null;

        // XcodeMLのパース準備
        m_dbFortran = ft;
        DbUpdater db = new DbUpdater(m_dbFortran, m_xmodContext);
        m_xmodContext.setDbUpdater(db);
        m_xmodContext.setSourceXmlFile(m_sourceFile);

        try {
            String xmlfile = m_sourceFile.getPath();
            stream = new FileInputStream(xmlfile);

            // パーサの生成
            reader = factory.createXMLStreamReader(stream);

            // イベントループ
            while (reader.hasNext()) {
                // 次のイベントを取得
                int eventType = reader.next();
                if (eventType == XMLStreamReader.START_ELEMENT) {
                    String elem_name = reader.getLocalName();
//                    System.out.println(elem_name);
                    firePropertyChange("status_sub_message", null, "parsing..." + elem_name);

                    // XcodeProgram
                    if (parseXcodeProgram(reader)) {
                    }
                    // typeTable、globalSymbols
                    else if (unmarshalType(reader)) {
                    }
                    // FfunctionDefinition、FmoduleDefinition、body、FcontainsStatement
                    else if (startElement(reader)) {
                    }
                    // その他
                    else if (unmarshalNode(reader)) {
                    }
                } else if (eventType == XMLStreamReader.END_ELEMENT) {
                    endElement(reader);
                }

                /** for debug by @hira */
                if (Thread.interrupted()) {
                    throw new InterruptedException();
                }
                /** for debug by @hira */

            }
        } catch (IOException ex) {
            firePropertyChange("status_sub_message", null, "exception");
            firePropertyChange("prograss_clear", null, null);
            throw new LanguageException(ex, m_sourceFile);

        } catch (XcodeMLException ex) {
            firePropertyChange("status_sub_message", null, "exception");
            firePropertyChange("prograss_clear", null, null);
            throw new LanguageException(ex, m_sourceFile);
        } catch (XMLStreamException ex) {
            firePropertyChange("status_sub_message", null, "exception");
            firePropertyChange("prograss_clear", null, null);
            throw new LanguageException(ex, m_sourceFile);
        } finally {

            // ステータスバー：進捗メッセージ
            firePropertyChange("status_sub_message", null, "done");
            firePropertyChange("prograss_clear", null, null);

            if (reader != null) {
                try {
                    reader.close();
                } catch (XMLStreamException ex) {
                }
            }
            if (stream != null) {
                try {
                    stream.close();
                } catch (IOException ex) {
                }
            }
        }

        // ステータスバー：進捗メッセージ
        firePropertyChange("status_sub_message", null, "done");
        firePropertyChange("prograss_clear", null, null);
    }

    /**
     * タイプテーブル要素をJAXBでアンマーシャリングを行う。
     * 要素名が typeTable, globalSymbolsのみアンマーシャリングを行う。
     * @param reader     ストリームリーダー
     * @return    成否
     * @throws XcodeMLException     XMLリードエラー
     */
    private boolean unmarshalType(XMLStreamReader reader)
            throws XcodeMLException {

        String elem_name = reader.getLocalName();
        if (!"typeTable".equals(elem_name)
                && !"globalSymbols".equals(elem_name)) {
            return false;
        }

        try {
            // アンマーシャリング
            Object obj = unmarshaller.unmarshal(reader);
            if (m_program == null) {
                m_program = new XcodeProgram();
            }

            // タイプテーブルをXcodeProgramにセットする。
            if ("typeTable".equals(elem_name)) {
                m_program.setTypeTable((TypeTable) obj);
                invokeEnter((TypeTable) obj);
            } else if ("globalSymbols".equals(elem_name)) {
                m_program.setGlobalSymbols((GlobalSymbols) obj);
                invokeEnter((GlobalSymbols) obj);
            }

        } catch (JAXBException ex) {
            // 例外処理:アンマーシャリングに失敗しました。[要素名=%s]
            throw new XcodeMLException(Message.getString("xcodemlparserstax.error.unmarshaller",
                                                         reader.getLocalName()));
        }

        return true;
    }

    /**
     * タイプテーブル要素をJAXBでアンマーシャリングを行う。
     * @param reader     ストリームリーダー
     * @return    成否
     * @throws XcodeMLException     XMLリードエラー
     */
    private boolean unmarshalNode(XMLStreamReader reader)
            throws XcodeMLException {
        try {
            // 要素名
            String elem_name = reader.getLocalName();

            // FfunctionDefinition/nameの場合
            if ("name".equals(elem_name)) {
                // FfunctionDefinition要素配下のname要素と共にVisiterへ渡す。
                // DB登録はVisitorから行う。
                IXmlNode node = m_nodeStack.peek();
                if (node != null && node instanceof FfunctionDefinition) {
                    // アンマーシャリング
                    Name name = (Name) unmarshaller.unmarshal(reader);
                    assert (name != null);

                    // FfunctionDefinition/name要素のみ組み立ててVisiterへ渡す。
                    ((FfunctionDefinition) node).setName(name);
                    node.enter(m_xmodVisitor);
                    return true;
                }
            }

            // アンマーシャリング
            IXmlNode node = (IXmlNode) unmarshaller.unmarshal(reader);

            invokeEnter(node);

        } catch (JAXBException ex) {
            // 例外処理:アンマーシャリングに失敗しました。[要素名=%s]
            throw new XcodeMLException(Message.getString("xcodemlparserstax.error.unmarshaller",
                                                         reader.getLocalName()));
        }

        return true;
    }

    /**
     * JAXBでアンマーシャリングを行わず、XML要素を直接取得する。
     *
     * FfunctionDefinition、FmoduleDefinition、FcontainsStatement要素
     *
     * @param reader     ストリームリーダー
     * @return    成否
     * @throws XcodeMLException     XMLリードエラー
     */
    private boolean startElement(XMLStreamReader reader)
            throws XcodeMLException {

        // 要素名
        String elem_name = reader.getLocalName();

        // 無視する要素
        if ("globalDeclarations".equals(elem_name))
            return true;
        if ("body".equals(elem_name))
            return true;

        // 属性
        String filename = reader.getAttributeValue(null, "file");
        String lineno = reader.getAttributeValue(null, "lineno");
        String endlineno = reader.getAttributeValue(null, "endlineno");

        IXmlNode node = null;
        // FfunctionDefinition、FmoduleDefinition、body要素以外は対象外
        if ("FfunctionDefinition".equals(elem_name)) {
            node = new FfunctionDefinition();
            ((FfunctionDefinition) node).setFile(filename);
            ((FfunctionDefinition) node).setLineno(lineno);
            ((FfunctionDefinition) node).setEndlineno(endlineno);
        } else if ("FmoduleDefinition".equals(elem_name)) {
            String name = reader.getAttributeValue(null, "name");
            node = new FmoduleDefinition();
            ((FmoduleDefinition) node).setFile(filename);
            ((FmoduleDefinition) node).setLineno(lineno);
            ((FmoduleDefinition) node).setEndlineno(endlineno);
            ((FmoduleDefinition) node).setName(name);
            node.enter(m_xmodVisitor);
        } else if ("FcontainsStatement".equals(elem_name)) {
            node = new FcontainsStatement();
            ((FcontainsStatement) node).setFile(filename);
            ((FcontainsStatement) node).setLineno(lineno);
            ((FcontainsStatement) node).setEndlineno(endlineno);
            node.enter(m_xmodVisitor);
        }

        if (node == null)
            return false;

        // XML要素をスタックに入れる
        m_nodeStack.push(node);

        // 事前のXMLノードをクリアする。
        m_xmodContext.setPreviousNode(null);

        return true;
    }

    /**
     * ノードの終了タグによるイベント
     *
     * Visitorのleaveメソッドを呼出し、DBへブロックの終了を登録する。
     *
     * @param reader        XMLスキーマリーダー
     * @return                成否
     * @throws XcodeMLException        パースエラー
     */
    private boolean endElement(XMLStreamReader reader) throws XcodeMLException {

        // 要素名
        String elem_name = reader.getLocalName();

        if (m_nodeStack.size() <= 0)
            return true;

        // スタックノード
        IXmlNode node = m_nodeStack.peek();

        if ("FfunctionDefinition".equals(elem_name)
                && node instanceof FfunctionDefinition) {
            node.leave(m_xmodVisitor);
            m_nodeStack.pop();
        } else if ("FmoduleDefinition".equals(elem_name)
                && node instanceof FmoduleDefinition) {
            node.leave(m_xmodVisitor);
            m_nodeStack.pop();
        } else if ("FcontainsStatement".equals(elem_name)
                && node instanceof FcontainsStatement) {
            node.leave(m_xmodVisitor);
            m_nodeStack.pop();
        }

        return true;
    }

    /**
     * XMLノードをパースする
     *
     * @param node
     *            XMLノード
     * @throws XcodeMLException
     */
    private void invokeEnter(IXmlNode node) throws XcodeMLException {

        if (!m_xmodVisitor.invokeEnter(node)) {
            throw new XcodeMLException(m_xmodContext.getLastErrorMessage(),
                    m_xmodContext.getLastCause());
        }

        // 事前のXMLノードを設定する。
        m_xmodContext.setPreviousNode(node);

        if (m_xmodContext.getCodeBuilder() != null)
            m_xmodContext.getCodeBuilder().flush();
    }

    /**
     * ルート要素のXcodeProgramをパースする。
     *
     * XcodeProgramに記述のソースファイルが存在するかチェックする。 ソースファイルが存在しない場合はXcodeMLExceptionとする。
     *
     * @param reader        XMLスキーマリーダー
     * @return                成否
     * @throws XcodeMLException        パースエラー
     */
    private boolean parseXcodeProgram(XMLStreamReader reader) throws XcodeMLException {

        // 要素名
        String elem_name = reader.getLocalName();
        if (!"XcodeProgram".equals(elem_name))
            return false;

        String source = reader.getAttributeValue(null, "source");
        if (source == null) {
            throw new XcodeMLException(Message.getString("xcodemlparserstax.error.sourcefile")); //ソースファイルが設定されていません。
        }

        // modify by @hira at 2016/01/28
        File srcPath = null;
        if (new File(source).isAbsolute()) {
            // 元ソースファイルを設定する
            this.languageFile = new SourceFile(source);
            srcPath = new File(source);
        }
        if (this.languageFile == null || this.languageFile.getFile() == null
            || !this.languageFile.getFile().exists()) {
            if (this.m_xmodContext.getBaseFolder() != null) {
                // 元ソースファイルパスの取得を行う
                srcPath = new File(this.m_xmodContext.getBaseFolder()  + File.separator + source);
                this.languageFile = new SourceFile(srcPath);
            }
            else {
                this.languageFile = new SourceFile(srcPath);
            }
        }
        if (this.languageFile == null || this.languageFile.getFile() == null
            || !this.languageFile.getFile().exists()) {
            // 元ソースファイルパスの取得を行う
            File path = m_sourceFile.getFile().getParentFile();
            srcPath = new File(path.getAbsoluteFile() + File.separator + source);

            if (this.m_xmodContext.getBaseFolder() != null) {
                // 基準パスからの相対パスを取得する
                String relPath = FileUtils.getRelativePath(srcPath, this.m_xmodContext.getBaseFolder());
                if (relPath == null) {
                    this.languageFile = new SourceFile(srcPath);
                }
                else {
                    this.languageFile = new SourceFile(relPath);
                }
            }
            else {
                this.languageFile = new SourceFile(srcPath);
            }
        }
        if (srcPath != null && srcPath.exists()) {
            // 相対パス設定であるかもしれないので、更新日付を絶対パスから設定する。
            this.languageFile.setModifyDate(srcPath);
        }

//        // 元ソースファイルの存在チェック
//        if (!this.languageFile.exists()) {
//            throw new XcodeMLException("ソースファイル[" + source + "]が存在しません。");
//        }
        // XMLファイルの更新日付を更新する。
        this.m_sourceFile.updateModifyDate();

        // XMLファイルにソースファイル、ソースファイルにXMLファイルを関連付ける
        this.m_sourceFile.setRelationFile(this.languageFile);
        this.languageFile.setRelationFile(this.m_sourceFile);

        return true;
    }

    @Override
    public CodeLine[] getCodeLineList() throws InterruptedException {

        // ダミーのデータベース作成
        Fortran ft = new Fortran();
        parseFile(ft);
        CodeBuilder builder = m_xmodContext.getCodeBuilder();
        if (builder == null)
            return null;

        return builder.getCodeLineList();
    }


    /**
     * 元ソースファイル(Fortranソースファイル)を取得する
     * @return        元ソースファイル(Fortranソースファイル)
     */
    @Override
    public SourceFile getLanguageFile() {
        return languageFile;
    }


    /**
     * XMLファイルからソースファイルを取得する.
     */
    @Override
    public void parseSourceFile()  {

        firePropertyChange("prograss_start", null, null);
        firePropertyChange("prograss_maxvalue", null, 100);
        firePropertyChange("status_sub_message", null, "parse start " + m_sourceFile.getFile().getName());

        XMLStreamReader reader = null;
        InputStream stream = null;

        try {
            String xmlfile = m_sourceFile.getPath();
            stream = new FileInputStream(xmlfile);

            // パーサの生成
            reader = factory.createXMLStreamReader(stream);

            // イベントループ
            while (reader.hasNext()) {
                // 次のイベントを取得
                int eventType = reader.next();
                if (eventType == XMLStreamReader.START_ELEMENT) {
                    String elem_name = reader.getLocalName();
//                    System.out.println(elem_name);
                    firePropertyChange("status_sub_message", null, "parsing..." + elem_name);

                    // XcodeProgramからソースファイルリストのみパースする。
                    if (parseXcodeProgram(reader)) {
                        break;
                    }
                } else if (eventType == XMLStreamReader.END_ELEMENT) {
                    endElement(reader);
                }
            }
        } catch (IOException ex) {
            firePropertyChange("status_sub_message", null, "exception");
            firePropertyChange("prograss_clear", null, null);
            throw new LanguageException(ex, m_sourceFile);

        } catch (XcodeMLException ex) {
            firePropertyChange("status_sub_message", null, "exception");
            firePropertyChange("prograss_clear", null, null);
            throw new LanguageException(ex, m_sourceFile);
        } catch (XMLStreamException ex) {
            firePropertyChange("status_sub_message", null, "exception");
            firePropertyChange("prograss_clear", null, null);
            throw new LanguageException(ex, m_sourceFile);
        } finally {

            // ステータスバー：進捗メッセージ
            firePropertyChange("status_sub_message", null, "done");
            firePropertyChange("prograss_clear", null, null);

            if (reader != null) {
                try {
                    reader.close();
                } catch (XMLStreamException ex) {
                }
            }
            if (stream != null) {
                try {
                    stream.close();
                } catch (IOException ex) {
                }
            }
        }

        // ステータスバー：進捗メッセージ
        firePropertyChange("status_sub_message", null, "done");
        firePropertyChange("prograss_clear", null, null);
    }

    /**
     * ソースファイルの基準フォルダを設定する
     * @param    folder    ソースファイルの基準フォルダ
     */
    @Override
    public void setBaseFolder(File folder) {
        m_xmodContext.setBaseFolder(folder);
    }

    /**
     * エラー情報を取得する.
     * @return        エラー情報リスト
     */
    @Override
    public ErrorInfo[] getErrorInfos() {
        if (this.m_xmodContext == null) return null;
        if (this.m_xmodContext.getDbUpdater() == null) return null;

        return this.m_xmodContext.getDbUpdater().getListErrorInfo();
    }
}
