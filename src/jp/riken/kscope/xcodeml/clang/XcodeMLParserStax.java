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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
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
import jp.riken.kscope.utils.StringUtils;
import jp.riken.kscope.xcodeml.XcodeMLParser;
import jp.riken.kscope.xcodeml.clang.xml.IXmlNode;
import jp.riken.kscope.xcodeml.clang.DbUpdater;
import jp.riken.kscope.xcodeml.clang.CodeBuilder;
import jp.riken.kscope.xcodeml.clang.xml.gen.FunctionDefinition;
import jp.riken.kscope.xcodeml.clang.xml.gen.GlobalSymbols;
import jp.riken.kscope.xcodeml.clang.xml.gen.TypeTable;
import jp.riken.kscope.xcodeml.clang.xml.gen.XcodeProgram;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FcontainsStatement;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FfunctionDefinition;
import jp.riken.kscope.xcodeml.fortran.xml.gen.FmoduleDefinition;
import jp.riken.kscope.xcodeml.fortran.xml.gen.Name;

/**
 * XcodeML構文解析クラス
 *
 * XcodeML出力のXMLファイルから一括でバインディングを行い、XcodeProgramクラスを生成する。
 * 生成XcodeProgramクラスから、コード行を作成し、データベースへ登録する。
 * @author RIKEN
 * @version    2015/04/01      エラーリスト(java.util.List)の追加バグ修正 : getErrorInfos
 * @version    2015/04/09      ソースファイル名を相対パスに変更   :  parseXcodeProgram
 */
public class XcodeMLParserStax extends XcodeMLParser {

    /** JAXB アンマーシャラー */
    private Unmarshaller unmarshaller = null;
    /** 現在処理中ノード */
    protected Stack<IXmlNode> m_nodeStack = null;
    /** XcodeMLパース設定 */
    protected XcodeMLContext m_xmodContext = null;
    /** XcodeMLパーサ */
    protected XcodeMLVisitor m_xmodVisitor = null;
    /** XcodeML:XcodeProgram要素(トップ要素) */
    protected XcodeProgram m_program;

    /**
     * コンストラクタ
     */
    public XcodeMLParserStax() {
        super();

        try {
            // JAXB用ファクトリの生成
            JAXBContext context = JAXBContext.newInstance(
                        jp.riken.kscope.xcodeml.clang.xml.gen.ObjectFactory.class);
            // アンマーシャラー生成
            unmarshaller = context.createUnmarshaller();

            // XcodeMLパース設定
            m_xmodContext = new XcodeMLContext();

            CodeBuilder fwriter = new CodeBuilder(m_xmodContext);

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
     * 生成XcodeProgramクラスから、コード行を作成し、データベースへ登録する。
     *
     * @param ft
     *            解析結果格納プログラムデータベース
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
        this.languageDb = ft;
        DbUpdater db = new DbUpdater(this.languageDb, m_xmodContext);
        m_xmodContext.setDbUpdater(db);
        m_xmodContext.setSourceXmlFile(m_sourceFile);

        try {
            String xmlfile = m_sourceFile.getPath();
            stream = new FileInputStream(xmlfile);

            // StAX用ファクトリの生成
            XMLInputFactory factory = XMLInputFactory.newInstance();
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
     * ルート要素のXcodeProgramをパースする。
     *
     * XcodeProgramに記述のソースファイルが存在するかチェックする。 ソースファイルが存在しない場合はXcodeMLExceptionとする。
     *
     * @param reader        XMLスキーマリーダー
     * @return                成否
     * @throws XcodeMLException        パースエラー
     */
    protected boolean parseXcodeProgram(XMLStreamReader reader) throws XcodeMLException {

        if (!super.parseXcodeProgram(reader)) {
            return false;
        }

        // 属性取得
        String source = reader.getAttributeValue(null, "source");
        String language = reader.getAttributeValue(null, "language");
        String time = reader.getAttributeValue(null, "time");
        String compiler_info = reader.getAttributeValue(null, "compiler-info");
        String version = reader.getAttributeValue(null, "version");
        if (this.languageFile != null) {
            source = this.languageFile.getPath();
        }

        XcodeProgram program = new XcodeProgram();
        program.setSource(source);
        program.setLanguage(language);
        program.setTime(time);
        program.setCompilerInfo(compiler_info);
        program.setVersion(version);

        // XcodeProgram要素の登録と行う.
        invokeEnter(program);

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
     * タイプテーブル要素をJAXBでアンマーシャリングを行う。
     * 要素名が typeTable, globalSymbolsのみアンマーシャリングを行う。
     * @param reader     ストリームリーダー
     * @return    成否
     * @throws XcodeMLException     XMLリードエラー
     */
    private boolean unmarshalType(XMLStreamReader reader) throws XcodeMLException {
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

    private boolean startElement(XMLStreamReader reader) throws XcodeMLException {

        // 要素名
        String elem_name = reader.getLocalName();

        // 無視する要素
        if ("globalDeclarations".equals(elem_name)) return true;

        // functionDefinition要素以外は対象外
        if (!"functionDefinition".equals(elem_name)) return false;

        // 属性
        String filename = reader.getAttributeValue(null, "file");
        String lineno = reader.getAttributeValue(null, "lineno");
        String endlineno = reader.getAttributeValue(null, "endlineno");

        FunctionDefinition node = new FunctionDefinition();
        node.setFile(filename);
        node.setLineno(lineno);
        node.setEndlineno(endlineno);

        // XML要素をスタックに入れる
        m_nodeStack.push(node);

        // 事前のXMLノードをクリアする。
        m_xmodContext.setPreviousNode(null);

        return false;
    }

    private void endElement(XMLStreamReader reader) throws XcodeMLException {
        // TODO 自動生成されたメソッド・スタブ

    }

    private boolean unmarshalNode(XMLStreamReader reader) throws XcodeMLException {

        try {
            // 要素名
            String elem_name = reader.getLocalName();

            // アンマーシャリング
            IXmlNode node = (IXmlNode) unmarshaller.unmarshal(reader);

            // functionDeclは無意味であるので無視する。
            if ("functionDecl".equals(elem_name)) return true;

            invokeEnter(node);

        } catch (JAXBException ex) {
            // 例外処理:アンマーシャリングに失敗しました。[要素名=%s]
            throw new XcodeMLException(Message.getString("xcodemlparserstax.error.unmarshaller",
                                                         reader.getLocalName()));
        }

        return true;
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
        ErrorInfo[] infos = this.m_xmodContext.getDbUpdater().getListErrorInfo();
        if (StringUtils.isNullOrEmpty(this.m_xmodContext.getLastErrorMessage())) {
            return infos;
        }
        List<ErrorInfo> list =  new ArrayList<ErrorInfo>();
        if (infos != null) {
            list.addAll(Arrays.asList(infos));
        }
        list.add(new ErrorInfo(this.m_xmodContext.getLastErrorMessage()));

        return list.toArray(new ErrorInfo[0]);
    }

    /**
     * ソースファイルの基準フォルダを取得する
     * @return   ソースファイルの基準フォルダ
     */
    @Override
    public File getBaseFolder() {
        if (this.m_xmodContext == null) return null;
        return this.m_xmodContext.getBaseFolder();
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

}
