package jp.riken.kscope.xcodeml;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
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

import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.ErrorInfo;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.exception.LanguageException;
import jp.riken.kscope.exception.XcodeMLException;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.ProgramUnit;
import jp.riken.kscope.parser.IAnalyseParser;
import jp.riken.kscope.utils.FileUtils;

public abstract class XcodeMLParser implements IAnalyseParser {

    /** 進捗状況表示用プロパティ */
    PropertyChangeSupport m_statusProperty;
    /** キャンセルフラグ */
    boolean m_cancel = false;
    /** インクルードファイルがない場合の対応 true:確認する/false:無視する */
    @SuppressWarnings("unused")
    private boolean m_confirm_include = true;
    /** パース対象のXMLファイル */
    protected SourceFile m_sourceFile;
    /** 元ソースファイル(ソースファイル:*.c,*.f) */
    protected SourceFile languageFile = null;
    /** Fortranデータベース */
    protected Fortran languageDb = null;

    /**
     * コンストラクタ
     */
    public XcodeMLParser() {
        // ステータスバーへの表示リスナ用
        m_statusProperty = new PropertyChangeSupport(this);
    }


    /**
     * プロパティー変更リスナの登録を行う。
     *
     * @param listener
     *            ステータスリスナー
     */
    @Override
    public void addPropertyChangeListener(PropertyChangeListener listener) {
        this.m_statusProperty.addPropertyChangeListener(listener);
    }

    /**
     * プロパティー変更リスナの削除を行う。
     *
     * @param listener
     *            ステータスリスナー
     */
    @Override
    public void removePropertyChangeListener(PropertyChangeListener listener) {
        this.m_statusProperty.removePropertyChangeListener(listener);
    }

    /**
     * プロパティー変更をリスナへ通知する
     *
     * @param propertyName
     *            プロパティー名
     * @param oldValue
     *            旧プロパティ値
     * @param newValue
     *            新プロパティ値
     */
    @Override
    public void firePropertyChange(String propertyName, Object oldValue, Object newValue) {
        this.m_statusProperty.firePropertyChange(propertyName, oldValue, newValue);
        if ("status_message".equals(propertyName)) {
            Application.status.setMessageStatus((String)newValue);
        }
        else if ("status_sub_message".equals(propertyName)) {
            Application.status.setMessageStatus((String)newValue);
        }
        else if ("prograss_clear".equals(propertyName)) {
            Application.status.setProgressStart(false);
        }
        /*
        else if ("prograss_maxvalue".equals(propertyName)) {
            Application.status.setProgress(0, (Integer)newValue);
        }
        else if ("prograss_value".equals(propertyName)) {
            Application.status.setProgressValue((Integer)newValue);
        }
        */
        else if ("prograss_start".equals(propertyName)) {
            Application.status.setProgressStart(true);
        }

    }

    /**
     * 元ソースファイル(Fortranソースファイル)を取得する
     * @return		元ソースファイル(Fortranソースファイル)
     */
    @Override
    public SourceFile getLanguageFile() {
        return languageFile;
    }


    /**
     * プロパティ変更による構文解析処理の中止イベントを取得する。
     *
     * @param evt
     *            プロパティ変更イベント
     */
    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        if ("cancel".equals(evt.getPropertyName())) {
            setCancel((Boolean) evt.getNewValue());
        }
    }

    /**
     * 構文解析処理の中止を設定する。
     *
     * @param cancel
     *            true:中止
     */
    @Override
    public void setCancel(boolean cancel) {
        m_cancel = cancel;
    }

    /**
     * 構文解析処理の中止を判断する。
     *
     * @return true:中止
     */
    public boolean isCancel() {
        return m_cancel;
    }

    /**
     * FortranクラスのcurrentUnitがProgramUnitであるかチェックする。
     *
     * @param ft
     *            出力Fortranクラス
     * @return true:currentUnitはProgramUnitである。
     */
    @SuppressWarnings("unused")
    private boolean isCurrentProgramUnit(Fortran ft) {
        ProgramUnit unit = ft.get_current_unit();
        if (unit instanceof ProgramUnit) {
            return true;
        }

        return false;
    }


    /**
     * 存在しなかったインクルードの確認フラグ
     *
     * @param confirm
     *            true:存在しなかった場合インクルードファイルを確認する
     */
    @Override
    public void setConfirmInclude(boolean confirm) {
        m_confirm_include = true;
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

                    // XcodeProgramからソースファイルリストのみパースする。
                    if (parseXcodeProgram(reader)) {
                        break;
                    }
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
     * ルート要素のXcodeProgramをパースする。
     *
     * XcodeProgramに記述のソースファイルが存在するかチェックする。 ソースファイルが存在しない場合はXcodeMLExceptionとする。
     *
     * @param reader		XMLスキーマリーダー
     * @return				成否
     * @throws XcodeMLException		パースエラー
     */
    protected boolean parseXcodeProgram(XMLStreamReader reader) throws XcodeMLException {

        // 要素名
        String elem_name = reader.getLocalName();
        if (!"XcodeProgram".equals(elem_name))
            return false;

        // 属性取得
        String source = reader.getAttributeValue(null, "source");
        String language = reader.getAttributeValue(null, "language");
        String time = reader.getAttributeValue(null, "time");
        String compiler_info = reader.getAttributeValue(null, "compiler-info");
        String version = reader.getAttributeValue(null, "version");

        if (source == null) {
            throw new XcodeMLException(Message.getString("xcodemlparserstax.error.sourcefile")); //ソースファイルが設定されていません。
        }

        if (new File(source).isAbsolute()) {
            // 元ソースファイルを設定する
            this.languageFile = new SourceFile(source);
        }
        else {
            // 元ソースファイルパスの取得を行う
            File path = m_sourceFile.getFile().getParentFile();
            File srcPath = new File(path.getAbsoluteFile() + File.separator + source);

            if (this.getBaseFolder() != null) {
                // 基準パスからの相対パスを取得する
                String relPath = FileUtils.getRelativePath(srcPath, this.getBaseFolder());
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

}
