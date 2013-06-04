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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.File;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import jp.riken.kscope.Application;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.exception.LanguageException;
import jp.riken.kscope.exception.XcodeMLException;
import jp.riken.kscope.language.Fortran;
import jp.riken.kscope.language.ProgramUnit;
import jp.riken.kscope.parser.IAnalyseParser;
import jp.riken.kscope.xcodeml.xml.gen.ObjectFactory;
import jp.riken.kscope.xcodeml.xml.gen.XcodeProgram;

/**
 * XcodeML構文解析抽象クラス
 *
 * XcodeML出力のXMLファイルから一括でバインディングを行い、XcodeProgramクラスを生成する。
 * 生成XcodeProgramクラスから、コード行を作成し、データベースへ登録する。
 *
 * @author riken
 */
public abstract class XcodeMLParserCls implements IAnalyseParser {

    int HIST_NUM = 10;
    Integer[] histgram;
    int line_counter = 0;
    /** 進捗状況表示用プロパティ */
    PropertyChangeSupport m_statusProperty;
    /** キャンセルフラグ */
    boolean m_cancel = false;
    /** インクルードファイルがない場合の対応 true:確認する/false:無視する */
    private boolean m_confirm_include = true;

    /** パース対象のXMLファイル */
    protected SourceFile m_sourceFile;

    /** XcodeML:XcodeProgram要素(トップ要素) */
    protected XcodeProgram m_program;

    /**
     * コンストラクタ
     */
    public XcodeMLParserCls() {
        histgram = new Integer[10];
        for (int i = 0; i < 10; i++)
            histgram[i] = 0;

        // ステータスバーへの表示リスナ用
        m_statusProperty = new PropertyChangeSupport(this);
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

        firePropertyChange("prograss_start", null, null);
        firePropertyChange("prograss_maxvalue", null, 100);

        try {
            m_sourceFile = file;

            String filePath = file.getPath();

            long start = System.currentTimeMillis();

            // JAXBContextオブジェクトの生成
            JAXBContext context = JAXBContext.newInstance(ObjectFactory.class);

            // Unmarsallerオブジェクトの取得
            Unmarshaller unmarshaller = context.createUnmarshaller();

            // アンマーシャリング
            // 戻り値はXcodeProgramクラス
            m_program = (XcodeProgram) unmarshaller
                    .unmarshal(new File(filePath));

            long stop = System.currentTimeMillis();
            long diff = stop - start;
            // System.out.println("JaxbReader 実行時間 : " + diff + "ミリ秒");

        } catch (JAXBException ex) {
            firePropertyChange("status_sub_message", null, "exception");
            firePropertyChange("prograss_clear", null, null);
            throw new LanguageException(ex, m_sourceFile);
        } finally {
            firePropertyChange("prograss_clear", null, null);
        }
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
     * 生成XcodeProgramクラスから、コード行を作成し、データベースへ登録する。
     *
     * @param ft
     *            解析結果格納フォートランデータベース
     * @throws InterruptedException 			割り込みエラー
     */
    @Override
    public void parseFile(Fortran ft) throws InterruptedException {
        try {
//            String outputFilePath = "xcodeml_debug.txt";
//            PrintWriter writer = new PrintWriter(new BufferedWriter(
//                    new FileWriter(outputFilePath)));

            XcodeMLContext context = new XcodeMLContext();
            // デバッグ出力
            // XcodeMLOption.setDebugOutput(true);

            CodeBuilder fwriter = new CodeBuilder(context);
//            fwriter.setWriter(writer);
            context.setCodeBuilder(fwriter);
            DbUpdater db = new DbUpdater(ft, context);
            context.setDbUpdater(db);
            context.setSourceXmlFile(m_sourceFile);

            XcodeMLVisitor visitor = new XcodeMLVisitor(context);
            if (!visitor.invokeEnter(m_program)) {
                throw new XcodeMLException(context.getLastErrorMessage(),
                        context.getLastCause());
            }
            fwriter.flush();

        } catch (XcodeMLException ex) {
            firePropertyChange("status_sub_message", null, "exception");
            firePropertyChange("prograss_clear", null, null);
            throw new LanguageException(ex, m_sourceFile);

        } catch (Exception ex) {
            firePropertyChange("status_sub_message", null, "exception");
            firePropertyChange("prograss_clear", null, null);
            throw new LanguageException(ex, m_sourceFile);
        }


        // ステータスバー：進捗メッセージ
        firePropertyChange("status_sub_message", null, "done");
        firePropertyChange("prograss_clear", null, null);
    }

    @Override
    public CodeLine[] getCodeLineList() throws InterruptedException {
        return null;
    }
}
