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

package jp.riken.kscope.properties;

import java.awt.Color;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;

import jp.riken.kscope.Message;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.XmlUtils;

/**
 * プロファイラプロパティクラス
 * @author RIKEN
 *
 */
public class ProfilerProperties extends PropertiesBase {

    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

    /* プロパティキー */
    /** コスト情報表示最大数 */
    private final String KEY_COSTINFO_MAXCOUNT = "costinfo-maxcount";
    /** コスト情報表示色:手続 */
    private final String KEY_COSTINFO_BARCOLOR_PROCEDURE = "costinfo-barcolor-procedure";
    /** コスト情報表示色:ループ */
    private final String KEY_COSTINFO_BARCOLOR_LOOP = "costinfo-barcolor-loop";
    /** コスト情報表示色:ライン */
    private final String KEY_COSTINFO_BARCOLOR_LINE = "costinfo-barcolor-line";
    /** コストルーラ:最小色 */
    private final String KEY_RULER_COLOR_MIN = "costruler-color-minimum";
    /** コストルーラ:最大色 */
    private final String KEY_RULER_COLOR_MAX = "costruler-color-maximum";
    /** コストルーラ:コード枠色 */
    private final String KEY_RULER_BORDERCOLOR_PANEL = "costruler-bordercolor-panel";
    /** コストルーラ:コード背景色 */
    private final String KEY_RULER_BACKCOLOR_PANEL = "costruler-backcolor-panel";
    /** 測定区間ステートメント:開始関数名 */
    private final String KEY_EPROF_FUNCTION_START = "eprof-function-start";
    /** 測定区間ステートメント:終了関数名 */
    private final String KEY_EPROF_FUNCTION_END = "eprof-function-end";
    /** 測定区間ステートメント:開始ステートメント */
    private final String KEY_EPROF_STATEMENT_START = "eprof-statement-start";
    /** 測定区間ステートメント:終了ステートメント */
    private final String KEY_EPROF_STATEMENT_END = "eprof-statement-end";
    /** コスト情報表示最大数:デフォルト=0*/
    private final int DEFAULT_COSTINFO_MAXNO = 0;
    /** プロファイラコストバーグラフ表示:初期状態 */
    public static boolean INITIALIZE_VISIBLE_BARGRAPH = false;
    /** プロファイラコストバーグラフ表示:初期状態false(=非表示) */
    private boolean visibleBargraph = INITIALIZE_VISIBLE_BARGRAPH;
    /** プロファイラコストルーラ表示:初期状態 */
    public static boolean INITIALIZE_VISIBLE_RULER = false;
    /** プロファイラコストルーラ表示:初期状態false(=非表示) */
    private boolean visibleRuler = INITIALIZE_VISIBLE_RULER;
    /** Eprof測定区間マクロ:関数名 */
    private final String MACRO_EPROF_STATEMENT_FUNCTION = "%FUNCTION";
    /** Eprof測定区間マクロ:グループ名 */
    private final String MACRO_EPROF_STATEMENT_NAME = "%NAME";
    /** Eprof測定区間マクロ:詳細番号 */
    private final String MACRO_EPROF_STATEMENT_NUMBER = "%NUMBER";
    /** Eprof測定区間マクロ:プライオリティレベル */
    private final String MACRO_EPROF_STATEMENT_LEVEL= "%LEVEL";
    /** コスト表示桁数(小数点以下桁数) */
    public static final int COST_RATIO_SCALE = 2;
    /** コストルーラ:最小値色 */
    private static final Color RULER_MIN_DEFAULTCOLOR = Color.BLUE;
    /** コストルーラ:最大値色 */
    private static final Color RULER_MAX_DEFAULTCOLOR = Color.RED;
    /** コストルーラ:コード枠色 */
    private static final Color RULER_PANEL_DEFAULTBORDERCOLOR = Color.GRAY;
    /** コストルーラ:コード背景色 */
    private static final Color RULER_PANEL_DEFAULTBACKCOLOR = new Color(220, 220, 220, 64);

    /**
     * コンストラクタ
     * @throws Exception     プロパティ読込エラー
     */
    public ProfilerProperties() throws Exception {
        loadProperties();
    }

    /**
     * コピーコンストラクタ
     * @param  properties  プロファイラプロパティ
     * @throws Exception     プロパティ読込エラー
     */
    public ProfilerProperties(ProfilerProperties properties) throws Exception {
    	// メニュー表示項目をコピーする
    	if (properties != null) {
    		this.visibleBargraph = properties.visibleBargraph;
    		this.visibleRuler = properties.visibleRuler;
    	}
        loadProperties();
    }


    /**
     * プロファイラプロパティをデフォルト設定ファイルから読み込む。
     * @throws Exception     プロパティ読込エラー
     */
    public void loadProperties() throws Exception {

        // リソースファイルの読込
        InputStream stream = ResourceUtils.getPropertiesFile(KscopeProperties.PROPERTIES_FILE);
        // プロファイラプロパティを設定ファイルから読み込む。
        loadProperties(stream);
    }


    /**
     * プロファイラプロパティを設定ファイルから読み込む。
     * @param  propertiesFile 		プロファイラプロパティ設定ファイル
     * @throws Exception     プロパティ読込エラー
     */
    public void loadProperties(File propertiesFile) throws Exception {

        if (!propertiesFile.exists()) {
            throw(new Exception(Message.getString("propertiesbase.exeption.notexist"))); //プロパティファイルが存在しません。
        }

        // リソースファイルの読込
        InputStream stream = new FileInputStream(propertiesFile);

        // XMLファイルのパース
        loadProperties(stream);
    }


    /**
     * 演算カウントプロパティを設定ファイルから読み込む。
     * @param   stream      設定ファイルストリーム
     * @throws Exception     プロパティ読込エラー
     */
    public void loadProperties(InputStream stream) throws Exception {

        // XMLファイルのパース
        XmlUtils xml = new XmlUtils(stream);

        // コスト情報表示最大数
        {
            String key = KEY_COSTINFO_MAXCOUNT;
            int value = xml.getInt("//settings/profiler[@key='" + key + "']/@value");
            if (value < 0) {
            	value = getCostinfoMaxCount();
            }
            if (value < 0) {
                value = DEFAULT_COSTINFO_MAXNO;
            }
            this.setCostinfoMaxCount(value);
        }
        // コスト情報表示色:手続
        {
            String key = KEY_COSTINFO_BARCOLOR_PROCEDURE;
            Color value = xml.getColor("//settings/profiler[@key='" + key + "']/@color");
            if (value == null) {
            	value = getCostinfoBarcolorProcedure();
            }
            this.setCostinfoBarcolorProcedure(value);
        }
        // コスト情報表示色:ループ
        {
            String key = KEY_COSTINFO_BARCOLOR_LOOP;
            Color value = xml.getColor("//settings/profiler[@key='" + key + "']/@color");
            if (value == null) {
                value = this.getCostinfoBarcolorLoop();
            }
            this.setCostinfoBarcolorLoop(value);
        }
        // コスト情報表示色:ライン
        {
            String key = KEY_COSTINFO_BARCOLOR_LINE;
            Color value = xml.getColor("//settings/profiler[@key='" + key + "']/@color");
            if (value == null) {
                value = this.getCostinfoBarcolorLine();
            }
            this.setCostinfoBarcolorLine(value);
        }
        // 測定区間:開始関数名
        {
            String key = KEY_EPROF_FUNCTION_START;
            String value = xml.getString("//settings/profiler[@key='" + key + "']/@value");
            if (value == null || value.isEmpty()) {
                value = this.getEprofFunctionStart();
            }
            this.setEprofFunctionStart(value);
        }
        // 測定区間:終了関数名
        {
            String key = KEY_EPROF_FUNCTION_END;
            String value = xml.getString("//settings/profiler[@key='" + key + "']/@value");
            if (value == null || value.isEmpty()) {
                value = this.getEprofFunctionEnd();
            }
            this.setEprofFunctionEnd(value);
        }
        // 測定区間:開始ステートメント
        {
            String key = KEY_EPROF_STATEMENT_START;
            String value = xml.getString("//settings/profiler[@key='" + key + "']/text()");
            if (value == null || value.isEmpty()) {
                value = this.getEprofStatementStart();
            }
            this.setEprofStatementStart(value);
        }
        // 測定区間:終了ステートメント
        {
            String key = KEY_EPROF_STATEMENT_END;
            String value = xml.getString("//settings/profiler[@key='" + key + "']/text()");
            if (value == null || value.isEmpty()) {
                value = this.getEprofStatementEnd();
            }
            this.setEprofStatementEnd(value);
        }
        // コストルーラ:最小色
        {
            String key = KEY_RULER_COLOR_MIN;
            Color value = xml.getColor("//settings/profiler[@key='" + key + "']/@color");
            if (value == null) {
            	value = getRulerColorMin();
            }
            if (value == null) {
            	value = RULER_MIN_DEFAULTCOLOR;
            }
            this.setRulerColorMin(value);
        }
        // コストルーラ:最大色
        {
            String key = KEY_RULER_COLOR_MAX;
            Color value = xml.getColor("//settings/profiler[@key='" + key + "']/@color");
            if (value == null) {
            	value = getRulerColorMax();
            }
            if (value == null) {
            	value = RULER_MAX_DEFAULTCOLOR;
            }
            this.setRulerColorMax(value);
        }
        // コストルーラ:コード枠色
        {
            String key = KEY_RULER_BORDERCOLOR_PANEL;
            Color value = xml.getColor("//settings/profiler[@key='" + key + "']/@color");
            if (value == null) {
            	value = getRulerPanelBorderColor();
            }
            if (value == null) {
            	value = RULER_PANEL_DEFAULTBORDERCOLOR;
            }
            this.setRulerPanelBorderColor(value);
        }
        // コストルーラ:コード背景色
        {
            String key = KEY_RULER_BACKCOLOR_PANEL;
            Color value = xml.getColor("//settings/profiler[@key='" + key + "']/@color");
            if (value == null) {
            	value = getRulerPanelBackColor();
            }
            if (value == null) {
            	value = RULER_PANEL_DEFAULTBACKCOLOR;
            }
            this.setRulerPanelBackColor(value);
        }

        return;
    }

	/**
     * プロパティをDOMノードに出力する
     * @param node		出力ノード
     */
    public void writeProperties(org.w3c.dom.Element node) {

        // ドキュメントの取得
        org.w3c.dom.Document document = node.getOwnerDocument();

        // コメントを追加
        {
            org.w3c.dom.Comment comment = document.createComment(Message.getString("profilerproperties.document.comment")); //プロファイラプロパティ
            node.appendChild(comment);
        }
        // コスト情報表示最大数
        {
            String key = KEY_COSTINFO_MAXCOUNT;
            int value = this.getCostinfoMaxCount();
            org.w3c.dom.Element elem = createPropertyElement(document, key);
            org.w3c.dom.Attr attrValue = document.createAttribute("value");
            attrValue.setValue(String.valueOf(value));
            elem.setAttributeNode(attrValue);
            node.appendChild(elem);
        }
        // コスト情報表示色:手続
        {
            String key = KEY_COSTINFO_BARCOLOR_PROCEDURE;
            Color value = this.getCostinfoBarcolorProcedure();
            if (value != null){
                org.w3c.dom.Element elem = createPropertyElement(document, key);
                XmlUtils.createColorAttribute(elem, value);
                node.appendChild(elem);
            }
        }
        // コスト情報表示色:ループ
        {
            String key = KEY_COSTINFO_BARCOLOR_LOOP;
            Color value = this.getCostinfoBarcolorLoop();
            if (value != null){
                org.w3c.dom.Element elem = createPropertyElement(document, key);
                XmlUtils.createColorAttribute(elem, value);
                node.appendChild(elem);
            }
        }
        // コスト情報表示色:ライン
        {
            String key = KEY_COSTINFO_BARCOLOR_LINE;
            Color value = this.getCostinfoBarcolorLine();
            if (value != null){
                org.w3c.dom.Element elem = createPropertyElement(document, key);
                XmlUtils.createColorAttribute(elem, value);
                node.appendChild(elem);
            }
        }

        // 測定区間:開始関数名
        {
            String key = KEY_EPROF_FUNCTION_START;
            String value = this.getEprofFunctionStart();
            org.w3c.dom.Element elem = createPropertyElement(document, key);
            org.w3c.dom.Attr attrValue = document.createAttribute("value");
            attrValue.setValue(String.valueOf(value));
            elem.setAttributeNode(attrValue);
            node.appendChild(elem);
        }
        // 測定区間:終了関数名
        {
            String key = KEY_EPROF_FUNCTION_END;
            String value = this.getEprofFunctionEnd();
            org.w3c.dom.Element elem = createPropertyElement(document, key);
            org.w3c.dom.Attr attrValue = document.createAttribute("value");
            attrValue.setValue(String.valueOf(value));
            elem.setAttributeNode(attrValue);
            node.appendChild(elem);
        }
        // 測定区間:開始ステートメント
        {
            String key = KEY_EPROF_STATEMENT_START;
            String value = this.getEprofStatementStart();
            if (value != null) {
                org.w3c.dom.Element elem = createPropertyElement(document, key);
                elem.appendChild( document.createCDATASection( value ) );
                node.appendChild(elem);
            }
        }
        // 測定区間:終了ステートメント
        {
            String key = KEY_EPROF_STATEMENT_END;
            String value = this.getEprofStatementEnd();
            if (value != null) {
                org.w3c.dom.Element elem = createPropertyElement(document, key);
                elem.appendChild( document.createCDATASection( value ) );
                node.appendChild(elem);
            }
        }
        // コストルーラ:最小色
        {
            String key = KEY_RULER_COLOR_MIN;
            Color value = this.getRulerColorMin();
            if (value != null) {
                org.w3c.dom.Element elem = createPropertyElement(document, key);
                XmlUtils.createColorAttribute(elem, value);
                node.appendChild(elem);
            }
        }
        // コストルーラ:最大色
        {
            String key = KEY_RULER_COLOR_MAX;
            Color value = this.getRulerColorMax();
            if (value != null) {
                org.w3c.dom.Element elem = createPropertyElement(document, key);
                XmlUtils.createColorAttribute(elem, value);
                node.appendChild(elem);
            }
        }
        // コストルーラ:コード枠色
        {
            String key = KEY_RULER_BORDERCOLOR_PANEL;
            Color value = this.getRulerPanelBorderColor();
            if (value != null) {
                org.w3c.dom.Element elem = createPropertyElement(document, key);
                XmlUtils.createColorAttribute(elem, value);
                node.appendChild(elem);
            }
        }
        // コストルーラ:コード背景色
        {
            String key = KEY_RULER_BACKCOLOR_PANEL;
            Color value = this.getRulerPanelBackColor();
            if (value != null) {
                org.w3c.dom.Element elem = createPropertyElement(document, key);
                XmlUtils.createColorAttribute(elem, value);
                node.appendChild(elem);
            }
        }
    }

    /**
     * プロパティ要素を作成する
     * @param document		XMLドキュメント
     * @param key			key属性値
     * @return				プロパティ要素
     */
    private org.w3c.dom.Element createPropertyElement(org.w3c.dom.Document document, String key) {

        org.w3c.dom.Element elem = document.createElement("profiler");
        // プロパティキー
        {
            org.w3c.dom.Attr attr = document.createAttribute("key");
            attr.setValue(key);
            elem.setAttributeNode(attr);
        }

        return elem;
    }


    /**
     * プロパティ変更イベントを通知する。
     */
    @Override
    public void firePropertyChange() {
        this.changes.firePropertyChange(this.getClass().getName(), null, this);

    }

    /**
     * コスト表示最大数を取得する
     * @return		コスト表示最大数
     */
    public int getCostinfoMaxCount() {
        return this.getInt(KEY_COSTINFO_MAXCOUNT, DEFAULT_COSTINFO_MAXNO);
    }

    /**
     * コスト表示最大数を設定する
     * @param value		コスト表示最大数
     */
    public void setCostinfoMaxCount(int value) {
        this.putInt(KEY_COSTINFO_MAXCOUNT, value);
    }

    /**
     * コスト情報表示色:手続を取得する
     * @return		コスト情報表示色:手続
     */
    public Color getCostinfoBarcolorProcedure() {
        Object value = this.getObject(KEY_COSTINFO_BARCOLOR_PROCEDURE);
        if (value == null) return null;
        if (!(value instanceof Color)) return null;
        return (Color)value;
    }

    /**
     * コスト情報表示色:手続を設定する
     * @param value		コスト情報表示色:手続
     */
    public void setCostinfoBarcolorProcedure(Color value) {
        this.putObject(KEY_COSTINFO_BARCOLOR_PROCEDURE, value);
    }

    /**
     * コスト情報表示色:ループを取得する
     * @return		コスト情報表示色:ループ
     */
    public Color getCostinfoBarcolorLoop() {
        Object value = this.getObject(KEY_COSTINFO_BARCOLOR_LOOP);
        if (value == null) return null;
        if (!(value instanceof Color)) return null;
        return (Color)value;
    }

    /**
     * コスト情報表示色:ループを設定する
     * @param value		コスト情報表示色:ループ
     */
    public void setCostinfoBarcolorLoop(Color value) {
        this.putObject(KEY_COSTINFO_BARCOLOR_LOOP, value);
    }

    /**
     * コスト情報表示色:ラインを取得する
     * @return		コスト情報表示色:ライン
     */
    public Color getCostinfoBarcolorLine() {
        Object value = this.getObject(KEY_COSTINFO_BARCOLOR_LINE);
        if (value == null) return null;
        if (!(value instanceof Color)) return null;
        return (Color)value;
    }

    /**
     * コスト情報表示色:ラインを設定する
     * @param value		コスト情報表示色:ライン
     */
    public void setCostinfoBarcolorLine(Color value) {
        this.putObject(KEY_COSTINFO_BARCOLOR_LINE, value);
    }

    /**
     * コストルーラ:最小色を取得する
     * @return		コストルーラ:最小色
     */
    public Color getRulerColorMin() {
        Object value = this.getObject(KEY_RULER_COLOR_MIN);
        if (value == null) return null;
        if (!(value instanceof Color)) return null;
        return (Color)value;
    }

    /**
     * コストルーラ:最小色を設定する
     * @param value		コストルーラ:最小色
     */
    public void setRulerColorMin(Color value) {
        this.putObject(KEY_RULER_COLOR_MIN, value);
	}

    /**
     * コストルーラ:最大色を取得する
     * @return		コストルーラ:最大色
     */
    public Color getRulerColorMax() {
        Object value = this.getObject(KEY_RULER_COLOR_MAX);
        if (value == null) return null;
        if (!(value instanceof Color)) return null;
        return (Color)value;
    }

    /**
     * コストルーラ:最大色を設定する
     * @param value		コストルーラ:最大色
     */
    public void setRulerColorMax(Color value) {
        this.putObject(KEY_RULER_COLOR_MAX, value);
	}

    /**
     * コストルーラ:コードパネル枠色を取得する
     * @return		コストルーラ:コードパネル枠色
     */
    public Color getRulerPanelBorderColor() {
        Object value = this.getObject(KEY_RULER_BORDERCOLOR_PANEL);
        if (value == null) return null;
        if (!(value instanceof Color)) return null;
        return (Color)value;
    }

    /**
     * コストルーラ:コードパネル枠色を設定する
     * @param value		コストルーラ:コードパネル枠色
     */
    public void setRulerPanelBorderColor(Color value) {
        this.putObject(KEY_RULER_BORDERCOLOR_PANEL, value);
	}

    /**
     * コストルーラ:コードパネル背景色を取得する
     * @return		コストルーラ:コードパネル背景色
     */
    public Color getRulerPanelBackColor() {
        Object value = this.getObject(KEY_RULER_BACKCOLOR_PANEL);
        if (value == null) return null;
        if (!(value instanceof Color)) return null;
        return (Color)value;
    }

    /**
     * コストルーラ:コードパネル背景色を設定する
     * @param value		コストルーラ:コードパネル背景色
     */
    public void setRulerPanelBackColor(Color value) {
        this.putObject(KEY_RULER_BACKCOLOR_PANEL, value);
	}

    /**
     * 測定区間:開始関数名を取得する
     * @return		測定区間:開始関数名
     */
    public String getEprofFunctionStart() {
        String value = this.getProperty(KEY_EPROF_FUNCTION_START);
        return value;
    }

    /**
     * 測定区間:開始関数名を設定する
     * @param value		測定区間:開始関数名
     */
    public void setEprofFunctionStart(String value) {
        this.put(KEY_EPROF_FUNCTION_START, value);
    }

    /**
     * 測定区間:終了関数名を取得する
     * @return		測定区間:終了関数名
     */
    public String getEprofFunctionEnd() {
        String value = this.getProperty(KEY_EPROF_FUNCTION_END);
        return value;
    }

    /**
     * 測定区間:終了関数名を設定する
     * @param value		測定区間:終了関数名
     */
    public void setEprofFunctionEnd(String value) {
        this.put(KEY_EPROF_FUNCTION_END, value);
    }

    /**
     * 測定区間:開始ステートメントを取得する
     * @return		測定区間:開始ステートメント
     */
    public String getEprofStatementStart() {
        String value = this.getProperty(KEY_EPROF_STATEMENT_START);
        return value;
    }

    /**
     * 測定区間:開始ステートメントを設定する
     * @param value		測定区間:開始ステートメント
     */
    public void setEprofStatementStart(String value) {
        this.put(KEY_EPROF_STATEMENT_START, value);
    }


    /**
     * 測定区間:終了ステートメントを取得する
     * @return		測定区間:終了ステートメント
     */
    public String getEprofStatementEnd() {
        String value = this.getProperty(KEY_EPROF_STATEMENT_END);
        return value;
    }

    /**
     * 測定区間:終了ステートメントを設定する
     * @param value		測定区間:終了ステートメント
     */
    public void setEprofStatementEnd(String value) {
        this.put(KEY_EPROF_STATEMENT_END, value);
    }


    /**
     * プロファイラコストバーグラフ表示
     * @return プロファイラコストバーグラフ表示
     */
    public boolean isVisibleBargraph() {
        return visibleBargraph;
    }

    /**
     * プロファイラコストバーグラフ表示
     * @param visible プロファイラコストバーグラフ表示
     */
    public void setVisibleBargraph(boolean visible) {
        this.visibleBargraph = visible;
    }

    /**
     * プロファイラコストルーラ表示
     * @return プロファイラコストルーラ表示
     */
    public boolean isVisibleRuler() {
        return visibleRuler;
    }

    /**
     * プロファイラコストルーラ表示
     * @param visible プロファイラコストルーラ表示
     */
    public void setVisibleRuler(boolean visible) {
        this.visibleRuler = visible;
    }


    /**
     * 測定区間ステートメントにグループ名マクロが含まれているかチェックする
     * @return		true=グループ名マクロが含まれている
     */
    public boolean existsMacroErofName() {
        return existsMacroErof(MACRO_EPROF_STATEMENT_NAME);
    }

    /**
     * 測定区間ステートメントに詳細番号マクロが含まれているかチェックする
     * @return		true=詳細番号マクロが含まれている
     */
    public boolean existsMacroErofNumber() {
        return existsMacroErof(MACRO_EPROF_STATEMENT_NUMBER);
    }

    /**
     * 測定区間ステートメントにプライオリティレベルマクロが含まれているかチェックする
     * @return		true=プライオリティレベルマクロが含まれている
     */
    public boolean existsMacroErofLevel() {
        return existsMacroErof(MACRO_EPROF_STATEMENT_LEVEL);
    }


    /**
     * 測定区間ステートメントにマクロが含まれているかチェックする
     * @return		true=マクロが含まれている
     */
    private boolean existsMacroErof(String macro) {
        boolean exists = false;
        {
            String statement = this.getEprofStatementStart();
            if (statement != null && !statement.isEmpty()) {
                exists |= (statement.indexOf(macro) >= 0);
            }
        }
        {
            String statement = this.getEprofStatementEnd();
            if (statement != null && !statement.isEmpty()) {
                exists |= (statement.indexOf(macro) >= 0);
            }
        }
        return exists;
    }

    /**
     * 測定区間:開始ステートメントを作成する
     * @param name			グループ名
     * @param number		詳細番号
     * @param level			プライオリティレベル
     * @return		測定区間:開始ステートメント
     */
    public String createEprofStatementStart(String name, String number, String level) {
        return createEprofStatement(this.getEprofStatementStart(), this.getEprofFunctionStart(), name, number, level);
    }

    /**
     * 測定区間:終了ステートメントを作成する
     * @param name			グループ名
     * @param number		詳細番号
     * @param level			プライオリティレベル
     * @return		測定区間:終了ステートメント
     */
    public String createEprofStatementEnd(String name, String number, String level) {
        return createEprofStatement(this.getEprofStatementEnd(), this.getEprofFunctionEnd(), name, number, level);
    }

    /**
     * 測定区間:開始ステートメントを作成する
     * @param statement		挿入ステートメント
     * @param function		関数名
     * @param name			グループ名
     * @param number		詳細番号
     * @param level			プライオリティレベル
     * @return		測定区間:開始ステートメント
     */
    private String createEprofStatement(String statement, String function, String name, String number, String level) {
        if (statement == null || statement.isEmpty()) return null;
        String value = statement;
        // 関数名
        if (function != null && !function.isEmpty()) {
            value = value.replaceAll(MACRO_EPROF_STATEMENT_FUNCTION, function);
        }
        else {
            value = value.replaceAll(MACRO_EPROF_STATEMENT_FUNCTION, "");
        }
        // グループ名
        if (name != null && !name.isEmpty()) {
            String rep_name = name;
            // 削除 2012/05/21 ダブルクォートで囲んでいたらダブルクォートは付けない。
//            if (!(rep_name.startsWith("\"") && rep_name.endsWith("\""))) {
//                rep_name = "\"" + rep_name + "\"";
//            }
            value = value.replaceAll(MACRO_EPROF_STATEMENT_NAME, rep_name);
        }
        else {
            value = value.replaceAll(MACRO_EPROF_STATEMENT_NAME, "");
        }
        // 詳細番号
        if (number != null && !number.isEmpty()) {
            value = value.replaceAll(MACRO_EPROF_STATEMENT_NUMBER, number);
        }
        else {
            value = value.replaceAll(MACRO_EPROF_STATEMENT_NUMBER, "");
        }
        // レベル
        if (level != null && !level.isEmpty()) {
            value = value.replaceAll(MACRO_EPROF_STATEMENT_LEVEL, level);
        }
        else {
            value = value.replaceAll(MACRO_EPROF_STATEMENT_LEVEL, "");
        }
        return value;
    }


    /**
     * プロファイラメニュー表示項目をコピーする.
     * @param  properties  プロファイラプロパティ
     */
    public void setVisibleProperties(ProfilerProperties properties) {
    	// メニュー表示項目をコピーする
    	if (properties != null) {
    		this.visibleBargraph = properties.visibleBargraph;
    		this.visibleRuler = properties.visibleRuler;
    	}
    }
}



