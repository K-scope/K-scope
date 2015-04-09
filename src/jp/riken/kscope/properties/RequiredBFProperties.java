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
import java.util.ArrayList;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathFactory;

import jp.riken.kscope.Message;
import jp.riken.kscope.common.ACCESSMEMORY_TYPE;
import jp.riken.kscope.data.RequiredBF;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.StringUtils;

import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * 要求Byte/FLOP設定プロパティ
 * @author RIKEN
 */
public class RequiredBFProperties  extends PropertiesBase {
    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

	/** メモリスループット算出モード */
	public enum MEM_THROUGHPUT_CALC_MODE {
		AUTO,		///< 自動判定：デフォルト
		STORE,		///< ストアあり
		NOSTORE	    ///< ストアなし
	}

	/** 要求BFの算出単位 */
	public enum BF_CALC_TYPE {
		BYTE_FLOP,		///< Byte/FLOP
		FLOP_BYTE		///< FLOP/Byte
	}

	/** デフォルトデータ型サイズ */
	public final int DEFUALT_DATASIZE = 4;
    /* プロパティキー */
    /** 要求Byte/FLOP要素 */
    private final String ELEM_REQUIRED_BF = "required_bf";
    /** 演算性能 GFLOPS */
    private final String KEY_FLOP_PERFORMANCE = "flop-performance";
    /** メモリスループット算出モード（スループット算出時にストアを考慮するかの選択） */
    private final String KEY_MEM_THROUGHPUT_CALC_MODE = "mem-throughput-calc_mode";
    /** 要求BF算出の単位 */
    private final String KEY_BF_CALC_TYPE = "calc-type";
    /** デフォルトサイズ */
    private final String KEY_DEFAULT_SIZE = "default-size";
    /** アクセス先キー属性 */
    private final String ATTR_KEY = "key";
    /** 値属性 */
    private final String ATTR_VALUE = "value";
    /** アクセス先名称属性 */
    private final String ATTR_NAME = "name";
    /** アクセス先背景色 */
    private final String ATTR_BACKGROUND_COLOR = "background-color";
    /** メモリスループット:ストアあり */
    private final String ATTR_MEM_THROUGHPUT_STORE = "mem-throughput-store";
    /** メモリスループット:ストアなし */
    private final String ATTR_MEM_THROUGHPUT_NOSTORE = "mem-throughput-nostore";
    /** 係数 */
    private final String ATTR_COEFFICIENT = "coef";
    /** 要求B/F算出 */
    private final String ATTR_REQUIRED_BF = "reqbf";
    /** 律速 */
    private final String ATTR_LIMITS = "limits";
    /** 有効/無効 */
    private final String ATTR_ENABLED = "enabled";
    /** デフォルトサイズ：real */
    private final String ATTR_SIZE_REAL = "size-real";
    /** デフォルトサイズ：integer */
    private final String ATTR_SIZE_INTEGER = "size-integer";

    /** 理論浮動小数点数演算性能 */
    private float theoretical_flop_performance;
    /** メモリスループット算出モード設定 */
    private MEM_THROUGHPUT_CALC_MODE memThroughtputCalcMode;
    /** BF算出単位 */
    private BF_CALC_TYPE BFCalcType;
    /** デフォルトサイズ:real (byte) */
    private int defaultSizeReal;
    /** デフォルトサイズ:integer (byte) */
    private int defaultSizeInteger;
    /** 要求Byte/FLOP設定リスト */
    private List<RequiredBF> listReqBF= new ArrayList<RequiredBF>();
    /** デフォルト要求Byte/FLOP設定 */
    private RequiredBFProperties defaultProperties;

    /**
     * コンストラクタ
     * @throws Exception     プロパティ読込エラー
     */
    public RequiredBFProperties() throws Exception {
    	// デフォルト値
    	this.memThroughtputCalcMode = MEM_THROUGHPUT_CALC_MODE.AUTO;
    	this.BFCalcType = BF_CALC_TYPE.BYTE_FLOP;
    	this.defaultSizeReal = DEFUALT_DATASIZE;
    	this.defaultSizeInteger = DEFUALT_DATASIZE;
        loadProperties();
    }

    /**
     * ソース設定プロパティをデフォルト設定ファイルから読み込む。
     * @throws Exception     プロパティ読込エラー
     */
    public void loadProperties() throws Exception {
        InputStream is = null;

        // リソースファイルの読込
        is = ResourceUtils.getPropertiesFile(KscopeProperties.PROPERTIES_FILE);

        loadProperties(is);
    }

    /**
     * 要求Byte/FLOP設定プロパティを設定ファイルから読み込む。
     * @param  propertiesFile 		要求Byte/FLOP設定プロパティ設定ファイル
     * @throws Exception     プロパティ読込エラー
     */
    public void loadProperties(File propertiesFile) throws Exception {

        if (!propertiesFile.exists()) {
            throw(new Exception(Message.getString("propertiesbase.exeption.notexist"))); //要求Byte/FLOPプロパティファイルが存在しません。
        }

        // リソースファイルの読込
        InputStream stream = new FileInputStream(propertiesFile);

        // XMLファイルのパース
        loadProperties(stream);
    }

    /**
     * 要求Byte/FLOP設定プロパティを設定ファイルから読み込む。
     * @param   stream      設定ファイルストリーム
     * @throws Exception     プロパティ読込エラー
     */
    public void loadProperties(InputStream stream ) throws Exception {
        // XMLファイルのパース
    	List<RequiredBF> list = parseRequiredBF(stream, "//" + ELEM_REQUIRED_BF);
    	if (list != null && list.size() > 0) {
    		this.listReqBF = list;
    	}
    }

    /**
     * 要求Byte/FLOP設定を取得する
     * @param stream		XML入力ストリーム
     * @param path		要求Byte/FLOP設定XPATH
     * @throws Exception 		要求Byte/FLOP設定パースエラー
     */
    public List<RequiredBF> parseRequiredBF(InputStream stream, String path) throws Exception {

        List<RequiredBF> list = new ArrayList<RequiredBF>();

        // XML
        DocumentBuilderFactory dbfactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder builder = dbfactory.newDocumentBuilder();
        org.w3c.dom.Document document = builder.parse(stream);

        XPathFactory factory = XPathFactory.newInstance();
        XPath xpath = factory.newXPath();

        // 要求BF要素の取得
        XPathExpression expr = xpath.compile(path);

        Object result = expr.evaluate(document, XPathConstants.NODESET);

        NodeList nodelist = (NodeList) result;
        if (nodelist == null) return null;

        for (int i=0; i<nodelist.getLength(); i++) {
            try {
                Node node = nodelist.item(i);
                RequiredBF reqbf = null;

                // 属性の取得
                NamedNodeMap attrs = node.getAttributes();
                Node attrnode;
                String value;
                // キー
                attrnode = attrs.getNamedItem(ATTR_KEY);
                if (attrnode == null) continue;
                String key = attrnode.getNodeValue();
                // 浮動小数点数演算性能
                if (KEY_FLOP_PERFORMANCE.equalsIgnoreCase(key)) {
                    attrnode = attrs.getNamedItem(ATTR_VALUE);
                    value = attrnode.getNodeValue();
                    if (StringUtils.isFloat(value)) {
                        this.setFlopPerformance(Float.parseFloat(value));
                    }
                    continue;
                }
                // メモリスループット算出モード
                else if (KEY_MEM_THROUGHPUT_CALC_MODE.equalsIgnoreCase(key)) {
                    attrnode = attrs.getNamedItem(ATTR_VALUE);
                    value = attrnode.getNodeValue();
                    try {
						this.setMemThroughputCalcMode(MEM_THROUGHPUT_CALC_MODE.valueOf(value.toUpperCase()));
					} catch (Exception ex) {
						ex.printStackTrace();
					}
                    continue;
                }
                // BF算出単位
                else if (KEY_BF_CALC_TYPE.equalsIgnoreCase(key)) {
                    attrnode = attrs.getNamedItem(ATTR_VALUE);
                    value = attrnode.getNodeValue();
                    try {
						this.setCalcType(BF_CALC_TYPE.valueOf(value.toUpperCase()));
					} catch (Exception ex) {
						ex.printStackTrace();
					}
                    continue;
                }
                // デフォルトサイズ
                else if (KEY_DEFAULT_SIZE.equalsIgnoreCase(key)) {
                	// real
                    attrnode = attrs.getNamedItem(ATTR_SIZE_REAL);
                    value = attrnode.getNodeValue();
                    if (StringUtils.isNumeric(value)) {
                        this.setDefaultSizeReal(Integer.parseInt(value));
                    }
                	// integer
                    attrnode = attrs.getNamedItem(ATTR_SIZE_INTEGER);
                    value = attrnode.getNodeValue();
                    if (StringUtils.isNumeric(value)) {
                        this.setDefaultSizeInteger(Integer.parseInt(value));
                    }
                    continue;
                }
                // Memory
                else if (ACCESSMEMORY_TYPE.MEMORY.getKey().equalsIgnoreCase(key)) {
                    reqbf = new RequiredBF(ACCESSMEMORY_TYPE.MEMORY);
                }
                // L1 cache
                else if (ACCESSMEMORY_TYPE.L1_CACHE.getKey().equalsIgnoreCase(key)) {
                    reqbf = new RequiredBF(ACCESSMEMORY_TYPE.L1_CACHE);
                }
                // L2 cache
                else if (ACCESSMEMORY_TYPE.L2_CACHE.getKey().equalsIgnoreCase(key)) {
                    reqbf = new RequiredBF(ACCESSMEMORY_TYPE.L2_CACHE);
                }
                // Register
                else if (ACCESSMEMORY_TYPE.REGISTER.getKey().equalsIgnoreCase(key)) {
                    reqbf = new RequiredBF(ACCESSMEMORY_TYPE.REGISTER);
                }
                // custom
                else if (ACCESSMEMORY_TYPE.CUSTOM.getKey().equalsIgnoreCase(key)) {
                    reqbf = new RequiredBF(ACCESSMEMORY_TYPE.CUSTOM);
                }
                if (reqbf == null) continue;

                // アクセス先名称
                attrnode = attrs.getNamedItem(ATTR_NAME);
                if (attrnode != null) {
                	String res = attrnode.getNodeValue();
                    reqbf.setName(res);
                }

                // アクセス先背景色
                attrnode = attrs.getNamedItem(ATTR_BACKGROUND_COLOR);
                if (attrnode != null) {
                    value = attrnode.getNodeValue();
                    Color res = StringUtils.parseColor(value);
                    reqbf.setBackColor(res);
                }

                // メモリスループット:ストアあり
                attrnode = attrs.getNamedItem(ATTR_MEM_THROUGHPUT_STORE);
                if (attrnode != null) {
                    value = attrnode.getNodeValue();
                    if (StringUtils.isFloat(value)) {
                    	reqbf.setMemThroughputStore(Float.parseFloat(value));
                    }
                }

                // メモリスループット:ストアなし
                attrnode = attrs.getNamedItem(ATTR_MEM_THROUGHPUT_NOSTORE);
                if (attrnode != null) {
                    value = attrnode.getNodeValue();
                    if (StringUtils.isFloat(value)) {
                    	reqbf.setMemThroughputNostore(Float.parseFloat(value));
                    }
                }

                // 係数
                attrnode = attrs.getNamedItem(ATTR_COEFFICIENT);
                if (attrnode != null) {
                    value = attrnode.getNodeValue();
                    if (StringUtils.isFloat(value)) {
                    	reqbf.setCoef(Float.parseFloat(value));
                    }
                }

                // 要求B/F算出フラグ
                attrnode = attrs.getNamedItem(ATTR_REQUIRED_BF);
                if (attrnode != null) {
                    value = attrnode.getNodeValue();
                    boolean res = Boolean.parseBoolean(value);
                    reqbf.setRequiredBF(res);
                }

                // 律速フラグ
                attrnode = attrs.getNamedItem(ATTR_LIMITS);
                if (attrnode != null) {
                    value = attrnode.getNodeValue();
                    boolean res = Boolean.parseBoolean(value);
                    reqbf.setLimiting(res);
                }

                // 有効・無効
                attrnode = attrs.getNamedItem(ATTR_ENABLED);
                if (attrnode != null) {
                    value = attrnode.getNodeValue();
                    boolean res = Boolean.parseBoolean(value);
                    reqbf.setEnabled(res);
                }

                list.add(reqbf);

            } catch (Exception ex) {
                ex.printStackTrace();
            }
        }

        if (list.size() <= 0) {
            list = null;
        }

        return list;
    }

    /**
     * プロパティをDOMノードに出力する
     * @param node		出力ノード
     */
    public void writeProperties(org.w3c.dom.Node node) {

        // ドキュメントの取得
        org.w3c.dom.Document document = node.getOwnerDocument();

        // コメントを追加
        {
            org.w3c.dom.Comment comment = document.createComment(Message.getString("requiredbfproperties.document.comment")); //要求Byte/FLOPプロパティ
            node.appendChild(comment);
        }

        if (this.listReqBF == null || this.listReqBF.size() <= 0) return;

        // 浮動小数点数演算性能 GFLOPS
        {
            org.w3c.dom.Element elem = document.createElement(ELEM_REQUIRED_BF);
            org.w3c.dom.Attr attrKey = document.createAttribute(ATTR_KEY);
            attrKey.setValue(KEY_FLOP_PERFORMANCE);
            elem.setAttributeNode(attrKey);

        	float value = this.getFlopPerformance();
            org.w3c.dom.Attr attrValue = document.createAttribute(ATTR_VALUE);
            attrValue.setNodeValue(String.valueOf(value));
            elem.setAttributeNode(attrValue);

            // ノード追加
            node.appendChild(elem);
        }
        // メモリスループット算出モード
        {
            org.w3c.dom.Element elem = document.createElement(ELEM_REQUIRED_BF);
            org.w3c.dom.Attr attrKey = document.createAttribute(ATTR_KEY);
            attrKey.setValue(KEY_MEM_THROUGHPUT_CALC_MODE);
            elem.setAttributeNode(attrKey);

            MEM_THROUGHPUT_CALC_MODE value = this.getMemThroughputCalcMode();
            org.w3c.dom.Attr attrValue = document.createAttribute(ATTR_VALUE);
            attrValue.setNodeValue(value.toString().toLowerCase());
            elem.setAttributeNode(attrValue);

            // ノード追加
            node.appendChild(elem);
        }
        // 要求BFの算出単位
        {
            org.w3c.dom.Element elem = document.createElement(ELEM_REQUIRED_BF);
            org.w3c.dom.Attr attrKey = document.createAttribute(ATTR_KEY);
            attrKey.setValue(KEY_BF_CALC_TYPE);
            elem.setAttributeNode(attrKey);

            BF_CALC_TYPE value = this.getBFCalcType();
            org.w3c.dom.Attr attrValue = document.createAttribute(ATTR_VALUE);
            attrValue.setNodeValue(value.toString().toLowerCase());
            elem.setAttributeNode(attrValue);

            // ノード追加
            node.appendChild(elem);
        }
        // デフォルトサイズ
        {
            org.w3c.dom.Element elem = document.createElement(ELEM_REQUIRED_BF);
            org.w3c.dom.Attr attrKey = document.createAttribute(ATTR_KEY);
            attrKey.setValue(KEY_DEFAULT_SIZE);
            elem.setAttributeNode(attrKey);
            // real
            int real = this.getDefaultSizeReal();
            org.w3c.dom.Attr attrReal = document.createAttribute(ATTR_SIZE_REAL);
            attrReal.setNodeValue(String.valueOf(real));
            elem.setAttributeNode(attrReal);
            // integer
            int integer = this.getDefaultSizeInteger();
            org.w3c.dom.Attr attrInteger = document.createAttribute(ATTR_SIZE_INTEGER);
            attrInteger.setNodeValue(String.valueOf(integer));
            elem.setAttributeNode(attrInteger);

            // ノード追加
            node.appendChild(elem);
        }
        // 要求Byte/FLOP設定
        for (RequiredBF bind : this.listReqBF) {
            org.w3c.dom.Element elem = document.createElement(ELEM_REQUIRED_BF);

            // キー名
            {
                org.w3c.dom.Attr attr;
                attr = document.createAttribute(ATTR_KEY);
                attr.setValue(bind.getType().getKey());
                elem.setAttributeNode(attr);
            }
            // アクセス先名称
            {
                org.w3c.dom.Attr attr;
                attr = document.createAttribute(ATTR_NAME);
                attr.setValue(bind.getName());
                elem.setAttributeNode(attr);

            }
            // アクセス先背景色
            {
                Color color = bind.getBackColor();
                org.w3c.dom.Attr attr = document.createAttribute(ATTR_BACKGROUND_COLOR);
                attr.setNodeValue(StringUtils.parseColorCode(color));
                elem.setAttributeNode(attr);
            }

            // メモリスループット算出モード:ストアあり
            {
            	float value = bind.getMemThroughputStore();
                org.w3c.dom.Attr attr = document.createAttribute(ATTR_MEM_THROUGHPUT_STORE);
                attr.setNodeValue(String.valueOf(value));
                elem.setAttributeNode(attr);
            }
            // メモリスループット算出モード:ストアなし
            {
            	float value = bind.getMemThroughputNostore();
                org.w3c.dom.Attr attr = document.createAttribute(ATTR_MEM_THROUGHPUT_NOSTORE);
                attr.setNodeValue(String.valueOf(value));
                elem.setAttributeNode(attr);
            }
            // 係数
            {
            	float value = bind.getCoef();
                org.w3c.dom.Attr attr = document.createAttribute(ATTR_COEFFICIENT);
                attr.setNodeValue(String.valueOf(value));
                elem.setAttributeNode(attr);
            }
            // 要求B/F算出（有効・無効）
            {
            	boolean value = bind.isRequiredBF();
                org.w3c.dom.Attr attr = document.createAttribute(ATTR_REQUIRED_BF);
                attr.setNodeValue(String.valueOf(value));
                elem.setAttributeNode(attr);
            }
            // 律速
            {
            	boolean value = bind.isLimiting();
                org.w3c.dom.Attr attr = document.createAttribute(ATTR_LIMITS);
                attr.setNodeValue(String.valueOf(value));
                elem.setAttributeNode(attr);
            }
            // 有効/無効
            {
            	boolean value = bind.isEnabled();
                org.w3c.dom.Attr attr = document.createAttribute(ATTR_ENABLED);
                attr.setNodeValue(String.valueOf(value));
                elem.setAttributeNode(attr);
            }

            // ノード追加
            node.appendChild(elem);
        }
    }

    /**
     * プロパティ変更イベントを通知する。
     */
    @Override
    public void firePropertyChange() {
        this.changes.firePropertyChange(this.getClass().getName(), null, this);
    }

    /**
     * 要求Byte/FLOP設定リストを取得する。
     * @return		要求Byte/FLOP設定リスト
     */
    public List<RequiredBF> getListRequiredBF() {
        return this.listReqBF;
    }

    /**
     * 要求Byte/FLOP設定数を取得する。
     * @return		要求Byte/FLOP設定数
     */
    public int getRequiredBFCount() {
        if (listReqBF == null || listReqBF.size() <= 0) {return 0;}
        return listReqBF.size();
    }

    /**
     * 要求Byte/FLOP設定を取得する。
     * @param	index		インデックス
     * @return		要求Byte/FLOP設定
     */
    public RequiredBF getRequiredBF(int index) {
        if (listReqBF == null || listReqBF.size() <= 0) {return null;}
        if (listReqBF.size() <= index) {return null;}
        return listReqBF.get(index);
    }

    /**
     * 要求Byte/FLOP設定を設定する。
     * @param	index		インデックス
     * @param	keyword		要求Byte/FLOP設定
     */
    public void setRequiredBF(int index, RequiredBF reqbf) {
        if (listReqBF == null || listReqBF.size() <= 0) {return;}
        if (listReqBF.size() <= index) {return;}
        listReqBF.set(index, reqbf);
    }

    /**
     * 要求Byte/FLOP設定を追加する。
     * @param	keyword		要求Byte/FLOP設定
     */
    public void addRequiredBF(RequiredBF reqbf) {
        if (listReqBF == null) {
        	listReqBF = new ArrayList<RequiredBF>();
        }
        listReqBF.add(reqbf);
    }

    /**
     * 要求Byte/FLOP設定を削除する。
     * @param	keyword		要求Byte/FLOP設定
     */
    public void removeRequiredBF(RequiredBF reqbf) {
        if (listReqBF == null) return;
        listReqBF.remove(reqbf);
    }

    /**
     * 要求Byte/FLOP設定を削除する。
     * @param	index		インデックス
     */
    public void removeRequiredBF(int index) {
        if (listReqBF == null) return;
        listReqBF.remove(index);
    }

    /**
     * 要求Byte/FLOP設定リストをクリアする。
     */
    public void clearRequiredBF() {
    	listReqBF = new ArrayList<RequiredBF>();
    }

    /**
     * 要求Byte/FLOP設定を取得する
     * @param type		要求Byte/FLOPタイプ
     * @return		Keyword情報
     */
    public RequiredBF getRequiredBF(ACCESSMEMORY_TYPE type) {
        if (type == null) return null;

        for (RequiredBF reqbf : listReqBF) {
        	ACCESSMEMORY_TYPE srctype = reqbf.getType();
            if (srctype == type) {
                return reqbf;
            }
        }

        return null;
    }

    /**
     * 理論浮動小数点数演算性能を取得する
     * @return		演算性能
     */
	public float getFlopPerformance() {
		return this.theoretical_flop_performance;
	}

	/**
	 * 理論浮動小数点数演算性能を設定する.
	 * @param performance	演算性能
	 */
	public void setFlopPerformance(float performance) {
		this.theoretical_flop_performance = performance;
	}


    /**
     * デフォルト要求Byte/FLOP設定を取得する.
     * @return   デフォルト要求Byte/FLOP設定
     */
    public RequiredBFProperties getDefaultProperties() {
    	return this.defaultProperties;
    }

    /**
     * デフォルト要求Byte/FLOP設定を設定する.
     * @return   デフォルト要求Byte/FLOP設定
     */
    public void setDefaultProperties(RequiredBFProperties properties) {
    	this.defaultProperties = properties;
    }

	/**
	 * スループット算出モード設定を取得する.
	 * @return		スループット算出モード設定
	 */
	public MEM_THROUGHPUT_CALC_MODE getMemThroughputCalcMode() {
		return memThroughtputCalcMode;
	}

	/**
	 * スループット算出モードを設定する
	 * @param memThroughputCalcMode		スループット算出モード設定
	 */
	public void setMemThroughputCalcMode(MEM_THROUGHPUT_CALC_MODE memThroughputCalcMode) {
		this.memThroughtputCalcMode = memThroughputCalcMode;
	}

	/**
	 * 要求BF算出の単位を取得する.
	 * @return		算出単位
	 */
	public BF_CALC_TYPE getBFCalcType() {
		return BFCalcType;
	}

	/**
	 * 要求BF算出の単位を設定する.
	 * @param BFCalcType		算出単位
	 */
	public void setCalcType(BF_CALC_TYPE BFCalcType) {
		this.BFCalcType = BFCalcType;
	}

	/**
	 * デフォルトサイズ：realを取得する.
	 * @return デフォルトサイズ：real
	 */
	public int getDefaultSizeReal() {
		return this.defaultSizeReal;
	}

	/**
	 * デフォルトサイズ：realを設定する.
	 * @param size デフォルトサイズ：real
	 */
	public void setDefaultSizeReal(int size) {
		this.defaultSizeReal = size;
	}

	/**
	 * デフォルトサイズ：integerを取得する.
	 * @return デフォルトサイズ：integer
	 */
	public int getDefaultSizeInteger() {
		return this.defaultSizeInteger;
	}

	/**
	 * デフォルトサイズ：integerを設定する.
	 * @param size デフォルトサイズ：integer
	 */
	public void setDefaultSizeInteger(int size) {
		this.defaultSizeInteger = size;
	}

	/**
	 * スループット算出モード：ストア有りを取得する.
	 * @return    スループット：ストア有り
	 */
	public float getMemThroughputStore() {
		int count = getRequiredBFCount();
		float value = 0.0F;
		for (int i=0; i<count; i++) {
			RequiredBF mem = getRequiredBF(i);
			float throughput = mem.getMemThroughputStore();
			float coef = mem.getCoef();
			value += throughput * coef;
		}
		return value;
	}

	/**
	 * スループット算出モード：ストア有りを取得する.
	 * @return    スループット：ストア有り
	 */
	public float getMemThroughputNostore() {
		int count = getRequiredBFCount();
		float value = 0.0F;
		for (int i=0; i<count; i++) {
			RequiredBF mem = getRequiredBF(i);
			float throughput = mem.getMemThroughputNostore();
			float coef = mem.getCoef();
			value += throughput * coef;
		}
		return value;
	}
}

