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
import jp.riken.kscope.data.Memoryband;
import jp.riken.kscope.utils.ResourceUtils;
import jp.riken.kscope.utils.StringUtils;

import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * 要求Byte/FLOP設定プロパティ
 * @author riken
 */
public class MemorybandProperties  extends PropertiesBase {
    /** シリアル番号 */
    private static final long serialVersionUID = 1L;

	/** 実効スループット算出モード */
	public enum THROUGHPUT_STORE_MODE {
		AUTO,		///< 自動判定：デフォルト
		STORE,		///< ストアあり
		NONESTORE	///< ストアなし
	}

	/** 算出単位 */
	public enum UNIT_TYPE {
		BYTE_FLOP,		///< Byte/FLOP
		FLOP_BYTE		///< FLOP/Byte
	}

	/** デフォルトデータ型サイズ */
	public final int DEFUALT_DATASIZE = 4;
    /* プロパティキー */
    /** 要求Byte/FLOP要素 */
    private final String ELEM_MEMORYBAND = "memoryband";
    /** 演算性能 GFLOPS */
    private final String KEY_OPERATION_PERFORMANCE = "operation-performance";
    /** スループットストアモード */
    private final String KEY_THROUGHPUT_STORE_MODE = "throughput-store-mode";
    /** 算出単位 */
    private final String KEY_UNIT_TYPE = "unit-type";
    /** デフォルトサイズ */
    private final String KEY_DEFAULT_SIZE = "default-size";
    /** アクセス先キー属性 */
    private final String ATTR_KEY = "key";
    /** 値属性 */
    private final String ATTR_VALUE = "value";
    /** アクセス先名称属性 */
    private final String ATTR_NAME = "name";
    /** アクセス先背景色 */
    private final String ATTR_BACKCOLOR = "backcolor";
    /** スループット:ストアあり */
    private final String ATTR_THROUGHPUT_STORE = "throughput-store";
    /** スループット:ストアなし */
    private final String ATTR_THROUGHPUT_NONESTORE = "throughput-nonestore";
    /** 係数 */
    private final String ATTR_COEFFICIENT = "coef";
    /** 要求B/F算出 */
    private final String ATTR_REQUIRED = "required";
    /** 律速 */
    private final String ATTR_LIMITING = "limiting";
    /** 有効/無効 */
    private final String ATTR_ENABLED = "enabled";
    /** デフォルトサイズ：real */
    private final String ATTR_REAL = "real";
    /** デフォルトサイズ：integer */
    private final String ATTR_INTEGER = "integer";

    /** 演算性能 */
    private float operation_performance;
    /** スループットストア設定 */
    private THROUGHPUT_STORE_MODE storeMode;
    /** 算出単位 */
    private UNIT_TYPE unitType;
    /** デフォルトサイズ:real (byte) */
    private int defaultSizeReal;
    /** デフォルトサイズ:integer (byte) */
    private int defaultSizeInteger;
    /** 要求Byte/FLOP設定リスト */
    private List<Memoryband> listMemory= new ArrayList<Memoryband>();
    /** デフォルト要求Byte/FLOP設定 */
    private MemorybandProperties defaultProperties;

    /**
     * コンストラクタ
     * @throws Exception     プロパティ読込エラー
     */
    public MemorybandProperties() throws Exception {
    	// デフォルト値
    	this.storeMode = THROUGHPUT_STORE_MODE.AUTO;
    	this.unitType = UNIT_TYPE.BYTE_FLOP;
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
    	List<Memoryband> list = parseMemoryband(stream, "//" + ELEM_MEMORYBAND);
    	if (list != null && list.size() > 0) {
    		this.listMemory = list;
    	}
    }

    /**
     * 要求Byte/FLOP設定を取得する
     * @param stream		XML入力ストリーム
     * @param path		要求Byte/FLOP設定XPATH
     * @throws Exception 		要求Byte/FLOP設定パースエラー
     */
    public List<Memoryband> parseMemoryband(InputStream stream, String path) throws Exception {

        List<Memoryband> list = new ArrayList<Memoryband>();

        // XML
        DocumentBuilderFactory dbfactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder builder = dbfactory.newDocumentBuilder();
        org.w3c.dom.Document document = builder.parse(stream);

        XPathFactory factory = XPathFactory.newInstance();
        XPath xpath = factory.newXPath();

        // memoryband要素の取得
        XPathExpression expr = xpath.compile(path);

        Object result = expr.evaluate(document, XPathConstants.NODESET);

        NodeList nodelist = (NodeList) result;
        if (nodelist == null) return null;

        for (int i=0; i<nodelist.getLength(); i++) {
            try {
                Node node = nodelist.item(i);
                Memoryband band = null;

                // 属性の取得
                NamedNodeMap attrs = node.getAttributes();
                Node attrnode;
                String value;
                // キー
                attrnode = attrs.getNamedItem(ATTR_KEY);
                if (attrnode == null) continue;
                String key = attrnode.getNodeValue();
                // 演算性能
                if (KEY_OPERATION_PERFORMANCE.equalsIgnoreCase(key)) {
                    attrnode = attrs.getNamedItem(ATTR_VALUE);
                    value = attrnode.getNodeValue();
                    if (StringUtils.isFloat(value)) {
                        this.setOperationPerformance(Float.parseFloat(value));
                    }
                    continue;
                }
                // スループットストアモード
                else if (KEY_THROUGHPUT_STORE_MODE.equalsIgnoreCase(key)) {
                    attrnode = attrs.getNamedItem(ATTR_VALUE);
                    value = attrnode.getNodeValue();
                    try {
						this.setStoreMode(THROUGHPUT_STORE_MODE.valueOf(value.toUpperCase()));
					} catch (Exception ex) {
						ex.printStackTrace();
					}
                    continue;
                }
                // 算出単位
                else if (KEY_UNIT_TYPE.equalsIgnoreCase(key)) {
                    attrnode = attrs.getNamedItem(ATTR_VALUE);
                    value = attrnode.getNodeValue();
                    try {
						this.setUnitType(UNIT_TYPE.valueOf(value.toUpperCase()));
					} catch (Exception ex) {
						ex.printStackTrace();
					}
                    continue;
                }
                // デフォルトサイズ
                else if (KEY_DEFAULT_SIZE.equalsIgnoreCase(key)) {
                	// real
                    attrnode = attrs.getNamedItem(ATTR_REAL);
                    value = attrnode.getNodeValue();
                    if (StringUtils.isNumeric(value)) {
                        this.setDefaultSizeReal(Integer.parseInt(value));
                    }
                	// integer
                    attrnode = attrs.getNamedItem(ATTR_INTEGER);
                    value = attrnode.getNodeValue();
                    if (StringUtils.isNumeric(value)) {
                        this.setDefaultSizeInteger(Integer.parseInt(value));
                    }
                    continue;
                }

                // Memory
                else if (ACCESSMEMORY_TYPE.MEMORY.getKey().equalsIgnoreCase(key)) {
                    band = new Memoryband(ACCESSMEMORY_TYPE.MEMORY);
                }
                // L1 cache
                else if (ACCESSMEMORY_TYPE.L1_CACHE.getKey().equalsIgnoreCase(key)) {
                    band = new Memoryband(ACCESSMEMORY_TYPE.L1_CACHE);
                }
                // L2 cache
                else if (ACCESSMEMORY_TYPE.L2_CACHE.getKey().equalsIgnoreCase(key)) {
                    band = new Memoryband(ACCESSMEMORY_TYPE.L2_CACHE);
                }
                // Register
                else if (ACCESSMEMORY_TYPE.REGISTER.getKey().equalsIgnoreCase(key)) {
                    band = new Memoryband(ACCESSMEMORY_TYPE.REGISTER);
                }
                // custom
                else if (ACCESSMEMORY_TYPE.CUSTOM.getKey().equalsIgnoreCase(key)) {
                    band = new Memoryband(ACCESSMEMORY_TYPE.CUSTOM);
                }
                if (band == null) continue;

                // アクセス先名称
                attrnode = attrs.getNamedItem(ATTR_NAME);
                String name = null;
                if (attrnode != null) {
                	name = attrnode.getNodeValue();
                    band.setName(name);
                }

                // アクセス先背景色
                attrnode = attrs.getNamedItem(ATTR_BACKCOLOR);
                Color backcolor = null;
                if (attrnode != null) {
                    value = attrnode.getNodeValue();
                    backcolor = StringUtils.parseColor(value);
                    band.setBackColor(backcolor);
                }

                // スループット:ストアあり
                attrnode = attrs.getNamedItem(ATTR_THROUGHPUT_STORE);
                if (attrnode != null) {
                    value = attrnode.getNodeValue();
                    if (StringUtils.isFloat(value)) {
                    	band.setThroughputStore(Float.parseFloat(value));
                    }
                }

                // スループット:ストアなし
                attrnode = attrs.getNamedItem(ATTR_THROUGHPUT_NONESTORE);
                if (attrnode != null) {
                    value = attrnode.getNodeValue();
                    if (StringUtils.isFloat(value)) {
                    	band.setThroughputNonestore(Float.parseFloat(value));
                    }
                }

                // 係数
                attrnode = attrs.getNamedItem(ATTR_COEFFICIENT);
                if (attrnode != null) {
                    value = attrnode.getNodeValue();
                    if (StringUtils.isFloat(value)) {
                    	band.setCoef(Float.parseFloat(value));
                    }
                }

                // 要求B/F算出フラグ
                attrnode = attrs.getNamedItem(ATTR_REQUIRED);
                if (attrnode != null) {
                    value = attrnode.getNodeValue();
                    boolean required = Boolean.parseBoolean(value);
                    band.setRequired(required);
                }

                // 律速フラグ
                attrnode = attrs.getNamedItem(ATTR_LIMITING);
                if (attrnode != null) {
                    value = attrnode.getNodeValue();
                    boolean limiting = Boolean.parseBoolean(value);
                    band.setLimiting(limiting);
                }

                // 有効・無効
                attrnode = attrs.getNamedItem(ATTR_ENABLED);
                if (attrnode != null) {
                    value = attrnode.getNodeValue();
                    boolean enabled = Boolean.parseBoolean(value);
                    band.setEnabled(enabled);
                }

                list.add(band);

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
            org.w3c.dom.Comment comment = document.createComment(Message.getString("memorybandproperties.document.comment")); //要求Byte/FLOPプロパティ
            node.appendChild(comment);
        }

        if (this.listMemory == null || this.listMemory.size() <= 0) return;

        // 演算性能 GFLOPS
        {
            org.w3c.dom.Element elem = document.createElement(ELEM_MEMORYBAND);
            org.w3c.dom.Attr attrKey = document.createAttribute(ATTR_KEY);
            attrKey.setValue(KEY_OPERATION_PERFORMANCE);
            elem.setAttributeNode(attrKey);

        	float value = this.getOperationPerformance();
            org.w3c.dom.Attr attrValue = document.createAttribute(ATTR_VALUE);
            attrValue.setNodeValue(String.valueOf(value));
            elem.setAttributeNode(attrValue);

            // ノード追加
            node.appendChild(elem);
        }
        // スループットストアモード
        {
            org.w3c.dom.Element elem = document.createElement(ELEM_MEMORYBAND);
            org.w3c.dom.Attr attrKey = document.createAttribute(ATTR_KEY);
            attrKey.setValue(KEY_THROUGHPUT_STORE_MODE);
            elem.setAttributeNode(attrKey);

            THROUGHPUT_STORE_MODE value = this.getStoreMode();
            org.w3c.dom.Attr attrValue = document.createAttribute(ATTR_VALUE);
            attrValue.setNodeValue(value.toString().toLowerCase());
            elem.setAttributeNode(attrValue);

            // ノード追加
            node.appendChild(elem);
        }
        // 算出単位
        {
            org.w3c.dom.Element elem = document.createElement(ELEM_MEMORYBAND);
            org.w3c.dom.Attr attrKey = document.createAttribute(ATTR_KEY);
            attrKey.setValue(KEY_UNIT_TYPE);
            elem.setAttributeNode(attrKey);

            UNIT_TYPE value = this.getUnitType();
            org.w3c.dom.Attr attrValue = document.createAttribute(ATTR_VALUE);
            attrValue.setNodeValue(value.toString().toLowerCase());
            elem.setAttributeNode(attrValue);

            // ノード追加
            node.appendChild(elem);
        }
        // デフォルトサイズ
        {
            org.w3c.dom.Element elem = document.createElement(ELEM_MEMORYBAND);
            org.w3c.dom.Attr attrKey = document.createAttribute(ATTR_KEY);
            attrKey.setValue(KEY_DEFAULT_SIZE);
            elem.setAttributeNode(attrKey);
            // real
            int real = this.getDefaultSizeReal();
            org.w3c.dom.Attr attrReal = document.createAttribute(ATTR_REAL);
            attrReal.setNodeValue(String.valueOf(real));
            elem.setAttributeNode(attrReal);
            // integer
            int integer = this.getDefaultSizeInteger();
            org.w3c.dom.Attr attrInteger = document.createAttribute(ATTR_INTEGER);
            attrInteger.setNodeValue(String.valueOf(integer));
            elem.setAttributeNode(attrInteger);

            // ノード追加
            node.appendChild(elem);
        }
        // 要求Byte/FLOP設定
        for (Memoryband bind : this.listMemory) {
            org.w3c.dom.Element elem = document.createElement(ELEM_MEMORYBAND);

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
                org.w3c.dom.Attr attr = document.createAttribute(ATTR_BACKCOLOR);
                attr.setNodeValue(StringUtils.parseColorCode(color));
                elem.setAttributeNode(attr);
            }

            // スループット:ストアあり
            {
            	float value = bind.getThroughputStore();
                org.w3c.dom.Attr attr = document.createAttribute(ATTR_THROUGHPUT_STORE);
                attr.setNodeValue(String.valueOf(value));
                elem.setAttributeNode(attr);
            }
            // スループット:ストアなし
            {
            	float value = bind.getThroughputNonestore();
                org.w3c.dom.Attr attr = document.createAttribute(ATTR_THROUGHPUT_NONESTORE);
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
            // 要求B/F算出
            {
            	boolean value = bind.isRequired();
                org.w3c.dom.Attr attr = document.createAttribute(ATTR_REQUIRED);
                attr.setNodeValue(String.valueOf(value));
                elem.setAttributeNode(attr);
            }
            // 律速
            {
            	boolean value = bind.isLimiting();
                org.w3c.dom.Attr attr = document.createAttribute(ATTR_LIMITING);
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
    public List<Memoryband> getListMemoryband() {
        return this.listMemory;
    }

    /**
     * 要求Byte/FLOP設定数を取得する。
     * @return		要求Byte/FLOP設定数
     */
    public int getMemorybandCount() {
        if (listMemory == null || listMemory.size() <= 0) {return 0;}
        return listMemory.size();
    }

    /**
     * 要求Byte/FLOP設定を取得する。
     * @param	index		インデックス
     * @return		要求Byte/FLOP設定
     */
    public Memoryband getMemoryband(int index) {
        if (listMemory == null || listMemory.size() <= 0) {return null;}
        if (listMemory.size() <= index) {return null;}

        return listMemory.get(index);
    }

    /**
     * 要求Byte/FLOP設定を設定する。
     * @param	index		インデックス
     * @param	keyword		要求Byte/FLOP設定
     */
    public void setMemoryband(int index, Memoryband band) {
        if (listMemory == null || listMemory.size() <= 0) {return;}
        if (listMemory.size() <= index) {return;}
        listMemory.set(index, band);
    }

    /**
     * 要求Byte/FLOP設定を追加する。
     * @param	keyword		要求Byte/FLOP設定
     */
    public void addMemoryband(Memoryband band) {
        if (listMemory == null) {
        	listMemory = new ArrayList<Memoryband>();
        }

        listMemory.add(band);
    }

    /**
     * 要求Byte/FLOP設定を削除する。
     * @param	keyword		要求Byte/FLOP設定
     */
    public void removeMemoryband(Memoryband band) {
        if (listMemory == null) return;

        listMemory.remove(band);
    }

    /**
     * 要求Byte/FLOP設定を削除する。
     * @param	index		インデックス
     */
    public void removeMemoryband(int index) {
        if (listMemory == null) return;

        listMemory.remove(index);
    }

    /**
     * 要求Byte/FLOP設定リストをクリアする。
     */
    public void clearMemoryband() {
    	listMemory = new ArrayList<Memoryband>();
    }

    /**
     * 要求Byte/FLOP設定を取得する
     * @param type		要求Byte/FLOPタイプ
     * @return		Keyword情報
     */
    public Memoryband getMemoryband(ACCESSMEMORY_TYPE type) {
        if (type == null) return null;

        for (Memoryband band : listMemory) {
        	ACCESSMEMORY_TYPE srctype = band.getType();
            if (srctype == type) {
                return band;
            }
        }

        return null;
    }

    /**
     * 演算性能を取得する
     * @return		演算性能
     */
	public float getOperationPerformance() {
		return this.operation_performance;
	}

	/**
	 * 演算性能を設定する.
	 * @param performance	演算性能
	 */
	public void setOperationPerformance(float performance) {
		this.operation_performance = performance;
	}


    /**
     * デフォルト要求Byte/FLOP設定を取得する.
     * @return   デフォルト要求Byte/FLOP設定
     */
    public MemorybandProperties getDefaultProperties() {
    	return this.defaultProperties;
    }

    /**
     * デフォルト要求Byte/FLOP設定を設定する.
     * @return   デフォルト要求Byte/FLOP設定
     */
    public void setDefaultProperties(MemorybandProperties properties) {
    	this.defaultProperties = properties;
    }

	/**
	 * スループットストア設定を取得する.
	 * @return		スループットストア設定
	 */
	public THROUGHPUT_STORE_MODE getStoreMode() {
		return storeMode;
	}

	/**
	 * スループットストア設定を設定する
	 * @param storeMode		スループットストア設定
	 */
	public void setStoreMode(THROUGHPUT_STORE_MODE storeMode) {
		this.storeMode = storeMode;
	}

	/**
	 * 算出単位を取得する.
	 * @return		算出単位
	 */
	public UNIT_TYPE getUnitType() {
		return unitType;
	}

	/**
	 * 算出単位を設定する.
	 * @param unitType		算出単位
	 */
	public void setUnitType(UNIT_TYPE unitType) {
		this.unitType = unitType;
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
	 * スループット：ストア有りを取得する.
	 * @return    スループット：ストア有り
	 */
	public float getThroughputStore() {
		int count = getMemorybandCount();
		float value = 0.0F;
		for (int i=0; i<count; i++) {
			Memoryband mem = getMemoryband(i);
			float throughput = mem.getThroughputStore();
			float coef = mem.getCoef();
			value += throughput * coef;
		}
		return value;
	}


	/**
	 * スループット：ストア有りを取得する.
	 * @return    スループット：ストア有り
	 */
	public float getThroughputNoneStore() {
		int count = getMemorybandCount();
		float value = 0.0F;
		for (int i=0; i<count; i++) {
			Memoryband mem = getMemoryband(i);
			float throughput = mem.getThroughputNonestore();
			float coef = mem.getCoef();
			value += throughput * coef;
		}
		return value;
	}
}

