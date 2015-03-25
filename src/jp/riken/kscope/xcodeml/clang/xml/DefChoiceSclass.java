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
package jp.riken.kscope.xcodeml.clang.xml;

import javax.xml.bind.annotation.XmlEnumValue;

/**
 * IDの記憶クラス:sclass (storage class)属性の選択肢
 * 以下の何れかの値を取る.
 * 		'auto',
 * 		'param',
 * 		'extern',
 * 		'extern_def',
 * 		'static',
 * 		'register',
 * 		'label',
 * 		'tagname',
 * 		'moe',
 * 		'typedef_name'
 * 		'gccLabel'
 *
 * @author RIKEN
 */
public enum DefChoiceSclass {

    AUTO("auto"),
    PARAM("param"),
    EXTERN("extern"),
    EXTERN_DEF("extern_def"),
    STATIC("static"),
    REGISTER("register"),
    LABEL("label"),
    TAGNAME("tagname"),
    MOE("moe"),
    TYPEDEF_NAME("typedef_name"),
    GCCLABEL("gccLabel");

    private final String value;

    /**
     * コンストラクタ
     * @param v		記憶クラス:sclass属性値
     */
    DefChoiceSclass(String v) {
        value = v;
    }

    /**
     * 記憶クラス:sclass属性値を取得する
     * @return		記憶クラス:sclass属性値
     */
    public String value() {
        return value;
    }

    /**
     * 記憶クラス:sclass属性値から記憶クラス:sclass列挙体を取得する
     * @param v		記憶クラス:sclass属性値
     * @return		記憶クラス:sclass列挙体
     */
    public static DefChoiceSclass fromValue(String v) {
        for (DefChoiceSclass c: DefChoiceSclass.values()) {
            if (c.value.equals(v)) {
                return c;
            }
        }
        throw new IllegalArgumentException(v);
    }

    /**
     * 記憶クラス:sclass属性値と同じであるかチェックする.
     * @param sclass		記憶クラス:sclass属性値
     * @return		true=一致
     */
    public boolean equals(String sclass) {
        return this.value.equalsIgnoreCase(sclass);
    }
}
