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

package jp.riken.kscope.language.fortran;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

/**
 * Fortran:union型クラス。
 * @deprecated 未使用
 * @author RIKEN
 *
 */
public class Union implements Serializable {
    /** シリアル番号 */
    private static final long serialVersionUID = -3660335230300380344L;
    private Set<jp.riken.kscope.language.fortran.Map> maps = new HashSet<jp.riken.kscope.language.fortran.Map>();

    /**
     * コンストラクタ
     */
    public Union() {

    }

    /**
     * コピーコンストラクタ
     */
    public Union(Union union) {
        if (union == null) return;
        union.maps = union.maps;
    }


    /**
     * map文の追加。
     *
     * @param map
     *          map文
     */
    public void add(jp.riken.kscope.language.fortran.Map map){
        if (map != null) {
            maps.add(map);
        }
    }

    /**
     * 共用体内のmapセットの取得。
     *
     * @return mapセット
     */
    public Set<jp.riken.kscope.language.fortran.Map> getMaps() {
        return maps;
    }

}
