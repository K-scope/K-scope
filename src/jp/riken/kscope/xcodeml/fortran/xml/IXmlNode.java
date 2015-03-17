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
package jp.riken.kscope.xcodeml.fortran.xml;


/**
 * XML要素クラス
 * @author RIKEN
 */
public interface IXmlNode {

    /**
     * 要素探索を開始する
     * @param visitor			XcodeMLノード探索
     * @return		成否
     */
    boolean enter(IXmlVisitor visitor);

    /**
     * 要素探索を終了する
     * @param visitor			XcodeMLノード探索
     */
    void leave(IXmlVisitor visitor);

}
