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
package jp.riken.kscope.profiler.dprof;

import java.util.ArrayList;
import java.util.List;

/**
 * ファイル情報レコード
 * @author RIKEN
 */
public class FileRecord {
	
	/** ファイル情報リスト */
    private List<FileInfo> fileInfoList;
	
    /**
     * コンストラクタ
     */
    public FileRecord() {
    	this.fileInfoList = new ArrayList<FileInfo>();
    }
    
    /**
     * ファイル情報を追加する
     * @param info		ファイル情報
     * @return		true=success
     */
    public boolean addFileInfo(FileInfo info) {
    	return this.fileInfoList.add(info);
    }
    
    /**
     * ファイル情報数を取得する
     * @return		ファイル情報数
     */
    public int getFileInfoCount() {
    	if (fileInfoList == null) return 0;
    	return this.fileInfoList.size();
    }
    
    
    /**
     * ファイル情報を取得する
     * @param index			ファイルインデックス
     * @return		ファイル情報
     */
    public FileInfo getFileInfo(int index) {
    	if (fileInfoList == null) return null;
    	return this.fileInfoList.get(index);
    }
}


