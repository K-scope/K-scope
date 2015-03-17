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

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import jp.riken.kscope.Message;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.exception.LanguageException;
import jp.riken.kscope.exception.XcodeMLException;
import jp.riken.kscope.parser.IAnalyseParser;
import jp.riken.kscope.data.FILE_TYPE;

/**
 * XMLファイルヘッダーを読み込み、C言語、Fortran用のXcodeMLパーサを生成する.
 * @author RIKEN
 */
public class FactoryXcodeMLParser {

	/**
	 * XMLファイルヘッダーを読み込み、C言語、Fortran用のXcodeMLパーサを生成する.
	 * @param file     読込XMLファイル
	 * @return		C言語、Fortran用のXcodeMLパーサ
	 */
    public static IAnalyseParser factoryParser(SourceFile file) {

		// XMLファイルの言語取得
		FILE_TYPE type = FactoryXcodeMLParser.parserLanguage(file);
		if (type == FILE_TYPE.FORTRANLANG) {
			// Fortran用XcodeMLパーサ
			return new jp.riken.kscope.xcodeml.fortran.XcodeMLParserStax();
		}
		else if (type == FILE_TYPE.CLANG) {
			// C言語用XcodeMLパーサ
			return new jp.riken.kscope.xcodeml.clang.XcodeMLParserStax();
		}
		
    	return null;
    }
    

	/**
	 * XMLファイルのルート要素<XcodeProgram>タブを読み込み、C言語、Fortranであるかチェックする.
	 * @param file     読込XMLファイル
	 * @return		C言語、Fortranファイルタイプ : FILE_TYPE.FORTRANLANG,FILE_TYPE.CLANG,FILE_TYPE.UNKNOWN;
	 */
    public static FILE_TYPE parserLanguage(SourceFile file) {
        // StAX 用ファクトリ 
        XMLInputFactory factory = XMLInputFactory.newInstance();
        XMLStreamReader reader = null;
        InputStream stream = null;

        try {
        	if (file == null) {
        		// ソースファイルが設定されていません。
                throw new XcodeMLException(Message.getString("xcodemlparserstax.error.sourcefile"));
        	}
        	
            String xmlfile = file.getPath();
            stream = new FileInputStream(xmlfile);

        	// StAXパーサの生成
            reader = factory.createXMLStreamReader(stream);

            // イベントループ
            while (reader.hasNext()) {
                // 次のイベントを取得
                int eventType = reader.next();
                if (eventType == XMLStreamReader.START_ELEMENT) {
                    String elem_name = reader.getLocalName();
                    System.out.println(elem_name);
                    if (elem_name == "XcodeProgram") {
	                    String source = reader.getAttributeValue(null, "source");
	                    String language = reader.getAttributeValue(null, "language");
	                    /*
	                    String time = reader.getAttributeValue(null, "time");
	                    String compiler_info = reader.getAttributeValue(null, "compiler-info");
	                    String version = reader.getAttributeValue(null, "version");
	                    System.out.println("source=" + source);
	                    System.out.println("language=" + language);
	                    System.out.println("time=" + time);
	                    System.out.println("compiler-info=" + compiler_info);
	                    System.out.println("version=" + version);
	                    */
	                    if ("Fortran".equalsIgnoreCase(language)) {
	                    	// Fortran
	                    	return FILE_TYPE.FORTRANLANG;
	                    }
	                    else if ("C".equalsIgnoreCase(language)) {
	                    	// C言語
	                    	return FILE_TYPE.CLANG;
	                    }
                    }
                }
            }
        } catch (IOException ex) {
            throw new LanguageException(ex, file);
        } catch (XcodeMLException ex) {
            throw new LanguageException(ex, file);
        } catch (XMLStreamException ex) {
            throw new LanguageException(ex, file);
        } finally {
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

		return FILE_TYPE.UNKNOWN;
    	
    }
}
