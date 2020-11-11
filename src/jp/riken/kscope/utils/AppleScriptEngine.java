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
package jp.riken.kscope.utils;

import java.io.File;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;

/**
 * AppleScript execution class
 *
 * @author RIKEN
 */
public class AppleScriptEngine {

  /**
   * Execute AppleScript in the folder selection dialog display.
   *
   * @param title Title
   * @param currentDirectoryPath Default path
   * @return Selected folder
   */
  public static File showFolderDialog(String title, String currentDirectoryPath) {
    try {
      final String script = createChooseFolderScript(title, currentDirectoryPath);
      ScriptEngine scriptEngine = new ScriptEngineManager().getEngineByName("AppleScript");
      final String result = (String) scriptEngine.eval(script);
      if (result == null || result.length() <= 0) {
        return null;
      }
      return new File(result);
    } catch (Exception ex) {
      ex.printStackTrace();
      return null;
    }
  }

  /**
   * Create an AppleScript to display the folder selection dialog.
   *
   * @param title Dialog description
   * @param path Default path
   * @return AppleScript
   */
  private static String createChooseFolderScript(String title, String path) {
    StringBuffer buf = new StringBuffer();
    // try
    buf.append("try\n");
    // set theFolder to choose folder with prompt "title" default location "path"
    buf.append("set theFolder to choose folder with prompt \"");
    buf.append(title);
    buf.append("\"");
    buf.append(" default location ");
    buf.append("\"");
    buf.append(path);
    buf.append("\"");
    buf.append("\n");
    buf.append("set thePath to theFolder as string\n");
    buf.append("set pPath to POSIX path of theFolder as string\n");
    buf.append("return pPath\n");
    buf.append("on error\n");
    buf.append("return \"\"\n");
    buf.append("end try\n");
    return buf.toString();
  }
}
