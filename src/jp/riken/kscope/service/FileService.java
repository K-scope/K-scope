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
package jp.riken.kscope.service;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.nio.charset.Charset;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import jp.riken.kscope.Message;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.model.ErrorInfoModel;
import jp.riken.kscope.model.PropertiesTableModel;
import jp.riken.kscope.utils.TextFileReader;

/**
 * Service class that manages files
 *
 * @author RIKEN
 */
public class FileService extends BaseService {

  /** Character code */
  private Charset charset;

  /** Constructor */
  public FileService() {}

  /**
   * Constructor
   *
   * @param errorModel Error model
   */
  public FileService(ErrorInfoModel errorModel) {
    super(errorModel);
  }

  /**
   * Set file properties
   *
   * @param file Property acquisition file
   * @param model Property setting model
   */
  public void setFileProperties(File file, PropertiesTableModel model) {
    String[] items = {
      Message.getString("fileservice.fileproperties.name"), // name
      Message.getString("fileservice.fileproperties.location"), // location
      Message.getString("fileservice.fileproperties.size"), // size
      Message.getString("fileservice.fileproperties.kind"), // type
      Message.getString("fileservice.fileproperties.update")
    }; // Update date and time
    String[] values = new String[5];

    // name
    values[0] = file.getName();
    // Location
    try {
      values[1] = file.getCanonicalPath();
    } catch (IOException e) {
    }

    if (file.isDirectory()) {
      values[2] = "";
      values[3] = Message.getString("fileservice.fileproperties.folder"); // folder
    } else if (file.isFile()) {
      // size
      float kbyte = (float) (file.length() / 1000.0);
      values[2] = String.format("%#.1f KB", kbyte);
      // type
      values[3] = Message.getString("fileservice.fileproperties.file"); // File
    }
    // Update date and time
    Calendar calendar = Calendar.getInstance();
    calendar.setTimeInMillis(file.lastModified());
    values[4] =
        new SimpleDateFormat(Message.getString("fileservice.fileproperties.dateformat"))
            .format(calendar.getTime()); // YYYY year MM month DD day HH: MM: SS

    // set in the model
    // Notification to the property panel is notified by Observer.
    model.setTitle(file.getName());
    model.setProperties(items, values);
  }

  /**
   * Output line of code to file
   *
   * @param srcfile source file
   * @param lines Code line list
   * @throws Exception Read exception
   */
  public void writeFile(File srcfile, CodeLine[] lines) throws Exception {
    StringBuffer buf = new StringBuffer();
    for (CodeLine line : lines) {
      buf.append(line.getStatement());
      buf.append("\n");
    }
    if (this.charset == null) {
      // Output to UTF-8 file
      this.charset = Charset.forName("UTF-8");
    }
    writeFile(srcfile, buf.toString(), this.charset);
  }

  /**
   * Output line of code to file
   *
   * @param srcfile source file
   * @param lines Code line list
   * @throws Exception Read exception
   */
  public void writeFile(SourceFile srcfile, CodeLine[] lines) throws Exception {
    StringBuffer buf = new StringBuffer();
    for (CodeLine line : lines) {
      buf.append(line.getStatement());
      buf.append("\n");
    }
    if (this.charset == null) {
      // Output to UTF-8 file
      this.charset = Charset.forName("UTF-8");
    }
    writeFile(srcfile.getFile(), buf.toString(), this.charset);
  }

  /**
   * Output line of code to file
   *
   * @param srcfile source file
   * @param text Output string
   * @throws Exception Write exception
   */
  public void writeFile(SourceFile srcfile, String text) throws Exception {
    if (this.charset == null) {
      // Output to UTF-8 file
      this.charset = Charset.forName("UTF-8");
    }
    writeFile(srcfile.getFile(), text, this.charset);
  }

  /**
   * Output code line to file with output character code
   *
   * @param outfile source file
   * @param text Output string
   * @param charset Output character code (UTF-8 if null)
   * @throws Exception Write exception
   */
  private void writeFile(File outfile, String text, Charset charset) throws Exception {

    try {
      // Create folder
      File folder = outfile.getParentFile();
      if (!folder.exists()) {
        folder.mkdirs();
      }
      // Generate output stream
      FileOutputStream fs = new FileOutputStream(outfile);
      if (charset == null) {
        charset = Charset.defaultCharset();
      }
      PrintWriter pw = new PrintWriter(new OutputStreamWriter(fs, charset));

      // Write to file
      pw.print(text);

      // Clean up
      pw.close();
      fs.close();

    } catch (Exception ex) {
      this.addErrorInfo(ex);
      throw ex;
    }
  }

  /**
   * Get a line of code from a file
   *
   * @param file Source file
   * @param parentPath parent folder
   * @return Code line list
   * @throws Exception Read error
   */
  public CodeLine[] readSourceFile(SourceFile file, File parentPath) throws Exception {
    if (file == null) return null;
    File readfile = file.getFile();
    if (readfile == null) return null;
    if (!readfile.isAbsolute() && parentPath != null) {
      readfile = new File(parentPath, readfile.getPath());
    }
    ArrayList<CodeLine> list = new ArrayList<CodeLine>();
    try {
      TextFileReader reader = new TextFileReader(readfile);
      String line;
      int line_no = 0;
      while ((line = reader.readLine()) != null) {
        line_no++;
        // Generate and add code lines
        list.add(new CodeLine(file, line, line_no, file.getPath()));
      }
      // Set the character code
      this.charset = reader.getCharset();

    } catch (Exception ex) {
      this.addErrorInfo(ex);
      throw ex;
    }

    if (list.size() <= 0) return null;

    // Read code line list
    return list.toArray(new CodeLine[0]);
  }

  /**
   * Get the character code
   *
   * @return Character code
   */
  public Charset getCharset() {
    return charset;
  }

  /**
   * Set the character code
   *
   * @param charset Character code
   */
  public void setCharset(Charset charset) {
    this.charset = charset;
  }
}
