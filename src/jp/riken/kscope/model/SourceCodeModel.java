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

package jp.riken.kscope.model;

import java.awt.Color;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Observable;
import javax.swing.text.SimpleAttributeSet;
import jp.riken.kscope.Message;
import jp.riken.kscope.common.KEYWORD_TYPE;
import jp.riken.kscope.data.BatchDocument;
import jp.riken.kscope.data.CodeLine;
import jp.riken.kscope.data.Keyword;
import jp.riken.kscope.data.SourceFile;
import jp.riken.kscope.data.VariableMemory;
import jp.riken.kscope.gui.ISourceBargraph;
import jp.riken.kscope.utils.TextFileReader;

/**
 * Source code model class
 *
 * @author RIKEN
 */
public class SourceCodeModel extends Observable {

  /** Display source file */
  private SourceFile sourceFile;

  /** Search keywords */
  private List<Keyword> searchWords;

  /** Emphasis range: Effective range, etc. */
  private List<CodeLine> highlightArea = new ArrayList<CodeLine>();

  /** Selection block: Structure tree selection block */
  private List<CodeLine> selectedBlock = new ArrayList<CodeLine>();

  /** Source code documentation */
  private BatchDocument document;

  /** Emphasis range: Background color such as effective range */
  private Color colorHighlightArea;
  /** Background color of selected block */
  private Color colorSelectedBlock;
  /** Character color of search string */
  private Color colorSearchFont;
  /** Background color of search string */
  private Color colorSearchBackground;

  /** Profiler: Cost data */
  private List<ISourceBargraph> listBarData;
  /** Profiler data: Maximum value */
  private float maxValue;
  /** Profiler data: Minimum value */
  private float minValue;
  /** Variable access destination memory */
  private List<VariableMemory> variableMemories;

  /**
   * Constructor
   *
   * @param source Display source file
   */
  public SourceCodeModel(SourceFile source) {
    this.sourceFile = source;
  }

  /**
   * Get the display source file path (absolute path)
   *
   * @return Display source file path (absolute path)
   */
  public String getFilePath() {
    if (sourceFile == null) return null;
    return this.sourceFile.getPath();
  }

  /**
   * Get the display source file path
   *
   * @return Display source file
   */
  public SourceFile getSourceFile() {
    return this.sourceFile;
  }

  /**
   * Set the display source file path
   *
   * @param file Display source file
   */
  public void setSourceFile(SourceFile file) {
    this.sourceFile = file;
  }

  /**
   * Get search keywords
   *
   * @return searchWord Search keyword
   */
  public List<Keyword> getSearchWords() {
    return searchWords;
  }

  /**
   * Set search keywords
   *
   * @param list Search keywords
   */
  public void setSearchWords(List<Keyword> list) {
    this.searchWords = list;

    // Set the highlight color for the search keyword.
    setSearchWordColor();
  }

  /**
   * Add a search keyword
   *
   * @param word Search keyword
   */
  public void addSearchWords(Keyword word) {
    if (this.searchWords == null) {
      this.searchWords = new ArrayList<Keyword>();
    }
    this.searchWords.add(word);

    // Set the highlight color for the search keyword.
    setSearchWordColor();
  }

  /** Clear the search keyword */
  public void clearSearchWords() {
    if (this.searchWords == null) return;
    this.searchWords.clear();
  }

  /**
   * Get the emphasis range.
   *
   * @return highlightArea Highlight range
   */
  public List<CodeLine> getHighlightArea() {
    return highlightArea;
  }

  /**
   * Set the emphasis range
   *
   * @param highlightArea Highlight range
   */
  public void setHighlightArea(List<CodeLine> highlightArea) {
    this.highlightArea = highlightArea;
  }

  /**
   * Add emphasis
   *
   * @param area Highlight range
   */
  public void addHighlightArea(CodeLine area) {
    this.highlightArea.add(area);
  }

  /** Clear the emphasis range */
  public void clearHighlightArea() {
    this.highlightArea.clear();
  }

  /**
   * Read the file and display it.
   *
   * @throws Exception Read error
   */
  public void readFile() throws Exception {
    readFile(this.sourceFile);
  }

  /**
   * Read the file and display it.
   *
   * @param filename File name
   * @throws Exception Read error
   */
  public void readFile(String filename) throws Exception {
    File file = new File(filename);
    if (!file.exists()) {
      throw new Exception(
          filename + Message.getString("sourcecodemodel.exception.notexist")); // does not exist.
    }

    readFile(new SourceFile(file));
  }

  /**
   * Read the file and display it.
   *
   * @param source File object
   * @throws Exception Read error
   */
  public void readFile(SourceFile source) throws Exception {
    File file = source.getFile();
    if (!file.exists()) {
      throw new Exception(
          file.getName()
              + Message.getString("sourcecodemodel.exception.notexist")); // does not exist.
    }

    SimpleAttributeSet attr = new SimpleAttributeSet();
    //        StyleConstants.setForeground(attr, Color.RED);

    document = new BatchDocument();
    TextFileReader reader = new TextFileReader(file);
    String line;
    while ((line = reader.readLine()) != null) {
      document.appendBatchLineString(line, attr);
    }

    document.processBatchUpdates(0);
  }

  /**
   * Get source code pine documentation
   *
   * @return Source code Pine documentation
   */
  public BatchDocument getDocument() {
    return document;
  }

  /**
   * Get the selected block.
   *
   * @return selectedBlock Selected block
   */
  public List<CodeLine> getSelectedBlock() {
    return selectedBlock;
  }

  /**
   * Set selection block
   *
   * @param highlightArea Selection block
   */
  public void setSelectedBlock(List<CodeLine> highlightArea) {
    this.selectedBlock = highlightArea;
  }

  /**
   * Add a selection block
   *
   * @param line selection block
   */
  public void addSelectedBlock(CodeLine line) {
    this.selectedBlock.add(line);
  }

  /** Clear the selected block */
  public void clearSelectedBlock() {
    this.selectedBlock.clear();
  }

  /**
   * Emphasis range: Acquires the background color such as the effective range
   *
   * @return Emphasis range: Background color such as effective range
   */
  public Color getColorHighlightArea() {
    return colorHighlightArea;
  }

  /**
   * Emphasis range: Get the background color such as the effective range
   *
   * @param color Emphasis range: Background color such as effective range
   */
  public void setColorHighlightArea(Color color) {
    this.colorHighlightArea = color;
  }

  /**
   * Get the background color of the selected block
   *
   * @return Background color of selected block
   */
  public Color getColorSelectedBlock() {
    return colorSelectedBlock;
  }

  /**
   * Set the background color of the selected block
   *
   * @param color Background color of the selected block
   */
  public void setColorSelectedBlock(Color color) {
    this.colorSelectedBlock = color;
  }

  /**
   * Get the character color of the search string
   *
   * @return Character color of search string
   */
  public Color getColorSearchFont() {
    return colorSearchFont;
  }

  /**
   * Set the character color of the search string
   *
   * @param colorSearchFont Character color of search string
   */
  public void setColorSearchFont(Color colorSearchFont) {
    this.colorSearchFont = colorSearchFont;

    // Set the highlight color for the search keyword.
    setSearchWordColor();
  }

  /**
   * Get the background color of the search string
   *
   * @return Character color of search string
   */
  public Color getColorSearchBackground() {
    return colorSearchBackground;
  }

  /**
   * Set the background color of the search string
   *
   * @param colorSearchBackground Character color of search string
   */
  public void setColorSearchBackground(Color colorSearchBackground) {
    this.colorSearchBackground = colorSearchBackground;

    // Set the highlight color for the search keyword.
    setSearchWordColor();
  }

  /** Set the highlight color for the search keyword. */
  private void setSearchWordColor() {
    if (this.searchWords == null) return;

    for (Keyword word : this.searchWords) {
      if (this.colorSearchFont != null) {
        word.setForecolor(this.colorSearchFont);
      }
      if (this.colorSearchBackground != null) {
        word.setBackgroundcolor(this.colorSearchBackground);
      }
    }
  }

  /**
   * Clear search / trace keywords
   *
   * @param type Clear keyword type
   */
  public void clearSearchWords(KEYWORD_TYPE type) {
    if (this.searchWords == null) return;
    if (this.searchWords.size() <= 0) return;
    for (int i = this.searchWords.size() - 1; i >= 0; i--) {
      if (this.searchWords.get(i).getType() == type) {
        this.searchWords.remove(i);
      }
    }
  }

  /** Notify model changes */
  public void notifyModel() {
    this.setChanged();
    this.notifyObservers();
    this.clearChanged();
  }

  /**
   * Get source bar graph data
   *
   * @return Source bar graph data
   */
  public List<ISourceBargraph> getListBarData() {
    return listBarData;
  }

  /**
   * Set source bar graph data
   *
   * @param listBarData Source bar graph data
   * @param max Maximum value of total cost data
   * @param min Minimum value of total cost data
   */
  public void setListBarData(List<ISourceBargraph> listBarData, float max, float min) {
    this.listBarData = listBarData;
    this.maxValue = max;
    this.minValue = min;
    notifyModel();
  }

  /**
   * Add source bar graph data
   *
   * @param list Source bar graph data
   */
  public void addListBarData(List<ISourceBargraph> list) {
    if (list == null) return;
    if (this.listBarData == null) {
      this.listBarData = new ArrayList<ISourceBargraph>();
    }
    this.listBarData.addAll(list);
    notifyModel();
  }

  /**
   * Add source bar graph data
   *
   * @param list Source bar graph data
   */
  public void addListBarData(ISourceBargraph[] list) {
    if (list == null) return;
    addListBarData(Arrays.asList(list));
  }

  /**
   * Set the variable access destination memory.
   *
   * @param list Variable access destination memory
   */
  public void setVariableMemories(List<VariableMemory> list) {
    this.variableMemories = list;
  }

  /**
   * Get variable access destination memory
   *
   * @return Variable access destination memory
   */
  public List<VariableMemory> getVariableMemories() {
    return variableMemories;
  }

  /**
   * Profiler data: Get maximum value
   *
   * @return Profiler data: Maximum
   */
  public float getMaxValue() {
    return maxValue;
  }

  /**
   * Profiler data: Get the minimum value
   *
   * @return Profiler data: Minimum
   */
  public float getMinValue() {
    return minValue;
  }
}
