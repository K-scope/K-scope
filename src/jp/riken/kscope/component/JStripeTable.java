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
package jp.riken.kscope.component;

import java.awt.Color;
import java.awt.Component;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import javax.swing.BorderFactory;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.JTableHeader;

/**
 * Striped table component
 *
 * @author RIKEN
 */
public class JStripeTable extends JTable implements MouseListener {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Line height */
  private final int ROW_HEIGHT = 22;

  /** Background color for odd lines */
  private final Color EVEN_COLOR = new Color(240, 240, 255);

  /** Left margin of cell */
  private final int CELL_MARGIN = 5;
  /** Column resizing flag */
  @SuppressWarnings("unused")
  private boolean resizing = false;

  /** Constructor */
  public JStripeTable() {
    this(SwingConstants.LEFT);
  }

  /**
   * Constructor
   *
   * @param alignments Column display alignment
   * @param defultalign Default display alignment
   */
  public JStripeTable(int[] alignments, int defultalign) {
    super();

    // Extend table height
    this.setFillsViewportHeight(true);

    // Don't show cell grid
    this.setShowGrid(false);

    // Set to non-editable
    this.setDefaultEditor(Object.class, null);
    // For horizontal scroll bar display
    this.setAutoResizeMode(AUTO_RESIZE_OFF);

    // Change line height
    // line
    this.setRowHeight(ROW_HEIGHT);

    // Strip the background color with even and odd lines
    StripeTableRenderer renderer = new StripeTableRenderer(alignments, defultalign, CELL_MARGIN);
    this.setDefaultRenderer(Object.class, renderer);

    // header
    JTableHeader header = this.getTableHeader();
    // Disable column movement of the table.
    header.setReorderingAllowed(false);

    // Header rendering settings
    header.setDefaultRenderer(renderer);

    this.getTableHeader().addMouseListener(this);
  }

  /**
   * Constructor
   *
   * @param alignment Default display alignment of cells
   */
  public JStripeTable(int alignment) {
    this(null, alignment);
  }
  /*
      @Override
      public boolean getScrollableTracksViewportWidth() {
          // System.out.println("width=" + getPreferredSize().width + "<" + getParent().getWidth() + "=" + (getPreferredSize().width < getParent().getWidth()));
          if (resizing) {
              if (getPreferredSize().width - getParent().getWidth() > -50) {
                  return false;
              }
          }

          return getPreferredSize().width < getParent().getWidth();
      }
  */

  /**
   * Striped background color with even and odd lines Drawing class
   *
   * @author RIKEN
   */
  class StripeTableRenderer extends DefaultTableCellRenderer {

    /** Serial number */
    private static final long serialVersionUID = 1L;
    /** Placement */
    private int[] alignments;
    /** Placement */
    private int default_alignment;
    /** Margin */
    private int margin;

    /**
     * Constructor
     *
     * @param defaultalign Default alignment
     * @param margin Margin
     */
    public StripeTableRenderer(int defaultalign, int margin) {
      this.default_alignment = defaultalign;
      this.margin = margin;
    }

    /**
     * Constructor
     *
     * @param alignments Column alignment
     * @param defaultalign Default alignment
     * @param margin Margin
     */
    public StripeTableRenderer(int[] alignments, int defaultalign, int margin) {
      this.alignments = alignments;
      this.default_alignment = defaultalign;
      this.margin = margin;
    }

    /**
     * Strip the background color with even and odd rows of cells
     *
     * @param table table
     * @param value Cell value
     * @param isSelected Selected state
     * @param hasFocus Focus state
     * @param row row index
     * @param column Column index
     * @return drawing component
     */
    @Override
    public Component getTableCellRendererComponent(
        JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
      super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);

      // Draw the background color in stripes on even and odd lines.
      if (row == -1) {
        // header
        setForeground(table.getTableHeader().getForeground());
        setBackground(table.getTableHeader().getBackground());
      } else if (isSelected) {
        setForeground(table.getSelectionForeground());
        setBackground(table.getSelectionBackground());
      } else {
        setForeground(table.getForeground());
        setBackground((row % 2 == 0) ? EVEN_COLOR : table.getBackground());
      }

      if (margin > 0) {
        setFont(table.getFont());
        // Set the border and secure space for left justification
        Border border = null;
        if (row == -1) {
          // header
          border = UIManager.getBorder("TableHeader.cellBorder");
        } else {
          border = table.getBorder();
          if (border instanceof LineBorder) {
            border = new LineBorder(Color.WHITE, 0, false);
          }
          // Do not draw borders other than headers
          border = null;
        }
        if (border != null) {
          Border setBorder =
              BorderFactory.createCompoundBorder(border, new EmptyBorder(0, margin, 0, 0));
          setBorder(setBorder);
        }
        // Text display including line breaks is limited to the first line.
        String text = value != null ? (String) value.toString() : "";
        String br = System.getProperty("line.separator");
        if (text.indexOf(br) < 0) {
          setValue(value);
        } else {
          String[] lines = text.split(br);
          setValue(lines[0]);
        }
      }

      // Character placement
      if (this.alignments == null) {
        // Numeric cells are right-justified
        setHorizontalAlignment(getCellHorizontalAlignment(value));
      } else {
        if (this.alignments.length > column) {
          setHorizontalAlignment(this.alignments[column]);
        } else {
          setHorizontalAlignment(this.default_alignment);
        }
      }
      return this;
    }

    /**
     * Left or right justify according to the cell value.
     *
     * @param value Cell value
     * @return SwingConstants.LEFT or SwingConstants.RIGHT
     */
    private int getCellHorizontalAlignment(Object value) {
      if (value instanceof Integer) return SwingConstants.RIGHT;
      if (value instanceof Long) return SwingConstants.RIGHT;
      if (value instanceof Float) return SwingConstants.RIGHT;
      if (value instanceof Double) return SwingConstants.RIGHT;
      if (value instanceof Number) return SwingConstants.RIGHT;

      return this.default_alignment;
    }
  }

  /**
   * Mouse click event
   *
   * @param event Mouse event information
   */
  @Override
  public void mouseClicked(MouseEvent event) {}

  /**
   * Mouse button down event
   *
   * @param e Mouse event information
   */
  @Override
  public void mousePressed(MouseEvent e) {
    if (e.getButton() == MouseEvent.BUTTON1) {
      resizing = true;
    }
  }

  /**
   * Mouse button up event
   *
   * @param e Mouse event information
   */
  @Override
  public void mouseReleased(MouseEvent e) {
    resizing = false;
  }

  /**
   * Mouseover event
   *
   * @param e Mouse event information
   */
  @Override
  public void mouseEntered(MouseEvent e) {}

  /**
   * Mouse out event
   *
   * @param e Mouse event information
   */
  @Override
  public void mouseExited(MouseEvent e) {
    resizing = false;
  }
}
