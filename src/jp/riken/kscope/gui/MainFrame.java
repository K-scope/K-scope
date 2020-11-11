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
package jp.riken.kscope.gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import javax.swing.BorderFactory;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.border.BevelBorder;
import javax.swing.border.LineBorder;
import jp.riken.kscope.Application;
import jp.riken.kscope.Message;
import jp.riken.kscope.action.AnalysisTabChangeAction;
import jp.riken.kscope.action.ExploreTreeChangeAction;
import jp.riken.kscope.action.FileExitAction;
import jp.riken.kscope.action.HelpVersionAction;
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.dialog.ProfilerLegendDialog;
import jp.riken.kscope.dialog.ProgressDialog;
import jp.riken.kscope.dialog.SearchFindDialog;
import jp.riken.kscope.dialog.SearchGrepDialog;
import jp.riken.kscope.dialog.SearchTreeDialog;
import jp.riken.kscope.menu.LanguageTreePopupMenu;
import jp.riken.kscope.menu.MainMenu;
import jp.riken.kscope.menu.ModuleTreePopupMenu;
import jp.riken.kscope.menu.ProfilerPopupMenu;
import jp.riken.kscope.menu.SourcePanelPopupMenu;
import jp.riken.kscope.menu.SourceTreePopupMenu;
import jp.riken.kscope.menu.VariablePopupMenu;
import jp.riken.kscope.menu.XmlTreePopupMenu;
import jp.riken.kscope.service.AppController;

/**
 * main frame
 *
 * @author RIKEN
 */
public class MainFrame extends javax.swing.JFrame implements ITabComponent {

  /** Serial number */
  private static final long serialVersionUID = 1L;

  /** Application controller */
  AppController controller;

  /** Status view */
  private StatusBarPanel panelStatusBar;
  /** Source view */
  private SourceView panelSourceView;
  /** Analytical Information View */
  private AnalysisView panelAnalysisView;
  /** Explorer view */
  private ExploreView panelExplorerView;
  /** Progress bar dialog */
  private ProgressDialog dialogProgress;

  /** Source search dialog */
  private SearchFindDialog dialogSearchFind;
  /** File search dialog */
  private SearchGrepDialog dialogSearchGrep;
  /** Tree search dialog */
  private SearchTreeDialog dialogSearchTree;
  /** Profiler Preview Dialog */
  private ProfilerLegendDialog dialogProfilerLegend;

  /** Tab focus listener Last access, active panel monitoring listener */
  private TabFocusListener focusListener;
  /** Main menu */
  private MainMenu menuMain;

  /** Constructor */
  public MainFrame() {
    super();
  }

  /**
   * Initialize
   *
   * @param controller Application controller
   */
  public void initialize(AppController controller) {
    this.controller = controller;
    initGUI();
  }

  /** Initialize the mainframe. */
  private void initGUI() {
    this.setTitle(Message.getString("mainframe.title")); // K-scope

    final int SPLIT_DIVIDERSIZE = 5; // Divider size
    focusListener = new TabFocusListener();

    BorderLayout frameLayout = new BorderLayout();
    Dimension screen_size = getToolkit().getScreenSize();
    this.setBounds(0, 0, screen_size.width, screen_size.height);
    this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    this.getContentPane().setLayout(frameLayout);
    {
      panelStatusBar = new StatusBarPanel();
      this.getContentPane().add(panelStatusBar, BorderLayout.SOUTH);
      panelStatusBar.setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED));
    }
    {
      JSplitPane splitHorizontal = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
      this.getContentPane().add(splitHorizontal, BorderLayout.CENTER);
      splitHorizontal.setResizeWeight(0.1);
      splitHorizontal.setDividerSize(SPLIT_DIVIDERSIZE);
      {
        JSplitPane splitVertical = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
        splitHorizontal.add(splitVertical, JSplitPane.RIGHT);
        splitVertical.setResizeWeight(0.8);
        splitVertical.setDividerSize(SPLIT_DIVIDERSIZE);

        // Add analysis view
        {
          // Measures to increase the height of JTabbed Pane on MacOSX for some reason
          // Do not add analytic views directly to JSplitPane
          // Add an analysis view to JPanel and add JPanel to JSplitPane.
          panelAnalysisView = new AnalysisView();
          // splitVertical.add(panelAnalysisInfoView, JSplitPane.RIGHT);
          JPanel panel = new JPanel();
          splitVertical.add(panel, JSplitPane.RIGHT);
          panel.setLayout(new BorderLayout());
          panel.setBorder(new LineBorder(Color.GRAY, 1));
          panel.add(panelAnalysisView);

          // Parent form settings
          panelAnalysisView.setParentComponent(this);
          // Focus listener settings
          panelAnalysisView.addTabFocusListener(focusListener);
        }
        // Add source view
        {
          panelSourceView = new SourceView();
          splitVertical.add(panelSourceView, JSplitPane.LEFT);

          // Parent form settings
          panelSourceView.setParentComponent(this);
          // Focus listener settings
          panelSourceView.addTabFocusListener(focusListener);
        }
      }
      // Add explorer view
      {
        panelExplorerView = new ExploreView();
        splitHorizontal.add(panelExplorerView, JSplitPane.LEFT);
      }
    }

    // Main menu
    menuMain = new MainMenu(controller);
    this.setJMenuBar(menuMain);

    // Source File Panel Context Menu
    SourcePanelPopupMenu menuSourcePanel = new SourcePanelPopupMenu(controller);
    this.panelSourceView.setSourcePanelPopupMenu(menuSourcePanel);

    // Structure Tree Explorer Context Menu
    LanguageTreePopupMenu menuLanguageTree = new LanguageTreePopupMenu(controller);
    panelExplorerView.setLanguagePopupMenu(menuLanguageTree);

    // Module Tree Explorer Context Menu
    ModuleTreePopupMenu menuModuleTree = new ModuleTreePopupMenu(controller);
    panelExplorerView.setModulePopupMenu(menuModuleTree);

    // Source File Explorer Context Menu
    SourceTreePopupMenu menuSourceTree = new SourceTreePopupMenu(controller);
    panelExplorerView.setSourcePopupMenu(menuSourceTree);

    // Tree change event
    ExploreTreeChangeAction treeChangeAction = new ExploreTreeChangeAction(controller);
    panelExplorerView.addTreeSelectionListener(treeChangeAction);

    // XML File Explorer context menu
    XmlTreePopupMenu menuXml = new XmlTreePopupMenu(controller);
    panelExplorerView.setXmlPopupMenu(menuXml);

    // Assign a menu action listener to a panel button.
    panelAnalysisView.setActionListener(menuMain);

    // Variable characteristic list context menu
    panelAnalysisView.getPanelVariable().setPopupMenu(new VariablePopupMenu(controller));

    // Profiler cost information context menu
    ProfilerPopupMenu profilerPopup = new ProfilerPopupMenu(controller);
    panelAnalysisView.setProfilerPopupMenu(profilerPopup);

    // Tab change event
    AnalysisTabChangeAction tabChangeAction = new AnalysisTabChangeAction(controller);
    panelAnalysisView.addTabChangeListener(tabChangeAction);

    // Screen size at startup
    this.setSize(new Dimension(screen_size.width, screen_size.height));

    // End event
    this.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
    this.addWindowListener(new FileExitAction(this.controller));

    // Progress bar dialog
    dialogProgress = new ProgressDialog(this, false);
    // Source search dialog
    dialogSearchFind = new SearchFindDialog(this, true);
    // File search dialog
    dialogSearchGrep = new SearchGrepDialog(this, true);
    // Tree search dialog
    dialogSearchTree = new SearchTreeDialog(this, true);

    // Register the status bar as a status notification
    Application.addStatus(panelStatusBar);
    // Register the progress bar dialog as a status notification
    Application.addStatus(dialogProgress);
  }

  /**
   * Get source view
   *
   * @return Source view
   */
  public SourceView getPanelSourceView() {
    return panelSourceView;
  }

  /**
   * Get the analysis information view.
   *
   * @return Analysis information view
   */
  public AnalysisView getPanelAnalysisView() {
    return panelAnalysisView;
  }

  /**
   * Get explorer view
   *
   * @return explorer view
   */
  public ExploreView getPanelExplorerView() {
    return panelExplorerView;
  }

  /**
   * Get the parent component.
   *
   * @return Parent component
   */
  @Override
  public ITabComponent getParentComponent() {
    return null;
  }

  /**
   * Set the parent component.
   *
   * @param component Parent component
   */
  @Override
  public void setParentComponent(ITabComponent component) {}

  /**
   * Set focus listener
   *
   * @param listener Focus listener
   */
  @Override
  public void addTabFocusListener(TabFocusListener listener) {}

  /** Close tab */
  @Override
  public void closeTabComponent() {
    ITabComponent forcus = this.focusListener.getLastTabComponent();
    if (forcus != null) {
      forcus.closeTabComponent();
    }
    return;
  }

  /**
   * Get the active view identifier
   *
   * @return Active view identifier
   */
  public FRAME_VIEW getEnumView() {
    ITabComponent forcus = this.focusListener.getLastTabComponent();
    if (forcus == null) return null;
    ITabComponent parent = forcus.getParentComponent();
    ClosableTabbedPane panel = null;
    if (parent != null && parent instanceof ClosableTabbedPane) {
      panel = (ClosableTabbedPane) parent;
    } else if (forcus instanceof ClosableTabbedPane) {
      panel = (ClosableTabbedPane) forcus;
    }

    if (panel == null) return null;

    return panel.getViewType();
  }

  /**
   * Get the controller.
   *
   * @return application controller
   */
  public AppController getController() {
    return this.controller;
  }

  /**
   * Get the progress bar dialog
   *
   * @return progress bar dialog
   */
  public ProgressDialog getDialogProgress() {
    return dialogProgress;
  }

  /** Display the version information dialog */
  public void showAboutDialog() {
    HelpVersionAction action = new HelpVersionAction(this.controller);
    action.showAboutDialog();
  }

  /** Exit the application */
  public void exitApplication() {
    FileExitAction action = new FileExitAction(this.controller);
    action.exitApplication(this);
  }

  /**
   * Get the source search dialog
   *
   * @return Source search dialog
   */
  public SearchFindDialog getDialogSearchFind() {
    return this.dialogSearchFind;
  }

  /**
   * Get the file search dialog
   *
   * @return File search dialog
   */
  public SearchGrepDialog getDialogSearchGrep() {
    return this.dialogSearchGrep;
  }

  /**
   * Get the tree search dialog
   *
   * @return Tree search dialog
   */
  public SearchTreeDialog getDialogSearchTree() {
    return this.dialogSearchTree;
  }

  /**
   * Get profiler preview dialog
   *
   * @return Profiler preview dialog
   */
  public ProfilerLegendDialog getDialogProfilerLegend() {
    return dialogProfilerLegend;
  }

  /**
   * Set profiler preview dialog
   *
   * @param dialog Profiler preview dialog
   */
  public void setDialogProfilerLegend(ProfilerLegendDialog dialog) {
    this.dialogProfilerLegend = dialog;
  }

  /**
   * Get the main menu.
   *
   * @return Main menu
   */
  public MainMenu getMenuMain() {
    return menuMain;
  }
}
