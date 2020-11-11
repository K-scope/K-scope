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

package jp.riken.kscope.menu;

import java.awt.Component;
import java.awt.event.ActionListener;
import java.util.Arrays;
import java.util.EventListener;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;
import javax.swing.JTree;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import javax.swing.tree.TreePath;

import jp.riken.kscope.Message;
import jp.riken.kscope.action.ActionBase;
import jp.riken.kscope.action.FileExportExploreAction;
import jp.riken.kscope.action.FileOpenSourceFileAction;
import jp.riken.kscope.action.FilePropertiesAction;
import jp.riken.kscope.action.ProjectAddFileAction;
import jp.riken.kscope.action.ProjectAddFolderAction;
import jp.riken.kscope.action.ProjectDeleteFileAction;
import jp.riken.kscope.action.SearchGrepAction;
import jp.riken.kscope.action.SearchTreeAction;
import jp.riken.kscope.action.TreeCollapseAllAction;
import jp.riken.kscope.action.TreeExpandAllAction;
import jp.riken.kscope.action.TreeExpandSelectAction;
import jp.riken.kscope.action.ViewOpenExploreBlockAction;
import jp.riken.kscope.common.EXPLORE_PANEL;
import jp.riken.kscope.common.FRAME_VIEW;
import jp.riken.kscope.service.AppController;

/**
 * Source Explorer Panel pop-up menu
 * @author RIKEN
 *
 */
public class SourceTreePopupMenu extends JPopupMenu implements PopupMenuListener, ITreePopupMenu {

    /** Serial number */
    private static final long serialVersionUID = 1L;

    /** Application controller */
    private AppController controller;
    /** All storage listener */
    private ActionListener actionTreeCollapseAll;
    /** All deployment listener */
    private ActionListener actionTreeExpandAll;
    /** Selective expansion listener */
    private ActionListener actionTreeExpandSelect;
    /** Open file Listener */
    private EventListener actionOpenFile;
    /** Explorer export action */
    private ActionListener actionExportExplore;

    /** Open file menu */
    private JMenuItem menuOpenFile;

    /**
     * Constructor
     */
    public SourceTreePopupMenu() {
        // Create a menu.
        initialize();
    }


    /**
     * Constructor
     * @param controller Application controller
     */
    public SourceTreePopupMenu(AppController controller) {
        this.controller = controller;

        // Create a menu.
        initialize();
    }

    /**
     * Create a menu.
     */
    private void initialize() {

        // Create menu
        // store everything
        JMenuItem menuCollapseAll = new JMenuItem(Message.getString("treechooserdialog.tooltip.collapseall")); // All stored
        this.add(menuCollapseAll);
        this.actionTreeCollapseAll = new TreeCollapseAllAction(this.controller);
        menuCollapseAll.addActionListener(this.actionTreeCollapseAll);

        // Expand all
        JMenuItem menuExpandAll = new JMenuItem(Message.getString("treechooserdialog.tooltip.expandall")); // Expand all
        this.add(menuExpandAll);
        this.actionTreeExpandAll = new TreeExpandAllAction(this.controller);
        menuExpandAll.addActionListener(this.actionTreeExpandAll);

        // Selective expansion
        JMenuItem menuExpandSelect = new JMenuItem(Message.getString("mainmenu.view.collapse-expand.selective")); // Selective expansion
        this.add(menuExpandSelect);
        this.actionTreeExpandSelect = new TreeExpandSelectAction(this.controller);
        menuExpandSelect.addActionListener(this.actionTreeExpandSelect);

        // spacer
        this.add(new JSeparator());

        // File search
        JMenuItem menuSearchGrep = new JMenuItem(Message.getString("mainmenu.search.file")); // File search
        this.add(menuSearchGrep);
        menuSearchGrep.addActionListener(new SearchGrepAction(this.controller));

        // Tree search
        JMenuItem menuSearchTree = new JMenuItem(Message.getString("mainmenu.search.tree")); // Tree search
        this.add(menuSearchTree);
        menuSearchTree.addActionListener(new SearchTreeAction(this.controller));

        // spacer
        this.add(new JSeparator());
        
        // Add source folder ...
        JMenuItem menuAddFolder = new JMenuItem(Message.getString("sourcetreepopupmenu.menu.addsrcfolder")); // Add source folder
        this.add(menuAddFolder);
        menuAddFolder.addActionListener(new ProjectAddFolderAction(this.controller, EXPLORE_PANEL.SOURCE));

        // Add source file ...
        JMenuItem menuAddFile = new JMenuItem(Message.getString("sourcetreepopupmenu.menu.addsrcfile")); // Add source file
        this.add(menuAddFile);
        menuAddFile.addActionListener(new ProjectAddFileAction(this.controller, EXPLORE_PANEL.SOURCE));

        // Delete source file
        JMenuItem menuDeleteFile = new JMenuItem(Message.getString("sourcetreepopupmenu.menu.deletesrcfile")); // Delete source file
        this.add(menuDeleteFile);
        menuDeleteFile.addActionListener(new ProjectDeleteFileAction(this.controller, EXPLORE_PANEL.SOURCE));

        // spacer
        this.add(new JSeparator());

        // open the file
        menuOpenFile = new JMenuItem(Message.getString("sourcetreepopupmenu.menu.openfile")); // Show selected files in source view
        this.add(menuOpenFile);
        actionOpenFile = new ViewOpenExploreBlockAction(this.controller);
        menuOpenFile.addActionListener((ActionListener) actionOpenFile);

        // File: Open the source file with an external tool
        JMenuItem menuFileOpenSourceFile = new JMenuItem(Message.getString("mainmenu.file.program")); // Open with an external tool
        this.add(menuFileOpenSourceFile);
        menuFileOpenSourceFile.addActionListener(new FileOpenSourceFileAction(this.controller, FRAME_VIEW.EXPLORE_VIEW));

        // Properties
        JMenuItem menuFileProperties = new JMenuItem(Message.getString("mainmenu.project.property")); // Properties
        this.add(menuFileProperties);
        menuFileProperties.addActionListener(new FilePropertiesAction(this.controller));

        // Export (Hide pop-up: Create action only)
        actionExportExplore = new FileExportExploreAction(this.controller);

        this.addPopupMenuListener(this);
    }

    /**
     * Show the pop-up menu only on JTree nodes.
     * @param invoker component
     * @param x coordinate X
     * @param y Coordinate Y
     */
    @Override
    public void show(Component invoker, int x, int y) {
        JTree tree = (JTree)invoker;
        TreePath[] selectepath = tree.getSelectionPaths();
        if(selectepath!=null) {
            TreePath path = tree.getPathForLocation(x, y);
            if(path!=null && Arrays.asList(selectepath).contains(path)) {
                super.show(invoker, x, y);
            }
        }
    }


    /**
     * Get all storage action listeners
     * @return All storage action listener
     */
    @Override
    public ActionListener getActionTreeCollapseAll() {
        return actionTreeCollapseAll;
    }

    /**
     * Get all deployment action listeners
     * @return All deployment action listener
     */
    @Override
    public ActionListener getActionTreeExpandAll() {
        return actionTreeExpandAll;
    }

    /**
     * Get Selective Deployment Action Listener
     * @return Selective expansion action listener
     */
    @Override
    public ActionListener getActionTreeExpandSelect() {
        return actionTreeExpandSelect;
    }

    /**
     * Get the listener to open the file.
     * @return Open file Listener
     */
    @Override
    public EventListener getActionOpenFile() {
        return actionOpenFile;
    }


    /**
     * Pop-up menu visible event. <br/>
     * Check if the action is executable
     * @param event Event information
     */
    @Override
    public void popupMenuWillBecomeVisible(PopupMenuEvent event) {

        // Check if the action is executable
        JPopupMenu menu = (JPopupMenu) event.getSource();
        int count = menu.getComponentCount();
        for (int i=0; i<count; i++) {
            Object obj = menu.getComponent(i);
            if (!(obj instanceof JMenuItem)) continue;
            JMenuItem submenu = (JMenuItem)obj;
            ActionListener[] actions = submenu.getActionListeners();
            if (actions == null) continue;
            for (ActionListener action : actions) {
                if (action instanceof ActionBase) {
                    boolean enabled = ((ActionBase)action).validateAction();
                    submenu.setEnabled(enabled);
                }
            }
        }
    }


    /**
     * Events with the pop-up menu canceled
     * @param event Event information
     */
    @Override
    public void popupMenuWillBecomeInvisible(PopupMenuEvent event) { }


    /**
     * Events with the pop-up menu canceled
     * @param event Event information
     */
    @Override
    public void popupMenuCanceled(PopupMenuEvent event) { }


    /**
     * Get the explorer export action
     * @return explorer export action
     */
    @Override
    public ActionListener getActionExportExplore() {
        return actionExportExplore;
    }

}
