# Scriba: A Book Writing Assistant for Org Mode

Scriba is an integrated suite of tools for Emacs designed to streamline the process of writing long-form projects like novels, technical books, and dissertations using the power of Org Mode. It provides a flexible project structure, powerful creation and navigation commands, and a focus on keeping your content and research notes organized yet separate.

At its core, Scriba helps you manage the complexity of a large writing project, so you can focus on what matters: writing.

---

## Screenshot
### NOTHING HERE YET.

## Features

*   **Flexible Project Structure**: Create new book projects with a single command. The entire directory structure (folder names for content, notes, custom research areas) is defined in a central `book-config.org` file.
*   **Dynamic Folder Management**: Run `M-x scriba-sync-folder-structure` at any time to automatically create or clean up directories based on your configuration file.
*   **Configurable Content Templates**: Define your own templates for different types of content (e.g., `Chapter`, `Scene`, `Part`) directly within your project's configuration.
*   **Organized Note-Keeping**: Separate your manuscript from your research. Create categorized notes for characters, locations, plot points, and more.
*   **Centralized Command Hub**: A powerful `hydra` menu gives you instant access to all of Scriba's commands with single-key shortcuts.
*   **Quick Navigation**: Instantly jump to any file in your project—manuscript or note—with a unified, searchable list using `scriba-jump-to-file`.
*   **Automated Back-references**: The `scriba-update-note-appearances` command scans your manuscript and automatically adds links in your note files, showing you everywhere a character or location is mentioned.
*   **Writing Statistics**: Get a quick overview of your project's progress, including word counts and section counts.
*   **Painless Compilation & Export**:
    *   Compile a `master-outline.org` file with `#+INCLUDE` directives, perfect for exporting your entire manuscript as a single document.
    *   Use a central `scriba-style.org` file to apply consistent formatting (fonts, margins, custom styles) across your entire project for PDF or HTML export.
*   **Project Visualization**: Generate a clickable SVG relationship graph of your project files with Graphviz to visualize how your notes and content are interconnected.

## Installation

### MELPA (Not yet on MELP)

Scriba is not yet on MELPA, but once it is, you can install it via:

```elisp
;; With use-package
(use-package scriba
  :ensure t
  :config
  ;; Optionally, enable auto-syncing of folders when you save the config file.
  ;; (add-hook 'after-save-hook #'scriba-auto-sync-on-config-save)
  )
```

### Manual Installation

#### Via Init File
1.  Download `scriba.el` and place it in a directory on your Emacs `load-path`.
2.  Add the following to your `init.el`:

```elisp
(require 'scriba)

;; Optional: Enable auto-syncing of folders when you save the config file.
;; (add-hook 'after-save-hook #'scriba-auto-sync-on-config-save)
```

#### Via Use-Package
```elisp
	(use-package scriba
		:load-path "~/Documents/bookIdeas/"
		:custom
		(scriba-author "YourAuthorName") ; The default author name to use when exporting a story. Each story can also override this setting
		(scriba-author-email "Contact@me") ; The default author contact email to use when exporting a story. Each story can also override this setting
		)

```

## Getting Started: Your First Project

Here is the standard workflow for starting a new book with Scriba.

1.  **Create the Project**:
    Run `M-x scriba-new-book`. You will be prompted for the book's name and a parent directory. Scriba will generate a complete project structure.

2.  **Customize Your Configuration**:
    Open the newly created `book-config.org` file. This is the brain of your project. Here you can change the names of the core folders, define your content types (`Chapter`, `Scene`), and list your note categories (`Characters`, `Locations`).

3.  **Sync Your Structure**:
    After editing `book-config.org`, save it. Then run `M-x scriba-sync-folder-structure`. This command will read your config file and create all the necessary directories and subdirectories for your content and notes.

4.  **Create Your First Chapter**:
    Open the Hydra menu with **`C-c C-b h`** (in any project file) and press **`c`** for "New Content". Select "Chapter" and give it a title. Scriba will create the file from your chapter template and place it in the correct subdirectory.

5.  **Create a Character Note**:
    From the Hydra, press **`n`** for "New Note". Select the "Characters" category and enter the character's name.

6.  **Write!**
    You are now set up. Use the Hydra (`C-c C-b h`) to navigate, create new files, and manage your project.

## The Scriba Hydra

The primary interface for Scriba is its Hydra menu, available on **`C-c C-b h`** within any Scriba project file.

```
╭─────────────────────Scriba Project Menu ───────────────────────╮
│ Create            │ View / Navigate     │ Actions              │
├───────────────────┼─────────────────────┼──────────────────────┤
│ _c_: New Content----│ _j_: Jump to File-----│ _u_: Update Appearances│
│ _n_: New Note-------│ _m_: Main File--------│ _S_: Project Stats-----│
│ _C_: New Character--│ _i_: Sections Idx-----│ _W_: Word Count File---│
│                   │ _N_: Note Cat Idx-----│ _d_: Distraction-Free--│
├───────────────────┼─────────────────────┼──────────────────────┤
│ _V_: Visualize Graph│ _o_: Open Other...----│ _B_: Backup Project----│
│                   │ _O_: Compile Outline--│ _G_: Git Commit--------│
│                   │ _e_: Export Project---│                      │
│                   │ _s_: Sync Folder Struc│                      │
│                   │                     │                      │
╰───────────────────┴─────────────────────┴──────────────────────╯
Press q to quit.
```

## Configuration

All project-specific configuration is handled in `book-config.org`.

### Folder Structure

You can change the names of the main project directories by editing these properties:
*   `:SCRIBA_CONTENT_DIR:` (e.g., "Manuscript")
*   `:SCRIBA_NOTES_DIR:` (e.g., "Bible")
*   `:SCRIBA_INDICES_DIR:` (e.g., "Indexes")
*   `:SCRIBA_EXTRA_DIRS:` (e.g., "Images, Research, OldDrafts")

After changing these, run `M-x scriba-sync-folder-structure` to apply your changes.

### Custom Content Templates

You can define templates for any content type listed in `:SCRIBA_CONTENT_TYPES:`.

1.  **Convention**: The property name must be `:SCRIBA_TEMPLATE_TYPENAME:`. For a "Part" content type, the property would be `:SCRIBA_TEMPLATE_PART:`.
2.  **Format**: The value is the template string. **Use `\n` for newlines.**
3.  **Placeholders**:
    *   Most templates use one `%s` placeholder, which will be replaced by the name you provide.
    *   The "Chapter" template is special and uses two: `%s` for the chapter number and `%s` for the title.

**Example `book-config.org`:**
```org
* Settings
:PROPERTIES:
:SCRIBA_CONTENT_DIR: Manuscript
:SCRIBA_NOTES_DIR: Bible
:SCRIBA_INDICES_DIR: Indexes
:SCRIBA_EXTRA_DIRS: Research, Images

:NOTE_CATEGORIES: Characters, Locations, Factions, Lore
:SCRIBA_CONTENT_TYPES: Part, Chapter, Scene

:SCRIBA_TEMPLATE_PART: #+TITLE: Part %s: %s\n\n* Overview\n
:SCRIBA_TEMPLATE_CHAPTER: #+TITLE: Chapter %s: %s\n#+PARENT: Part X\n\n* Summary\n* Manuscript\n
:SCRIBA_TEMPLATE_SCENE: #+TITLE: %s\n#+SETUPFILE: ../../scriba-style.org\n\n- POV:: \n\n* Action\n
:END:
```

### Export Styling (`scriba-style.org`)

When you create a new project, Scriba also creates `scriba-style.org` in the root directory. This file is included via `#+SETUPFILE:` in some templates and in the master outline.

Use this file to define project-wide export settings, such as:
*   `#+OPTIONS:` for consistent export behavior.
*   `#+LATEX_HEADER:` for custom PDF fonts, margins, and packages.
*   `#+HTML_HEAD:` for custom CSS.

This ensures every piece of your book exports with the same professional and consistent formatting.

## Contributing

This is an open-source project and contributions are very welcome! 
This project is currently a WIP as well, So Please Please leave suggestions or submit improvements.

## License

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 3 of the License, or (at your option) any later version. See the `LICENSE` file for details.
