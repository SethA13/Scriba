;;; scriba.el --- An Org mode system for book writing -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Your Name
;; Author: Your Name <your.email@example.com>
;; Maintainer: Your Name <your.email@example.com>
;; URL: https://your-repo-url.com
;; Keywords: writing, outlines, notes, org, authoring, book
;; Prefix: scriba
;; Package-Requires: ((emacs "28.1") (org "9.5") (hydra "0.15.0"))

;; Version: 0.3.0 (Architectural fix: converted to a minor mode for proper integration with Org)

;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see https://www.gnu.org/licenses/.

;;; Commentary:
;;
;; Scriba helps organize notes and content for writing a book using Org mode.
;; It sets up a flexible project structure, allowing customization of folder names
;; and addition of custom project directories.
;;
;; Workflow:
;; 1. Create a new book project (`scriba-new-book`).
;; 2. Open any `.org` file in the project. `scriba-mode` will activate automatically.
;; 3. Customize project structure (folder names, extra dirs) and define note
;;    categories in `book-config.org`.
;; 4. Create content sections (`scriba-new-section`).
;; 5. Create notes within categories (`scriba-new-note`).
;; 6. Use writing support tools like word count and project stats.
;; 7. Access commands via the `Scriba` menu or the hydra (`scriba-hydra` or `C-c C-b h`).
;; 8. Optionally, update references (`scriba-update-note-appearances`).
;; 9. Collate and export your book (`scriba-collate-book`, `scriba-export-project`).
;; 10. Use Git for version control (optional `scriba-git-commit-project`).
;;
;; Collaboration:
;; Scriba encourages the use of Git for version control. For collaborative
;; projects, use a shared Git repository (e.g., on GitHub, GitLab, or a
;; self-hosted server).
;;
;; For distraction-free writing, consider packages like `writeroom-mode`.

;;; Code:

(require 'org)
(require 'org-element)
(require 'subr-x)
(require 'cl-lib)
(require 'hydra)
(require 'filenotify) ; For potential future auto-updates, not used yet.
(require 'vc)         ; For Git integration

;;;; Global Variables and Constants
(defvar scriba-mode-map (make-keymap) "Keymap for scriba-mode minor mode.")
(defvar scriba-menu)

(defconst scriba--folder-separator (file-name-as-directory "/") "System's folder separator.")
(defconst scriba--file-ending ".org" "File ending for Scriba files.")

(defconst scriba--config-filename "book-config.org" "Filename for book configuration.")
(defconst scriba--main-filename "main.org" "Filename for the main book entry point.")
(defconst scriba--gitignore-filename ".gitignore" "Filename for Git ignore rules.")

;; Default folder names (can be overridden in book-config.org)
(defconst scriba--default-content-folder "Content" "Default folder name for content sections.")
(defconst scriba--default-notes-folder "Notes" "Default folder name for all notes.")
(defconst scriba--default-indices-folder "Indices" "Default folder name for index files.")

(defconst scriba--sections-index-filename "sections-index.org" "Index file for content sections.")
(defconst scriba--note-category-index-prefix "notes-" "Prefix for note category index files.")

(defconst scriba--note-categories-property "NOTE_CATEGORIES" "Property in config for note categories.")
(defconst scriba--appearances-heading "Appearances in Content" "Heading for note appearances.")

;; Properties in book-config.org for customizing directory names
(defconst scriba--prop-content-dir "SCRIBA_CONTENT_DIR")
(defconst scriba--prop-content-types "SCRIBA_CONTENT_TYPES")
(defconst scriba--prop-notes-dir "SCRIBA_NOTES_DIR")
(defconst scriba--prop-indices-dir "SCRIBA_INDICES_DIR")
(defconst scriba--prop-extra-dirs "SCRIBA_EXTRA_DIRS")


(defconst scriba--prop-template-prefix "SCRIBA_TEMPLATE_" "Prefix for template properties in book-config.org.")
(defconst scriba--prop-template-default "SCRIBA_TEMPLATE_DEFAULT" "Property for the default/fallback template.")
;;;; Customization Variables
(defcustom scriba-author (user-full-name)
	"Default author name for new book projects."
	:group 'scriba
	:type 'string)

(defcustom scriba-author-email (user-mail-address)
	"Default author email for new book projects."
	:group 'scriba
	:type 'string)

(defcustom scriba-git-init-on-new-book t
	"If non-nil, initialize a Git repository when creating a new book."
	:group 'scriba
	:type 'boolean)

(defcustom scriba-style-file-contents
	(string-join
	 '("#+OPTIONS: toc:nil num:nil ^:{} author:nil"
		 ""
		 "COMMENT: This file is included by content files (like Scenes) to provide"
		 "COMMENT: consistent export options. You can add common LaTeX headers,"
		 "COMMENT: HTML styles, or other Org export settings here."
		 ) "\n")
	"Default contents for the project's `scriba-style.org` file."
	:group 'scriba
	:type 'string)

(defcustom scriba-main-file-template
	(string-join
	 '("#+TITLE: %s" ; book title
		 "#+AUTHOR: %s" ; author
		 "#+TODO: TODO WORKING FEEDBACK REVIEW | DONE" ; custom TODO states
		 ""
		 "* Overview"
		 "  Brief description of the book."
		 ""
		 "* TODO Writing Goals [0%%]"
		 "** TODO Outline Act I"
		 "** TODO Draft Chapter 1"
		 ""
		 "* Structure"
		 "  Access content, notes, and configuration via the Scriba hydra (`C-c C-b h` or `M-x scriba-hydra`)."
		 "  ** [[file:%s/%s][Content Sections]]" ; indices_dir/sections_index_filename
		 ""
		 "* Notes Indices"
		 "  ;; Links to note category indices will be added here by scriba-new-note"
		 ""
		 "* Other Project Areas"
		 "  ;; Links to extra directories will be added here if SCRIBA_EXTRA_DIRS is set"
		 ""
		 "* [[file:%s][Book Configuration]]" ; config_filename
		 ) "\n")
	"Template string for the main project file.
Placeholders:
	%s (book title)
	%s (author)
	%s (indices directory name)
	%s (sections index filename)
	%s (config filename)"
	:group 'scriba
	:type 'string)

(defcustom scriba-config-file-template
	(string-join
	 '("#+TITLE: Configuration for %s" ; book title
		 "#+AUTHOR: %s" ; author
		 ""
		 "* Settings"
		 ":PROPERTIES:"
		 ":SCRIBA_CONTENT_DIR: Content"
		 ":SCRIBA_NOTES_DIR: Notes"
		 ":SCRIBA_INDICES_DIR: Indices"
		 ":SCRIBA_EXTRA_DIRS: Worldbuilding, Images"
		 ""
		 ":NOTE_CATEGORIES: Characters, Locations, Plot Points, Research"
		 ":SCRIBA_CONTENT_TYPES: Chapter, Scene, Part"
		 ""
		 "COMMENT: --- Custom Content Templates ---"
		 "COMMENT: Define templates for content types below. Use \\n for newlines."
		 "COMMENT: The property name must be :SCRIBA_TEMPLATE_TYPENAME: (e.g., :SCRIBA_TEMPLATE_CHAPTER:)."
		 ":SCRIBA_TEMPLATE_CHAPTER: #+TITLE: Chapter %s: %s\\n\\n* Summary\\n\\n* Manuscript\\n"
		 ":SCRIBA_TEMPLATE_SCENE: #+TITLE: %s\\n#+SETUPFILE: ../../scriba-style.org\\n\\n- POV:: \\n- Setting:: \\n- Time:: \\n\\n* Action\\n"
		 ":SCRIBA_TEMPLATE_DEFAULT: #+TITLE: %s\\n\\n* Overview\\n"
		 ":END:"
		 ""
		 "** About This File"
		 "   This file stores project-specific settings for Scriba."
		 "   - To define a template for a content type like 'Chapter', create a property"
		 "     named `:SCRIBA_TEMPLATE_CHAPTER:` (all uppercase)."
		 "   - The value of the property is the template string. Use `\\n` for newlines."
		 "   - Placeholders like `%s` can be used. The `scriba-new-content` function"
		 "     will pass arguments like chapter number and title."
		 ""
		 "   - `NOTE_CATEGORIES`: Comma-separated list of note categories."
		 "   - `SCRIBA_CONTENT_DIR`: Default content folder name."
		 "   - `SCRIBA_CONTENT_TYPES`: Comma-separated list of content types (e.g., Chapter, Scene)."
		 "   - `SCRIBA_NOTES_DIR`, `SCRIBA_INDICES_DIR`: Customize the names of core project folders."
		 "     If changed after project creation, ensure you move existing folders."
		 "   - `SCRIBA_EXTRA_DIRS`: Comma-separated list of additional top-level directories you want"
		 "     for your project. Scriba will create these and link to them from `main.org`."
		 ) "\n")
	"Template string for the book configuration file.
Placeholders:
%s (book title)
%s (author)"
	:group 'scriba
	:type 'string)

(defcustom scriba-section-template-string
	"#+TITLE: %s\n\n* %s\n\n"
	"Template string for new section files. %s, %s -> title, title."
	:group 'scriba
	:type 'string)

(defcustom scriba-note-template-string
	"#+TITLE: %s (%s)\n#+CATEGORY: %s\n\n* %s\n\n* %s\n"
	"Template string for new note files. %s... -> note_name, category, appearances_heading."
	:group 'scriba
	:type 'string)

(defcustom scriba-default-gitignore-contents
	(string-join
	 '("*.elc"
		 "*~"
		 "*.bak"
		 "collated-book.org" ; Default collated output
		 "*.pdf"
		 "*.html"
		 "*.odt"
		 ".DS_Store"
		 ".project" ; Common IDE files
		 ".projectile"
		 ".ccls-cache/"
		 "auto-save-list/"
		 ".dir-locals.el") "\n")
	"Default contents for the .gitignore file created in new projects."
	:group 'scriba
	:type 'string)

(defcustom scriba-character-template
	(string-join
	 '("#+TITLE: %s (Character)"
		 "#+CATEGORY: Characters"
		 ""
		 "* Physical Description"
		 ""
		 "* Personality & Traits"
		 ""
		 "* Backstory"
		 ""
		 "* Motivations & Goals"
		 ""
		 "* Relationships"
		 ""
		 "* Notes"
		 ""
		 "* Appearances in Content") "\n")
	"Template string for new character profile files. %s -> Character Name."
	:group 'scriba
	:type 'string)

(defcustom scriba-master-outline-filename "master-outline.org"
	"Default filename for the compiled master outline."
	:group 'scriba
	:type 'string)

(defcustom scriba-chapter-template-string
	"#+TITLE: Chapter %s: %s\n#+AUTHOR: %s\n\n* %s\n\n"
	"Template for new chapter files. %s -> Chapter Number, %s -> Chapter Title, %s -> author, %s -> Chapter Title again."
	:group 'scriba
	:type 'string)

(defcustom scriba-scene-template-string
	"#+TITLE: %s\n#+DATE: \n#+SETUPFILE: ../../scriba-style.org\n\n- POV:: \n- Setting:: \n- Time:: \n\n* \n\n"
	"Template for new scene files. %s -> Scene Title, %s -> Scene Title again."
	:group 'scriba
	:type 'string)

;;;; Helper Functions
(defun scriba--get-template-for-type (type)
	"Retrieve the template string for a given content TYPE from book-config.org.
The function looks for a property named :SCRIBA_TEMPLATE_TYPE: (all uppercase).
If not found, it falls back to :SCRIBA_TEMPLATE_DEFAULT:, then to a hardcoded default."
	(let* ((root (scriba--book-root-folder))
				 (config-file (concat root scriba--config-filename))
				 ;; Construct the property name, e.g., "SCRIBA_TEMPLATE_CHAPTER"
				 (type-prop-name (concat scriba--prop-template-prefix (upcase type)))
				 (specific-template (scriba--get-property-value type-prop-name config-file))
				 (default-template (scriba--get-property-value scriba--prop-template-default config-file)))
		;; The `replace-regexp-in-string` is crucial to convert "\\n" into actual newlines.
		(when specific-template
			(setq specific-template (replace-regexp-in-string "\\\\n" "\n" specific-template)))
		(when default-template
			(setq default-template (replace-regexp-in-string "\\\\n" "\n" default-template)))

		;; Return the specific template, or the default, or a hardcoded fallback.
		(or specific-template
				default-template
				"#+TITLE: %s\n\n* %s\n\n")))

(defun scriba--parse-config-property (property-name)
	"Parse a comma-separated property from the book config file."
	(let* ((root (scriba--book-root-folder))
				 (config-file (expand-file-name "book-config.org" root)))
		(when (file-exists-p config-file)
			(with-temp-buffer
				(insert-file-contents config-file)
				(goto-char (point-min))
				(when (re-search-forward (format "^[ \t]*:%s:[ \t]*\\(.*\\)$" property-name) nil t)
					(let ((value (match-string-no-properties 1)))
						(when (and value (not (string-empty-p value)))
							(mapcar #'string-trim
											(split-string value "," t "[ \t]*")))))))))

(defun scriba--get-config-directories ()
	"Get all directory configurations from book-config.org."
	(list
	 :content-dir (or (car (scriba--parse-config-property "SCRIBA_CONTENT_DIR")) "Content")
	 :content-types (scriba--parse-config-property "SCRIBA_CONTENT_TYPES")
	 :notes-dir (or (car (scriba--parse-config-property "SCRIBA_NOTES_DIR")) "Notes")
	 :note-categories (scriba--parse-config-property "NOTE_CATEGORIES")
	 :indices-dir (or (car (scriba--parse-config-property "SCRIBA_INDICES_DIR")) "Indices")
	 :extra-dirs (scriba--parse-config-property "SCRIBA_EXTRA_DIRS")))

(defun scriba--ensure-directory-exists (dir-path)
	"Create directory if it doesn't exist, return t if created."
	(unless (file-exists-p dir-path)
		(make-directory dir-path t)
		(message "Created directory: %s" dir-path)
		t))

(defun scriba--directory-empty-p (dir-path)
	"Check if directory is empty (no files or subdirectories)."
	(when (file-directory-p dir-path)
		(let ((contents (directory-files dir-path nil "^[^.]")))
			(null contents))))

(defun scriba--get-expected-directories (config root)
	"Get list of all directories that should exist based on config."
	(let ((expected '()))
		;; Main directories
		(push (plist-get config :content-dir) expected)
		(push (plist-get config :notes-dir) expected)
		(push (plist-get config :indices-dir) expected)

		;; Content type subdirectories
		(dolist (content-type (plist-get config :content-types))
			(push (format "%s/%s" (plist-get config :content-dir) content-type) expected))

		;; Note category subdirectories
		(dolist (category (plist-get config :note-categories))
			(push (format "%s/%s" (plist-get config :notes-dir) category) expected))

		;; Extra directories
		(dolist (extra-dir (plist-get config :extra-dirs))
			(push extra-dir expected))

		;; Convert to absolute paths
		(mapcar (lambda (dir) (expand-file-name dir root)) expected)))

(defun scriba--find-unused-scriba-directories (config root)
	"Find Scriba-managed directories that are no longer in config and are empty."
	(let ((expected-dirs (scriba--get-expected-directories config root))
				(unused-dirs '()))

		;; Check content directory subdirectories
		(let ((content-dir (expand-file-name (plist-get config :content-dir) root)))
			(when (file-exists-p content-dir)
				(dolist (subdir (directory-files content-dir t "^[^.]"))
					(when (and (file-directory-p subdir)
										 (not (member subdir expected-dirs))
										 (scriba--directory-empty-p subdir))
						(push subdir unused-dirs)))))

		;; Check notes directory subdirectories
		(let ((notes-dir (expand-file-name (plist-get config :notes-dir) root)))
			(when (file-exists-p notes-dir)
				(dolist (subdir (directory-files notes-dir t "^[^.]"))
					(when (and (file-directory-p subdir)
										 (not (member subdir expected-dirs))
										 (scriba--directory-empty-p subdir))
						(push subdir unused-dirs)))))

		;; Check for unused extra directories (only if they're empty)
		(dolist (file (directory-files root t "^[^.]"))
			(when (and (file-directory-p file)
								 (not (member file expected-dirs))
								 (scriba--directory-empty-p file)
								 ;; Only consider directories that might be Scriba-managed
								 ;; (avoid system directories like .git, etc.)
								 (not (string-match-p "^\\." (file-name-nondirectory file)))
								 ;; Don't remove core project files area
								 (not (member (file-name-nondirectory file) '("." ".."))))
				;; Only add if it looks like it might be a Scriba directory
				(let ((dirname (file-name-nondirectory file)))
					(when (or (string-match-p "^[A-Z]" dirname) ; Capitalized like typical Scriba dirs
										(member dirname '("content" "notes" "indices" "worldbuilding" "images")))
						(push file unused-dirs)))))

		unused-dirs))


(defun scriba--sanitize-string (str)
	"Sanitize STR for use in file names and Org links by converting it to CamelCase.
Removes special characters and spaces, and capitalizes the first letter of each word.
e.g., \"My new character's profile\" -> \"MyNewCharactersProfile\"."
	(let* ((trimmed (string-trim str))
				 ;; Replace any non-alphanumeric characters with a space to ensure word separation.
				 (spaced (replace-regexp-in-string "[^[:alnum:]]+" " " trimmed))
				 ;; Split the string into a list of words. The 't' removes empty strings.
				 (words (split-string spaced " +" t)))
		;; Map over the list of words, capitalize each one, and join them together.
		(mapconcat #'capitalize words "")))

(defun scriba--book-root-folder (&optional start-dir)
	"Return the root folder of the Scriba project.
If START-DIR is nil, uses current buffer's directory.
Throws an error if not in a project."
	(let ((dir (or start-dir (when buffer-file-name (file-name-directory buffer-file-name)) default-directory)))
		(unless dir (error "Cannot determine current directory"))
		(let ((current-dir (expand-file-name dir)))
			(while (and current-dir (not (file-exists-p (concat current-dir scriba--folder-separator scriba--config-filename))))
				(let ((parent (file-name-directory (directory-file-name current-dir))))
					(if (equal parent current-dir)
							(setq current-dir nil) ; Reached filesystem root
						(setq current-dir parent))))
			(unless current-dir (error "Not in a Scriba project. Config file '%s' not found." scriba--config-filename))
			(file-name-as-directory current-dir))))

(defun scriba--get-property-value (property-name file-path)
	"Get the value of a keyword or property from an Org file.
It first searches for a file-level keyword (e.g., '#+TITLE:').
If not found, it then searches for the property in the first
property drawer it finds."
	(when (and file-path (file-readable-p file-path))
		(with-temp-buffer
			(insert-file-contents-literally file-path)
			(org-mode)
			(let (value)
				;; 1. Prioritize file-level keywords (e.g., #+TITLE:)
				(goto-char (point-min))
				(when (re-search-forward
							 (format "^#\\+%s:[ \t]*\\(.*\\)" (upcase property-name))
							 nil t)
					(setq value (string-trim (match-string 1))))

				;; 2. If no keyword was found, look for a property drawer.
				(unless value
					(goto-char (point-min))
					(when (re-search-forward org-property-drawer-re nil t)
						(setq value (org-entry-get (point) property-name t))))

				;; Return the found value.
				value))))

(defun scriba--get-config-property (property-name)
	"Get PROPERTY-NAME from the book's config file."
	(let* ((root (scriba--book-root-folder))
				 (config-file (concat root scriba--config-filename)))
		(scriba--get-property-value property-name config-file)))

(defun scriba--get-customizable-folder-name (prop-key default-name)
	"Get folder name from config or use DEFAULT-NAME."
	(or (scriba--get-config-property prop-key) default-name))

(defun scriba--get-project-path (type &optional root-dir)
	"Return the full path to a standard project directory TYPE.
TYPE can be 'root, 'content, 'notes, 'indices, or 'config-file, 'main-file."
	(let ((root (or root-dir (scriba--book-root-folder))))
		(pcase type
			('root root)
			('config-file (concat root scriba--config-filename))
			('main-file (concat root scriba--main-filename))
			('content (concat root
												(scriba--get-customizable-folder-name scriba--prop-content-dir scriba--default-content-folder)
												scriba--folder-separator))
			('notes (concat root
											(scriba--get-customizable-folder-name scriba--prop-notes-dir scriba--default-notes-folder)
											scriba--folder-separator))
			('indices (concat root
												(scriba--get-customizable-folder-name scriba--prop-indices-dir scriba--default-indices-folder)
												scriba--folder-separator))
			(_ (error "Unknown project path type: %s" type)))))

(defun scriba--add-link-to-index (index-file link-target link-description &optional heading-level)
	"Add a link to the specified INDEX-FILE.
HEADING-LEVEL defaults to 2 if not provided."
	(let ((level (or heading-level 2)))
		(with-current-buffer (find-file-noselect index-file)
			(goto-char (point-max))
			(unless (bolp) (insert "\n"))
			(insert (make-string level ?*) " "
							(format "[[file:%s][%s]]\n" link-target link-description))
			(save-buffer)
			(unless (get-file-buffer index-file) ; If it wasn't already open
				(kill-buffer (current-buffer))))))

(defun scriba--string-to-file (str filename)
	"Write STR to FILENAME, creating parent directories if needed."
	(make-directory (file-name-directory filename) t)
	(with-temp-buffer
		(insert str)
		(write-file filename nil)))

(defun scriba--get-all-content-files ()
	"Return a list of all content files in the project, searching recursively.
This will find files in subdirectories like `Content/Chapter/` etc."
	(let ((content-dir (scriba--get-project-path 'content)))
		;; Ensure the content directory actually exists before trying to search it.
		(when (file-directory-p content-dir)
			;; Use the recursive version of directory-files.
			(directory-files-recursively content-dir (concat scriba--file-ending "$")))))

(defun scriba--get-all-note-files ()
	"Return a list of all note files across all categories."
	(let* ((notes-root-dir (scriba--get-project-path 'notes))
				 (all-note-files '()))
		(dolist (category-dir (directory-files notes-root-dir t "^[^.]"))
			(when (file-directory-p category-dir)
				(dolist (note-file (directory-files category-dir t (concat "^[^.].*\\" scriba--file-ending "$")))
					(push note-file all-note-files))))
		all-note-files))

;;;; Core Functionality
;;;###autoload
(defun scriba-clean-unused-directories ()
	"Remove unused empty directories that are no longer in the config."
	(interactive)
	(let* ((root (scriba--book-root-folder))
				 (config (scriba--get-config-directories))
				 (unused-dirs (scriba--find-unused-scriba-directories config root)))

		(unless root
			(error "Not in a Scriba project"))

		(if unused-dirs
				(progn
					(with-output-to-temp-buffer "*Scriba Unused Directories*"
						(princ "Unused Empty Directories Found\n")
						(princ "==============================\n\n")
						(princ "The following empty directories are not in your current config:\n\n")
						(dolist (dir unused-dirs)
							(princ (format "  %s\n" (file-relative-name dir root))))
						(princ "\nUse `scriba-sync-folder-structure` with prefix argument (C-u) to remove them.\n"))
					(message "Found %d unused empty directories - see *Scriba Unused Directories* buffer"
									 (length unused-dirs)))
			(message "No unused empty directories found"))))

;;;###autoload
(defun scriba-show-config-structure ()
	"Display the current configuration and what directories would be created."
	(interactive)
	(let* ((root (scriba--book-root-folder))
				 (config (scriba--get-config-directories)))

		(unless root
			(error "Not in a Scriba project"))

		(with-output-to-temp-buffer "*Scriba Config Structure*"
			(princ "Scriba Project Configuration\n")
			(princ "============================\n\n")
			(princ (format "Project Root: %s\n\n" root))

			(princ "Main Directories:\n")
			(princ (format "  %s/\n" (plist-get config :content-dir)))
			(princ (format "  %s/\n" (plist-get config :notes-dir)))
			(princ (format "  %s/\n" (plist-get config :indices-dir)))

			(when (plist-get config :extra-dirs)
				(princ "\nExtra Directories:\n")
				(dolist (dir (plist-get config :extra-dirs))
					(princ (format "  %s/\n" dir))))

			(princ "\nContent Structure:\n")
			(dolist (type (plist-get config :content-types))
				(princ (format "  %s/%s/\n" (plist-get config :content-dir) type)))

			(princ "\nNotes Structure:\n")
			(dolist (category (plist-get config :note-categories))
				(princ (format "  %s/%s/\n" (plist-get config :notes-dir) category)))

			(princ "\nUse `scriba-sync-folder-structure` to create missing directories.\n"))))

;;;###autoload
(defun scriba-auto-sync-on-config-save ()
	"Automatically sync folder structure when book-config.org is saved."
	(when (and buffer-file-name
						 (string-match-p "book-config\\.org$" buffer-file-name)
						 (scriba--book-root-folder))
		(scriba-sync-folder-structure)))

;; Optional: Add hook to auto-sync when config is saved
;; (add-hook 'after-save-hook #'scriba-auto-sync-on-config-save)

;;;###autoload
(defun scriba-sync-folder-structure (&optional clean-unused)
	"Synchronize the project folder structure with book-config.org settings.
With prefix argument CLEAN-UNUSED, also remove unused empty directories."
	(interactive "P")
	(let* ((root (scriba--book-root-folder))
				 (config (scriba--get-config-directories))
				 (created-dirs '())
				 (removed-dirs '()))

		(unless root
			(error "Not in a Scriba project"))

		;; Create main directories
		(let ((content-dir (expand-file-name (plist-get config :content-dir) root))
					(notes-dir (expand-file-name (plist-get config :notes-dir) root))
					(indices-dir (expand-file-name (plist-get config :indices-dir) root)))

			(when (scriba--ensure-directory-exists content-dir)
				(push (plist-get config :content-dir) created-dirs))
			(when (scriba--ensure-directory-exists notes-dir)
				(push (plist-get config :notes-dir) created-dirs))
			(when (scriba--ensure-directory-exists indices-dir)
				(push (plist-get config :indices-dir) created-dirs))

			;; Create content type subdirectories
			(dolist (content-type (plist-get config :content-types))
				(let ((type-dir (expand-file-name content-type content-dir)))
					(when (scriba--ensure-directory-exists type-dir)
						(push (format "%s/%s" (plist-get config :content-dir) content-type) created-dirs))))

			;; Create note category subdirectories
			(dolist (category (plist-get config :note-categories))
				(let ((category-dir (expand-file-name category notes-dir)))
					(when (scriba--ensure-directory-exists category-dir)
						(push (format "%s/%s" (plist-get config :notes-dir) category) created-dirs))))

			;; Create extra directories
			(dolist (extra-dir (plist-get config :extra-dirs))
				(let ((extra-path (expand-file-name extra-dir root)))
					(when (scriba--ensure-directory-exists extra-path)
						(push extra-dir created-dirs)))))

		;; Clean unused directories if requested
		(when clean-unused
			(let ((unused-dirs (scriba--find-unused-scriba-directories config root)))
				(when unused-dirs
					(if (yes-or-no-p (format "Remove %d unused empty directories? " (length unused-dirs)))
							(dolist (dir unused-dirs)
								(condition-case err
										(progn
											(delete-directory dir)
											(push (file-relative-name dir root) removed-dirs)
											(message "Removed empty directory: %s" dir))
									(error (message "Failed to remove directory %s: %s" dir (error-message-string err)))))
						(message "Cleanup cancelled by user")))))

		;; Report results
		(let ((messages '()))
			(when created-dirs
				(push (format "Created %d directories: %s"
											(length created-dirs)
											(string-join (reverse created-dirs) ", "))
							messages))
			(when removed-dirs
				(push (format "Removed %d empty directories: %s"
											(length removed-dirs)
											(string-join (reverse removed-dirs) ", "))
							messages))
			(if messages
					(message "Scriba: %s" (string-join messages "; "))
				(message "Scriba: All directories up to date - no changes needed")))))

;;;###autoload
(defun scriba-generate-relationship-graph ()
	"Generate a clickable SVG relationship graph of all project files using Graphviz.
The SVG is saved in the project root and opened in the default browser."
	(interactive)
	;; 1. Check for the Graphviz `dot` command-line tool.
	(unless (executable-find "dot")
		(error "Graphviz 'dot' command not found. Please install Graphviz."))

	(let* ((root (scriba--book-root-folder))
				 (nodes '())
				 (edges '())
				 (dot-file-path (concat root "project-graph.dot"))
				 (svg-file-path (concat root "project-graph.svg")))

		(message "Scanning project to build relationship graph...")

		;; 2. Data Extraction: Iterate through all project files to define nodes and edges.
		(let ((all-project-files
					 (append (directory-files-recursively (scriba--get-project-path 'content root) "\\.org$")
									 (scriba--get-all-note-files))))

			(dolist (file all-project-files)
				(let* ((node-id (scriba--sanitize-string (file-name-base file)))
							 (node-label (file-name-base file))
							 (node-type-raw (file-name-nondirectory (file-name-directory file)))
							 (node-type (if (string-match-p "Content" (file-name-directory file))
															(file-name-nondirectory (file-name-directory (file-name-directory file)))
														node-type-raw))
							 (node-attrs `(:label ,node-label
																		;; This is the magic: embed a file: URL into the SVG node.
																		:URL ,(format "file://%s" (expand-file-name file))
																		:shape "box"
																		:style "rounded")))
					(push (cons node-id node-attrs) nodes)

					;; Scan the current file for links to create edges.
					(with-temp-buffer
						(insert-file-contents file)
						(org-mode)
						(goto-char (point-min))
						(while (re-search-forward "\\[\\[file:\\([^]]+\\)\\]" nil t)
							(let* ((linked-file-relative (match-string 1))
										 (linked-file-abs (expand-file-name linked-file-relative (file-name-directory file)))
										 (target-node-id (when (file-exists-p linked-file-abs)
																			 (scriba--sanitize-string (file-name-base linked-file-abs)))))
								(when target-node-id
									(push (format "\"%s\" -> \"%s\";" node-id target-node-id) edges))))))))

		(message "Generating Graphviz DOT file...")

		;; 3. DOT File Generation: Write the collected data into a .dot file.
		(with-temp-buffer
			(insert "digraph ScribaProject {\n")
			(insert "  graph [rankdir=LR, splines=true, overlap=false];\n")
			(insert "  node [style=rounded];\n\n")

			;; Write node definitions
			(dolist (node-entry nodes)
				(let* ((id (car node-entry))
							 (attrs (cdr node-entry))
							 (label (plist-get attrs :label))
							 (url (plist-get attrs :URL)))
					(insert (format "  \"%s\" [label=\"%s\", URL=\"%s\"];\n" id label url))))

			(insert "\n")

			;; Write edge definitions
			(dolist (edge (delete-dups (sort edges #'string-lessp)))
				(insert (format "  %s\n" edge)))

			(insert "}\n")
			(write-file dot-file-path))

		(message "Compiling SVG from DOT file...")

		;; 4. SVG Compilation: Run the `dot` command to convert the .dot file to a clickable SVG.
		(let ((exit-code (call-process "dot" nil nil nil "-Tsvg" dot-file-path "-o" svg-file-path)))
			(if (= exit-code 0)
					(progn
						(message "Graph generated successfully! Opening %s..." svg-file-path)
						;; 5. Open the final SVG in the user's default web browser.
						(browse-url (format "file://%s" (expand-file-name svg-file-path))))
				(error "Graphviz 'dot' command failed with exit code %d. Check the .dot file for errors." exit-code)))))

(defun scriba-jump-to-file ()
	"Provide a unified, searchable list of all project files to jump to."
	(interactive)
	(let* ((root (scriba--book-root-folder))
				 (all-files '())
				 (choice-map (make-hash-table :test 'equal))
				 (content-files (scriba--get-all-content-files)))

		;; 1. Gather all files and create descriptive labels
		;; Content files - if the function returns nil, try to find them manually
		(if content-files
				(dolist (file content-files)
					(let* ((relative-path (file-relative-name file root))
								 (dir-path (file-name-directory relative-path))
								 (dir-name (file-name-nondirectory (directory-file-name dir-path)))
								 (type (if (string-empty-p dir-name) "Content" (capitalize dir-name)))
								 (label (format "%s: %s" type (file-name-base file))))
						(push label all-files)
						(puthash label file choice-map)))
			;; Fallback: manually search for content files
			(when (file-exists-p (expand-file-name "Content" root))
				(let ((content-org-files (directory-files-recursively
																	(expand-file-name "Content" root) "\\.org$")))
					(dolist (file content-org-files)
						(let* ((relative-path (file-relative-name file root))
									 (dir-path (file-name-directory relative-path))
									 (dir-name (file-name-nondirectory (directory-file-name dir-path)))
									 (type (if (string-empty-p dir-name) "Content" (capitalize dir-name)))
									 (label (format "%s: %s" type (file-name-base file))))
							(push label all-files)
							(puthash label file choice-map))))))

		;; Note files
		(dolist (file (scriba--get-all-note-files))
			(let* ((relative-path (file-relative-name file root))
						 (dir-path (file-name-directory relative-path))
						 (dir-name (file-name-nondirectory (directory-file-name dir-path)))
						 (type (if (string-empty-p dir-name) "Note" (capitalize dir-name)))
						 (label (format "%s: %s" type (file-name-base file))))
				(push label all-files)
				(puthash label file choice-map)))

		;; Add main and config files
		(let ((main-label "Project: Main File")
					(config-label "Project: Config File"))
			(push main-label all-files)
			(puthash main-label (scriba--get-project-path 'main-file root) choice-map)
			(push config-label all-files)
			(puthash config-label (scriba--get-project-path 'config-file root) choice-map))

		;; 2. Prompt the user
		(let* ((all-files-sorted (sort all-files #'string-lessp))
					 (selection (completing-read "Jump to: " all-files-sorted nil t)))
			(when (and selection (not (string-empty-p selection)))
				(find-file (gethash selection choice-map))))))

;;;###autoload
(defun scriba-new-character (character-name)
	"Create a new character profile note.
This is a specialized wrapper around `scriba-new-note`."
	(interactive (list (read-string "New Character Name: ")))
	(let* ((root (scriba--book-root-folder))
				 (safe-name (scriba--sanitize-string character-name))
				 (note-path (concat (scriba--get-project-path 'notes root)
														"Characters" scriba--folder-separator
														safe-name scriba--file-ending)))
		;; First, ensure 'Characters' is a valid category.
		(unless (member "Characters" (scriba--get-note-categories))
			(error "Note category 'Characters' not found in %s. Please add it." scriba--config-filename))

		;; Use a custom template and bypass the standard `scriba-new-note` prompts
		(make-directory (file-name-directory note-path) t)
		(when (file-exists-p note-path)
			(error "Character file '%s' already exists." note-path))

		(scriba--string-to-file
		 (format scriba-character-template character-name)
		 note-path)

		;; Add link to the Characters index
		(let* ((indices-dir-name (scriba--get-customizable-folder-name scriba--prop-indices-dir scriba--default-indices-folder))
					 (notes-dir-name (scriba--get-customizable-folder-name scriba--prop-notes-dir scriba--default-notes-folder))
					 (category-index-path (concat (scriba--get-project-path 'indices root)
																				"notes-Characters.org")))
			(scriba--add-link-to-index category-index-path
																 (concat ".." scriba--folder-separator notes-dir-name scriba--folder-separator "characters" scriba--folder-separator safe-name scriba--file-ending)
																 character-name))

		(message "Character '%s' created." character-name)
		(find-file note-path)))

;;;###autoload
(defun scriba-compile-master-outline ()
	"Compile a master outline file using #+INCLUDE for all sections.
This creates a single file for viewing or exporting the entire manuscript."
	(interactive)
	(let* ((root (scriba--book-root-folder))
				 (main-file (scriba--get-project-path 'main-file root))
				 (output-file (concat root scriba-master-outline-filename))
				 (book-title (or (scriba--get-property-value "TITLE" main-file) "My Book"))
				 (book-author (or (scriba--get-property-value "AUTHOR" main-file) scriba-author))
				 (sections-index-path (concat (scriba--get-project-path 'indices root) scriba--sections-index-filename))
				 (include-content "")
				 (section-files '()))

		;; Get sections in order from the index file
		(when (file-readable-p sections-index-path)
			(with-temp-buffer
				(insert-file-contents sections-index-path)
				(org-mode)
				(goto-char (point-min))
				(while (re-search-forward "^\\*+ \\[\\[file:\\([^]]+\\)\\]\\[[^]]+\\]\\]" nil t)
					;; Get a relative path from the project root for the INCLUDE statement
					(let ((abs-path (expand-file-name (match-string 1) (file-name-directory sections-index-path))))
						(push (file-relative-name abs-path root) section-files)))))
		(setq section-files (nreverse section-files))

		;; Fallback to grabbing all files recursively if index is empty
		(unless section-files
			(let ((content-dir (scriba--get-project-path 'content root)))
				(setq section-files (directory-files-recursively content-dir "\\.org$"))
				;; Make paths relative to the project root for the include statement
				(setq section-files (mapcar (lambda (f) (file-relative-name f root)) section-files))))

		;; Build the string of #+INCLUDE statements
		(dolist (section-file section-files)
			(setq include-content (concat include-content
																		(format "#+INCLUDE: \"%s\"\n" section-file))))

		(scriba--string-to-file
		 (format "#+TITLE: %s\n#+AUTHOR: %s\n#+OPTIONS: toc:t num:t ^:{}\n\n%s"
						 book-title
						 book-author
						 include-content)
		 output-file)
		(message "Master outline compiled into %s" output-file)
		(find-file output-file)))

;;;###autoload
(defun scriba-new-book (book-name book-path)
	"Create a new Scriba project.
BOOK-NAME is the title of the book.
BOOK-PATH is the directory where the project folder will be created."
	(interactive
	 (list (read-string "Book Name: ")
				 (read-directory-name "Parent Directory for Book Project: ")))
	(let* ((safe-book-name (scriba--sanitize-string book-name))
				 (project-root (file-name-as-directory (concat (file-name-as-directory book-path) safe-book-name)))
				 (content-folder-name scriba--default-content-folder)
				 (notes-folder-name scriba--default-notes-folder)
				 (indices-folder-name scriba--default-indices-folder)
				 (content-dir (concat project-root content-folder-name scriba--folder-separator))
				 (notes-dir (concat project-root notes-folder-name scriba--folder-separator))
				 (indices-dir (concat project-root indices-folder-name scriba--folder-separator))
				 (extra-dirs-str nil)
				 (extra-dirs-list '()))

		(when (file-exists-p project-root)
			(error "Project directory '%s' already exists." project-root))

		(make-directory project-root t)
		(make-directory content-dir t)
		(make-directory notes-dir t)
		(make-directory indices-dir t)

		;; Create config file first, as it might define extra_dirs
		(scriba--string-to-file
		 (format scriba-config-file-template
						 book-name
						 scriba-author
						 scriba--note-categories-property
						 scriba--prop-content-dir scriba--default-content-folder
						 scriba--prop-content-types ; NEW
						 scriba--prop-notes-dir scriba--default-notes-folder
						 scriba--prop-indices-dir scriba--default-indices-folder
						 scriba--prop-extra-dirs
						 scriba--note-categories-property
						 scriba--prop-content-dir
						 scriba--prop-notes-dir
						 scriba--prop-indices-dir
						 scriba--prop-extra-dirs
						 scriba--prop-content-types ; NEW
						 )
		 (concat project-root scriba--config-filename))

		;; Read SCRIBA_EXTRA_DIRS from the newly created config
		(setq extra-dirs-str (scriba--get-property-value scriba--prop-extra-dirs (concat project-root scriba--config-filename)))
		(when (and extra-dirs-str (not (string-empty-p extra-dirs-str)))
			(setq extra-dirs-list (split-string extra-dirs-str "[, ]+" t))
			(dolist (extra-dir extra-dirs-list)
				(make-directory (concat project-root (scriba--sanitize-string extra-dir) scriba--folder-separator) t)))

		;; Create main file
		(let ((main-file-content
					 (format scriba-main-file-template
									 book-name
									 scriba-author
									 indices-folder-name ; Use default here, customization applies on read
									 scriba--sections-index-filename
									 scriba--config-filename)))
			(when extra-dirs-list
				(let ((extra-links (mapconcat
														(lambda (dir)
															(format "  ** [[file:%s/][My %s]]" (scriba--sanitize-string dir) dir))
														extra-dirs-list "\n")))
					(setq main-file-content
								(replace-regexp-in-string
								 "^\\* Other Project Areas\n  ;; Links to extra directories.*$"
								 (format "* Other Project Areas\n%s" extra-links)
								 main-file-content))))
			(scriba--string-to-file main-file-content (concat project-root scriba--main-filename)))


		;; Create initial Sections Index
		(scriba--string-to-file
		 (format "#+TITLE: Content Sections Index for %s\n\n" book-name)
		 (concat indices-dir scriba--sections-index-filename))

		;; Create the optional style file in the project root
		(scriba--string-to-file
		 scriba-style-file-contents
		 (concat project-root "scriba-style.org"))

		;; Initialize Git repo and .gitignore
		(when scriba-git-init-on-new-book
			(with-current-buffer (find-file-noselect (concat project-root scriba--main-filename))
				(call-process "git" nil 0 0 "-C" (shell-quote-argument project-root) "init")
				(scriba--string-to-file scriba-default-gitignore-contents
																(concat project-root scriba--gitignore-filename))
				(call-process "git" nil 0 0 "-C" (shell-quote-argument project-root) "add" ".")
				(call-process "git" nil 0 0 "-C" (shell-quote-argument project-root) "commit" "-m" "Initial project setup by Scriba.")
				(message "Git repository initialized.")))

		(message "Scriba project '%s' created at %s" book-name project-root)
		(find-file (concat project-root scriba--main-filename))))

(defun scriba--get-note-categories ()
	"Return a list of note categories from the centralized config reader."
	(let ((config (scriba--get-config-directories)))
		(plist-get config :note-categories)))

(defun scriba--get-content-types ()
	"Return a list of content types from the centralized config reader."
	(let ((config (scriba--get-config-directories)))
		(plist-get config :content-types)))

;;;###autoload
(defun scriba-new-content (type name)
	"Create a new content file of a specific TYPE using templates from book-config.org.
This command creates the file in a corresponding subdirectory under the Content folder."
	(interactive
	 (let ((types (or (scriba--get-content-types) '("Chapter" "Scene"))))
		 (list (completing-read "Content Type: " types nil t (car types))
					 (read-string "Content Name/Title: "))))

	(let* ((root (scriba--book-root-folder))
				 (safe-type-name (scriba--sanitize-string type))
				 (safe-file-name (scriba--sanitize-string name))
				 (content-dir (scriba--get-project-path 'content root))
				 (type-subdir (concat content-dir safe-type-name scriba--folder-separator))
				 (file-path (concat type-subdir safe-file-name scriba--file-ending))
				 (template (scriba--get-template-for-type type))
				 (final-content "")
				 (final-title name))

		(make-directory type-subdir t)

		(when (file-exists-p file-path)
			(error "%s file '%s' already exists." type name))

		;; Format the template with the correct arguments based on type.
		;; This allows different templates to have different numbers of placeholders.
		(cond
		 ((string-equal (downcase type) "chapter")
			(let ((num (1+ (length (directory-files type-subdir t "\\.org$")))))
				(setq final-title (format "Chapter %d: %s" num name))
				;; The Chapter template expects two arguments: number and title.
				(setq final-content (format template num name))))
		 ;; For all other types, we assume the template expects one argument: the name.
		 (t
			(setq final-content (format template name))))

		(scriba--string-to-file final-content file-path)

		;; Add link to the main sections index.
		(let* ((index-path (concat (scriba--get-project-path 'indices root) scriba--sections-index-filename))
					 (link-target (file-relative-name file-path (file-name-directory index-path))))
			(scriba--add-link-to-index index-path link-target final-title))

		(message "%s '%s' created." type name)
		(find-file file-path)))

;;;###autoload
(defun scriba-new-note (category note-name)
	"Create a new note within a specific category."
	(interactive
	 (let ((categories (scriba--get-note-categories)))
		 (unless categories
			 (error "No note categories defined. Please add '%s' property to '%s'."
							scriba--note-categories-property scriba--config-filename))
		 (list (completing-read "Note Category: " categories nil t)
					 (read-string "New Note Name: "))))

	(let* ((root (scriba--book-root-folder))
				 (safe-category-name (scriba--sanitize-string category))
				 (safe-note-name (scriba--sanitize-string note-name))
				 (notes-dir-name (scriba--get-customizable-folder-name scriba--prop-notes-dir scriba--default-notes-folder))
				 (indices-dir-name (scriba--get-customizable-folder-name scriba--prop-indices-dir scriba--default-indices-folder))
				 (category-dir (concat (scriba--get-project-path 'notes root) safe-category-name scriba--folder-separator))
				 (note-file-name (concat safe-note-name scriba--file-ending))
				 (note-path (concat category-dir note-file-name))
				 (category-index-filename (concat scriba--note-category-index-prefix safe-category-name scriba--file-ending))
				 (category-index-path (concat (scriba--get-project-path 'indices root) category-index-filename))
				 (main-file-path (scriba--get-project-path 'main-file root)))

		(make-directory category-dir t) ; Ensure category subfolder exists

		(when (file-exists-p note-path)
			(error "Note file '%s' already exists in category '%s'." note-path category))

		;; Create note file
		(scriba--string-to-file
		 (format scriba-note-template-string
						 note-name category category note-name scriba--appearances-heading)
		 note-path)

		;; Create or update category index
		(unless (file-exists-p category-index-path)
			(scriba--string-to-file
			 (format "#+TITLE: %s Notes Index\n\n" category)
			 category-index-path)
			;; Add link to this new category index in main.org
			(with-current-buffer (find-file-noselect main-file-path)
				(goto-char (point-max))
				(when (re-search-backward "^\\* Notes Indices" nil t)
					(goto-char (line-end-position))
					(insert (format "\n  ** [[file:%s/%s][%s Notes]]"
													indices-dir-name
													category-index-filename
													category))
					(save-buffer))
				(unless (get-file-buffer main-file-path)
					(kill-buffer (current-buffer)))))

		(scriba--add-link-to-index category-index-path
															 (concat ".." scriba--folder-separator notes-dir-name scriba--folder-separator safe-category-name scriba--folder-separator note-file-name)
															 note-name)

		(message "Note '%s' created in category '%s'." note-name category)
		(find-file note-path)))

;;;###autoload
(defun scriba-update-note-appearances ()
	"Scan content sections for mentions of notes and update their 'Appearances' section."
	(interactive)
	(let* ((root (scriba--book-root-folder))
				 (all-note-files-with-titles '())
				 (content-files (scriba--get-all-content-files)))

		(message "Scanning for note appearances...")

		;; 1. Gather all note files and their titles
		(dolist (note-file (scriba--get-all-note-files))
			(let ((note-title (scriba--get-property-value "TITLE" note-file)))
				(when note-title
					(setq note-title (replace-regexp-in-string " *([^(]*)$" "" note-title)) ; Remove (Category) part
					(push (cons note-file note-title) all-note-files-with-titles))))

		(unless all-note-files-with-titles
			(message "No note files found to process.")
			(cl-return))

		;; 2. For each note, scan content files
		(dolist (note-entry all-note-files-with-titles)
			(let* ((note-file (car note-entry))
						 (note-title-raw (cdr note-entry))
						 (note-title-regexp (concat "\\<" (regexp-quote note-title-raw) "\\>")) ; Whole word match
						 (appearances '())
						 (note-file-buffer (find-file-noselect note-file)))

				(with-current-buffer note-file-buffer
					(goto-char (point-min))
					(when (re-search-forward (concat "^\\* " (regexp-quote scriba--appearances-heading)) nil t)
						(let ((beg (line-end-position)))
							(org-end-of-subtree t t)
							(delete-region beg (point)))))

				(dolist (content-file content-files)
					(let ((content-title (scriba--get-property-value "TITLE" content-file)))
						(with-temp-buffer
							(insert-file-contents content-file)
							(goto-char (point-min))
							;; Search for the note title as a whole word, case-insensitively
							(while (re-search-forward note-title-regexp nil t)
								(push (format "[[file:%s][%s]]"
															(file-relative-name content-file (file-name-directory note-file))
															(or content-title (file-name-nondirectory content-file)))
											appearances)
								;; Avoid re-matching the same instance if the list items themselves contain the title
								(goto-char (match-end 0)))
							)
						)
					)

				(setq appearances (delete-dups appearances)) ; Remove duplicate links to the same file

				(when appearances
					(with-current-buffer note-file-buffer
						(goto-char (point-min))
						(unless (re-search-forward (concat "^\\* " (regexp-quote scriba--appearances-heading)) nil t)
							(goto-char (point-max))
							(unless (bolp) (insert "\n"))
							(insert (format "* %s\n" scriba--appearances-heading)))
						(goto-char (line-end-position)) (insert "\n")
						(dolist (appearance (reverse appearances)) ; Insert in found order
							(insert "- " appearance "\n"))
						(save-buffer)
						(unless (get-file-buffer note-file) ; If we opened it with find-file-noselect and it wasn't already open
							(kill-buffer note-file-buffer))))))
		(message "Note appearances updated.")))

;;;###autoload
(defun scriba-collate-book (output-file)
	"Collate all content sections into a single OUTPUT-FILE."
	(interactive
	 (list (read-file-name "Output file for collated book: "
												 (concat (scriba--book-root-folder) "collated-book" scriba--file-ending)
												 nil nil nil)))
	(let* ((root (scriba--book-root-folder))
				 (main-file (scriba--get-project-path 'main-file root))
				 (book-title (or (scriba--get-property-value "TITLE" main-file) "My Book"))
				 (book-author (or (scriba--get-property-value "AUTHOR" main-file) scriba-author))
				 (sections-index-path (concat (scriba--get-project-path 'indices root) scriba--sections-index-filename))
				 (collated-content "")
				 (section-files '()))

		(when (file-readable-p sections-index-path)
			(with-temp-buffer
				(insert-file-contents sections-index-path)
				(org-mode)
				(goto-char (point-min))
				(while (re-search-forward "^\\*+ \\[\\[file:\\([^]]+\\)\\]\\[[^]]+\\]\\]" nil t)
					(push (expand-file-name (match-string 1) (file-name-directory sections-index-path)) section-files))))
		(setq section-files (nreverse section-files))

		(unless section-files
			(setq section-files (scriba--get-all-content-files)))

		(dolist (section-file section-files)
			(when (file-readable-p section-file)
				(with-temp-buffer
					(insert-file-contents section-file)
					(goto-char (point-min))
					(while (re-search-forward "^#\\+" nil t)
						(if (looking-at "TITLE:")
								(let* ((title-line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
											 (title-val (replace-regexp-in-string "^#\\+TITLE:[ \t]*" "" title-line)))
									(delete-region (line-beginning-position) (line-end-position 1))
									(insert (format "* %s\n" title-val)))
							(delete-region (line-beginning-position) (line-end-position 1))))
					(setq collated-content (concat collated-content (buffer-string) "\n\n")))))

		(scriba--string-to-file
		 (format "#+TITLE: %s\n#+AUTHOR: %s\n#+OPTIONS: toc:nil num:nil ^:{}\n\n%s" ; Common export options
						 book-title
						 book-author
						 collated-content)
		 output-file)
		(message "Book collated into %s" output-file)
		(find-file output-file)))

;;;; Writing Support & Stats
;;;###autoload
(defun scriba-backup-project ()
	"Create a timestamped .zip backup of the entire project directory.
Excludes the .git directory and other backup files."
	(interactive)
	(let* ((root (scriba--book-root-folder))
				 (project-name (file-name-nondirectory (directory-file-name root)))
				 (timestamp (format-time-string "%Y%m%d-%H%M%S"))
				 ;; Default to saving the backup in the parent directory
				 (backup-dir (read-directory-name "Save backup in directory: " (concat root "../")))
				 (zip-file (expand-file-name (format "%s-backup-%s.zip" project-name timestamp) backup-dir)))

		(if (not (executable-find "zip"))
				(error "The 'zip' command is not in your system's PATH.")
			(progn
				(message "Backing up project to %s ..." zip-file)
				(call-process "zip" nil nil nil
											"-r" zip-file "."
											"-x" ".git/*" "*.zip" "*.elc" "*/.DS_Store" "*/auto-save-list/*"
											:workdir root)
				(message "Project backup created successfully at %s" zip-file)))))

(defun scriba--calculate-word-count-in-buffer ()
	"Core logic for word counting, using the robust `org-element-map`.
This is a non-interactive worker function."
	(let ((count 0)
				(org-element-use-cache nil)) ; ensure fresh parse
		;; `org-element-map' is the correct way to iterate over parsed elements.
		;; It is faster and more reliable than a manual `while` loop.
		(org-element-map (org-element-parse-buffer)
				;; List of element types that contain countable text.
				'(paragraph verse quote center-block example-block table-cell item)
			(lambda (element)
				;; For each matching element, get its plain-text contents.
				(let* ((contents (org-element-interpret-data (org-element-contents element)))
							 ;; Split the contents into words and add the length to our count.
							 (words (length (split-string contents "\\W+" t))))
					(setq count (+ count words)))))
		count))

;;;###autoload
(defun scriba-word-count-current-file ()
	"Calculate and display word count for the current Org mode buffer."
	(interactive)
	(unless (derived-mode-p 'org-mode) (error "Not an Org mode buffer"))
	(let ((count (scriba--calculate-word-count-in-buffer)))
		(message "Word count (approx.): %d" count)
		count))

(defun scriba-project-stats ()
	"Display statistics for the current Scriba project."
	(interactive)
	(let* ((root (scriba--book-root-folder))
				 (content-files (scriba--get-all-content-files))
				 (note-files (scriba--get-all-note-files))
				 (total-words 0)
				 (num-sections (length content-files))
				 (num-notes (length note-files))
				 (main-file (scriba--get-project-path 'main-file root))
				 (book-title (or (scriba--get-property-value "TITLE" main-file) "Untitled Project"))
				 (stats-buffer-name (format "*Scriba Stats: %s*" book-title))
				 (output ""))

		(dolist (file content-files)
			(with-temp-buffer
				(insert-file-contents file)
				(org-mode) ; ensure org functions are available
				;; Call the worker function directly within the temp buffer's context
				(setq total-words (+ total-words (scriba--calculate-word-count-in-buffer)))))

		(setq output (format "Project Statistics for: %s\n\n" book-title))
		(setq output (concat output (format "- Total Content Sections: %d\n" num-sections)))
		(setq output (concat output (format "- Total Notes: %d\n" num-notes)))
		(setq output (concat output (format "- Estimated Total Word Count (Content): %d\n" total-words)))

		;; Optional: Progress from main.org goals
		(let ((goals-progress (ignore-errors
														(with-temp-buffer
															(insert-file-contents main-file)
															(org-mode)
															(org-agenda-get-some-progress-percentage (org-find-exact-headline-in-buffer "Writing Goals"))))))
			(when goals-progress
				(setq output (concat output (format "- Writing Goals Progress: %s\n" goals-progress)))))

		(with-current-buffer (get-buffer-create stats-buffer-name)
			(erase-buffer)
			(insert output)
			(goto-char (point-min))
			(display-buffer (current-buffer)))))

;;;; Git Integration

;;;###autoload
(defun scriba-git-commit-project (commit-message)
	"Add all changes in the project and commit with COMMIT-MESSAGE."
	(interactive (list (read-string "Git commit message: "
																	(format "Scriba: %s update" (format-time-string "%Y-%m-%d %H:%M")))))
	(let* ((root (scriba--book-root-folder))
				 (default-directory root)) ; Crucial for vc commands and shell commands
		(unless (vc-registered root)
			(error "Project at %s is not under Git version control." root))
		(when (or (vc-git-untracked-files) (vc-git-dirty-files))
			(shell-command (format "git -C %s add ." (shell-quote-argument root)))
			(shell-command (format "git -C %s commit -m %s"
														 (shell-quote-argument root)
														 (shell-quote-argument commit-message)))
			(message "Project changes committed: %s" commit-message))
		(unless (or (vc-git-untracked-files) (vc-git-dirty-files))
			(message "No changes to commit."))))

;;;; Export Functionality

;;;###autoload
(defun scriba-export-project ()
	"Offer to collate and export the project to various formats."
	(interactive)
	(let* ((root (scriba--book-root-folder))
				 (default-collated-file (concat root "collated-book" scriba--file-ending))
				 (collated-file default-collated-file))
		(if (y-or-n-p (format "Collate book first to '%s'? " default-collated-file))
				(scriba-collate-book collated-file)
			(setq collated-file (read-file-name "Path to existing collated Org file: " root nil t)))

		(unless (file-exists-p collated-file)
			(error "Collated file '%s' not found or could not be created." collated-file))

		(find-file collated-file)
		(message "Collated book is '%s'. Use Org's export dispatcher (C-c C-e) to export." collated-file)
		(call-interactively 'org-export-dispatch)))

;;;; Hydra Menu

;;;###autoload
(defun scriba-hydra ()
	"Show the Scriba project hydra if in a project."
	(interactive)
	;; This is the critical line. It forces Emacs to load the full `scriba.el` file,
	;; which evaluates the `defhydra` macro above, defining `scriba-hydra/body`.
	(condition-case err
			(progn
				(scriba--book-root-folder)
				(scriba-hydra/body))
		(error (message "Scriba: %s (To start, use `scriba-new-book`)" (error-message-string err)))))

(defun scriba--get-current-book-name ()
	"Return the current book's name, or nil if not in a project."
	(let ((root (ignore-errors (scriba--book-root-folder))))
		(when root
			(or (scriba--get-property-value "TITLE" (scriba--get-project-path 'main-file root))
					(file-name-nondirectory (directory-file-name root))))))

(defun scriba--open-note-category-index ()
	"Prompt for a note category and open its index file."
	(interactive)
	(let* ((root (scriba--book-root-folder)) ; Ensures we are in a project
				 (categories (scriba--get-note-categories)))
		(unless categories
			(error "No note categories defined in config."))
		(let* ((chosen-category (completing-read "Open index for category: " categories nil t))
					 (safe-category-name (scriba--sanitize-string chosen-category))
					 (category-index-filename (concat scriba--note-category-index-prefix safe-category-name scriba--file-ending))
					 (category-index-path (concat (scriba--get-project-path 'indices root) category-index-filename)))
			(if (file-exists-p category-index-path)
					(find-file category-index-path)
				(message "Index file for category '%s' does not exist (expected at %s)."
								 chosen-category category-index-path)))))

;;;###autoload
(defhydra scriba-hydra
	(
	 :pre (condition-case err (scriba--book-root-folder)
					(error (message "Scriba: Not in a project. (To start, use `scriba-new-book`)")
								 (hydra-keyboard-quit)))
	 :title (or (scriba--get-current-book-name) "Scriba")
	 :color blue :hint nil :foreign-keys warn
	 )
	"
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
"
	;; Column 1: Create
	("c" scriba-new-content "New Content")
	("n" scriba-new-note "New Note")
	("C" scriba-new-character "New Character")
	("V" scriba-generate-relationship-graph "Visualize Graph")

	;; Column 2: View / Navigate / Compile
	("j" scriba-jump-to-file "Jump to File")
	("m" (lambda () (interactive) (find-file (scriba--get-project-path 'main-file))) "Main File")
	("i" (lambda () (interactive) (find-file (concat (scriba--get-project-path 'indices) scriba--sections-index-filename))) "Sections Index")
	("N" scriba--open-note-category-index "Note Category Index")
	("o" (lambda ()
				 (interactive)
				 (let* ((root (scriba--book-root-folder))
								(extra-dirs-str (scriba--get-config-property scriba--prop-extra-dirs))
								(choices '()))
					 (when (and extra-dirs-str (not (string-empty-p extra-dirs-str)))
						 (dolist (dir (split-string extra-dirs-str "[, ]+" t))
							 (push (cons dir (concat root (scriba--sanitize-string dir) scriba--folder-separator)) choices)))
					 (unless choices (error "No SCRIBA_EXTRA_DIRS defined in %s" scriba--config-filename))
					 (let ((chosen (completing-read "Open extra project folder: " (mapcar #'car choices) nil t)))
						 (find-file (cdr (assoc chosen choices))))))
	 "Open Extra Dir")
	("O" scriba-compile-master-outline "Compile Master Outline")
	("x" scriba-collate-book "Collate Book")
	("e" scriba-export-project "Export Project")
	("s" scriba-sync-folder-structure "Sync Folder Struc")

	;; Column 3: Actions
	("u" scriba-update-note-appearances "Update Appearances")
	("S" scriba-project-stats "Project Stats")
	("W" scriba-word-count-current-file "Word Count File")
	("d" (lambda () (interactive)
				 (if (fboundp 'writeroom-mode)
						 (writeroom-mode)
					 (error "Package 'writeroom-mode' is not installed."))) "Toggle Distraction-Free")
	("B" scriba-backup-project "Backup Project")
	("G" scriba-git-commit-project "Git Commit")

	("q" nil "Quit" :color blue))

;;;; Mode Definition

;;;###autoload
(define-minor-mode scriba-mode
	"A minor mode for working with Scriba book projects.
Activates keybindings (`C-c C-b h`) and menu items for Scriba
functionality in Org files within a Scriba project."
	:init-value nil
	:lighter " Scriba"
	:keymap scriba-mode-map
	:group 'scriba
	(if scriba-mode
			;; When mode is activated, add the menu.
			(easy-menu-define scriba-menu scriba-mode-map "Scriba Project Menu"
				`("Scriba"
					["New Book Project..." scriba-new-book t]
					"--"
					["New Content Section..." scriba-new-section scriba-mode]
					["New Note..." scriba-new-note scriba-mode]
					"--"
					["Update Note Appearances" scriba-update-note-appearances scriba-mode]
					["Collate Book Content..." scriba-collate-book scriba-mode]
					["Export Project..." scriba-export-project scriba-mode]
					"--"
					["Project Statistics" scriba-project-stats scriba-mode]
					["Word Count Current File" scriba-word-count-current-file (and scriba-mode (derived-mode-p 'org-mode))]
					"--"
					["Git Commit Project" scriba-git-commit-project (and scriba-mode (ignore-errors (vc-registered (scriba--book-root-folder))))]
					"--"
					;; This now correctly points to the interactive `scriba-hydra` command.
					["Show Project Hydra" scriba-hydra scriba-mode]))
		;; When mode is deactivated, the menu is removed automatically.
		))

;; This now correctly points to the interactive `scriba-hydra` command.
(define-key scriba-mode-map (kbd "C-c C-b h") 'scriba-hydra/body)

(defun scriba--maybe-activate-mode ()
	"Activate `scriba-mode` if the current buffer is an Org file inside a Scriba project."
	(when (and buffer-file-name (derived-mode-p 'org-mode))
		(condition-case nil
				(when (scriba--book-root-folder)
					(scriba-mode 1))
			(error nil))))

(add-hook 'find-file-hook #'scriba--maybe-activate-mode)
(add-hook 'org-mode-hook #'scriba--maybe-activate-mode)
(provide 'scriba)



;;; scriba.el ends here

;;;	"
;;;╭──────────────────────────── Scriba Project Menu ────────────────────────────────╮
;;;│ Navigation        │ Creation          │ Actions                │ Git            │
;;;├───────────────────┼───────────────────┼────────────────────────┼────────────────┤
;;;│ _m_: Main File    │ _s_: New Section  │ _u_: Update Appearances│ _C_: Commit    │
;;;│ _F_: Config File  │ _n_: New Note     │ _x_: Collate Book      │                │
;;;│ _i_: Sections Idx │                   │ _e_: Export Project    │                │
;;;│ _N_: Note Cat Idx │                   │ _S_: Project Stats     │                │
;;;│ _o_: Open Other...│                   │ _W_: Word Count File   │                │
;;;╰───────────────────┴───────────────────┴────────────────────────┴────────────────╯
;;;Press 'q' to quit.
;;;"
