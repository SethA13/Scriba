;;; scriba.el --- An Org mode system for book writing -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Seth Adair
;; Author: Seth Adair <seth1309@gmail.com>
;; Maintainer: Seth Adair <seth1309@gmail.com>
;; URL: https://github.com/SethA13/Scriba
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
(defvar scriba--template-processing-depth 0
	"Track template processing depth to prevent infinite loops.")
(defvar scriba--template-variable-cache (make-hash-table :test 'equal)
	"Cache for evaluated template variables to prevent re-evaluation.")
(defvar scriba--debug-call-count 0
	"Counter for debug calls.")

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

(defconst scriba--prop-note-template-prefix "SCRIBA_NOTE_TEMPLATE_" "Prefix for note template properties in book-config.org.")
(defconst scriba--prop-note-template-default "SCRIBA_NOTE_TEMPLATE_DEFAULT" "Property for the default/fallback note template.")
(defconst scriba--prop-variable-prefix "SCRIBA_VAR_" "Prefix for custom variable properties in book-config.org.")
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
		 "  :PROPERTIES:"
		 "  :SCRIBA_CONTENT_DIR: Content"
		 "  :SCRIBA_NOTES_DIR: Notes"
		 "  :SCRIBA_INDICES_DIR: Indices"
		 "  :SCRIBA_EXTRA_DIRS: Worldbuilding, Images"
		 "  :"
		 "  :NOTE_CATEGORIES: Characters, Locations, Plot Points, Research"
		 "  :SCRIBA_CONTENT_TYPES: Chapter, Scene, Part"
		 "  :"
		 "  COMMENT: --- Custom User-Defined Variables (Lisp code to be evaluated) ---"
		 "  COMMENT: Define new template variables here. The value is a Lisp expression."
		 "  :SCRIBA_VAR_POV: (completing-read \"Point of View: \" '(\"1st\" \"Some_Name\" \"Omniscient\"))"
		 "  :SCRIBA_VAR_SETTING: (read-string \"Setting: \")"
		 "  :"
		 "  COMMENT: --- Custom Content Templates (use {{...}} for variables) ---"
		 "  COMMENT: Available built-in variables: {{title}}, {{number}}, {{date}}"
		 "  COMMENT: Custom variables (e.g., {{pov}}) are defined above."
		 "  :SCRIBA_TEMPLATE_CHAPTER: #+TITLE: Chapter {{number}}: {{title}}\\n#+AUTHOR: %s\\n\\n* {{title}}\\n"
		 "  :SCRIBA_TEMPLATE_SCENE: #+TITLE: {{title}}\\n#+SETUPFILE: ../../scriba-style.org\\n#+DATE: {{date}}\\n\\n- POV:: {{pov}}\\n- Setting:: {{setting}}\\n- Time:: \\n\\n* Action\\n"
		 "  :SCRIBA_TEMPLATE_DEFAULT: #+TITLE: {{title}}\\n\\n* Overview\\n"
		 "  :"
		 "  COMMENT: --- Custom Note Templates ---"
		 "  :SCRIBA_NOTE_TEMPLATE_CHARACTERS: #+TITLE: {{title}} (Character)\\n#+CATEGORY: Characters\\n\\n* Physical Description\\n\\n* Personality\\n\\n* Backstory\\n\\n* Appearances in Content\\n"
		 "  :SCRIBA_NOTE_TEMPLATE_DEFAULT: #+TITLE: {{title}} ({{category}})\\n#+CATEGORY: {{category}}\\n\\n* Details\\n\\n* Appearances in Content\\n"
		 "  :END:"
		 ""
		 "** About This File"
		 "   This file stores project-specific settings for Scriba."
		 ""
		 "*** User-Defined Template Variables"
		 "    - You can create your own template variables like `{{pov}}` by defining them in the"
		 "      properties drawer above with the prefix `:SCRIBA_VAR_` (e.g., `:SCRIBA_VAR_POV:`)."
		 "    - The value of the property must be a valid Emacs Lisp s-expression (code)."
		 "    - When a template containing your variable is used, Scriba will execute this code"
		 "      and substitute the result. This allows for interactive prompts."
		 ""
		 "*** Structure & Built-in Templates"
		 "    - Edit `SCRIBA_CONTENT_TYPES` and `NOTE_CATEGORIES` to customize your project menus."
		 "    - Edit the `:SCRIBA_TEMPLATE_...` and `:SCRIBA_NOTE_TEMPLATE_...` properties to change"
		 "      the default content for new files."
		 "    - Use `\\n` for newlines within template strings."
		 ""
		 "    Run `M-x scriba-sync-folder-structure` after changing directory or category names."
		 ) "\n")
	"Template string for the book configuration file.
Placeholders:
%s (book title)
%s (author)
%s (author, for the default chapter template)"
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
(defun scriba--ensure-directory-exists (dir-path)
	"Create directory if it doesn't exist, return t if created, nil otherwise."
	(unless (file-directory-p dir-path)
		(make-directory dir-path t)
		t)) ; Return t to indicate a directory was created.

(defun scriba--format-template-debug (template-string &rest args)
	"Debug version with extensive logging."
	(let ((call-id (cl-incf scriba--debug-call-count)))
		(message "DEBUG [%d]: === STARTING TEMPLATE PROCESSING ===" call-id)
		(message "DEBUG [%d]: Template: %S" call-id template-string)
		(message "DEBUG [%d]: Args: %S" call-id args)

		;; Check if we're already in a recursive call
		(when (> scriba--template-processing-depth 0)
			(message "DEBUG [%d]: WARNING - Already in template processing (depth: %d)"
							 call-id scriba--template-processing-depth))

		(when (> scriba--template-processing-depth 3)
			(error "DEBUG [%d]: STOPPING - Template processing depth is %d"
						 call-id scriba--template-processing-depth))

		(let ((scriba--template-processing-depth (1+ scriba--template-processing-depth))
					(content template-string)
					(data (append args nil)))

			(message "DEBUG [%d]: Processing depth now: %d" call-id scriba--template-processing-depth)

			;; Add built-in values
			(message "DEBUG [%d]: Adding built-in values..." call-id)
			(plist-put data :date (format-time-string "<%Y-%m-%d %a>"))

			;; Number logic
			(when (and (eq (plist-get data :context) 'content) (plist-get data :type))
				(message "DEBUG [%d]: Adding number for type: %s" call-id (plist-get data :type))
				(let* ((root (scriba--book-root-folder))
							 (safe-type-name (scriba--sanitize-string (plist-get data :type)))
							 (content-base-dir (scriba--get-project-path 'content root))
							 (type-subdir (expand-file-name safe-type-name content-base-dir)))
					(when (file-exists-p type-subdir)
						(plist-put data :number (number-to-string (1+ (length (directory-files type-subdir t "\\.org$"))))))))

			;; Find all variables
			(message "DEBUG [%d]: Looking for variables in: %S" call-id content)
			(let ((variables-found '())
						(temp-content content)
						(safety-counter 0))

				(while (and (string-match "{{\\([a-zA-Z0-9_-]+\\)}}" temp-content)
										(< safety-counter 20)) ; Safety limit
					(let ((var-name (match-string 1 temp-content)))
						(message "DEBUG [%d]: Found variable: %s" call-id var-name)
						(unless (member var-name variables-found)
							(push var-name variables-found))
						(setq temp-content (substring temp-content (match-end 0)))
						(cl-incf safety-counter)))

				(when (>= safety-counter 20)
					(error "DEBUG [%d]: Too many variables found - possible infinite template" call-id))

				(message "DEBUG [%d]: All variables found: %S" call-id variables-found)

				;; Process each variable
				(dolist (var-name variables-found)
					(message "DEBUG [%d]: Processing variable: %s" call-id var-name)

					(let* ((var-name-key (intern (concat ":" var-name)))
								 (var-value (plist-get data var-name-key)))

						(message "DEBUG [%d]: Pre-existing value for %s: %S" call-id var-name var-value)

						;; If no pre-existing value, try to get from config
						(unless var-value
							(let* ((prop-name (concat scriba--prop-variable-prefix (upcase var-name)))
										 (lisp-code (scriba--get-raw-config-property prop-name)))

								(message "DEBUG [%d]: Property name: %s" call-id prop-name)
								(message "DEBUG [%d]: Lisp code: %S" call-id lisp-code)

								(when lisp-code
									(message "DEBUG [%d]: About to evaluate: %S" call-id lisp-code)
									(condition-case err
											(progn
												(setq var-value (eval (read lisp-code)))
												(message "DEBUG [%d]: Evaluation result: %S" call-id var-value))
										(error
										 (message "DEBUG [%d]: ERROR evaluating %s: %S" call-id var-name err)
										 (error "Error evaluating custom variable '{{%s}}': %s" var-name err))))))

						;; Replace in content
						(when var-value
							(let ((old-content content)
										(replacement (format "%s" var-value)))
								(message "DEBUG [%d]: Replacing {{%s}} with: %S" call-id var-name replacement)
								(setq content (replace-regexp-in-string
															 (regexp-quote (concat "{{" var-name "}}"))
															 replacement
															 content t t))
								(when (string= old-content content)
									(message "DEBUG [%d]: WARNING - No replacement occurred for %s" call-id var-name))))))

				(message "DEBUG [%d]: Final content: %S" call-id content)
				(message "DEBUG [%d]: === ENDING TEMPLATE PROCESSING ===" call-id)
				content))))

;; Reset function
(defun scriba--reset-debug ()
	"Reset debug counters."
	(interactive)
	(setq scriba--debug-call-count 0
				scriba--template-processing-depth 0)
	(message "Debug state reset"))

(defun scriba--format-template (template-string &rest args)
	"A powerful template engine for Scriba.
Replaces built-in variables like {{title}} and user-defined variables
from the config file. ARGS is a plist of extra data, e.g., '(:type 'chapter' :name 'foo')."
	;; Prevent infinite recursion
	(when (> scriba--template-processing-depth 10)
		(error "Template processing depth exceeded (>10) - possible infinite loop detected"))

	(let ((scriba--template-processing-depth (1+ scriba--template-processing-depth))
				(content template-string)
				(data (append args nil)) ; Make a mutable copy of the arguments plist
				(processed-vars (make-hash-table :test 'equal))) ; Track variables processed in this call

		;; --- Step 1: Add built-in dynamic values to the data plist ---
		(plist-put data :date (format-time-string "<%Y-%m-%d %a>"))

		;; Add chapter/content number if applicable
		(when (and (eq (plist-get data :context) 'content) (plist-get data :type))
			(let* ((root (scriba--book-root-folder))
						 (safe-type-name (scriba--sanitize-string (plist-get data :type)))
						 ;; Fix: Use proper path construction instead of concat
						 (content-base-dir (scriba--get-project-path 'content root))
						 (type-subdir (expand-file-name safe-type-name content-base-dir)))
				(when (file-exists-p type-subdir)
					(plist-put data :number (number-to-string (1+ (length (directory-files type-subdir t "\\.org$"))))))))

		;; --- Step 2: Process template variables using string replacement ---
		;; Collect all variables first to avoid infinite loops
		(let ((variables-to-process '()))
			(let ((temp-content content))
				(while (string-match "{{\\([a-zA-Z0-9_-]+\\)}}" temp-content)
					(let ((var-name (match-string 1 temp-content)))
						(unless (member var-name variables-to-process)
							(push var-name variables-to-process))
						(setq temp-content (substring temp-content (match-end 0))))))

			;; Now process each unique variable once
			(dolist (var-name-raw variables-to-process)
				(let* ((var-name-key (intern (concat ":" var-name-raw)))
							 (cache-key (concat "var:" var-name-raw))
							 (var-value nil))

					;; Skip if we've already processed this variable in this call
					(unless (gethash var-name-raw processed-vars)
						(puthash var-name-raw t processed-vars)

						;; A. Check if the value was already provided or calculated
						(setq var-value (plist-get data var-name-key))

						;; B. If not, check cache first
						(unless var-value
							(setq var-value (gethash cache-key scriba--template-variable-cache)))

						;; C. If still not found, look for a user-defined variable in the config
						(unless var-value
							(let* ((prop-name (concat scriba--prop-variable-prefix (upcase var-name-raw)))
										 (lisp-code (scriba--get-raw-config-property prop-name)))
								(when lisp-code
									(condition-case err
											(progn
												;; Safely evaluate the user's Lisp code
												(setq var-value (eval (read lisp-code)))
												;; Cache the result
												(puthash cache-key var-value scriba--template-variable-cache))
										(error (error "Error evaluating custom variable '{{%s}}': %s" var-name-raw err))))))

						;; D. Replace ALL occurrences of this variable in the content string
						(if var-value
								(let ((replacement-string (format "%s" var-value)))
									(while (string-match (regexp-quote (concat "{{" var-name-raw "}}")) content)
										(setq content (replace-match replacement-string t t content))))
							;; If no value found, show warning but leave placeholder
							(message "Warning: No value found for template variable '{{%s}}'" var-name-raw))))))

		;; Return the processed content
		content))

(defun scriba--clear-template-cache ()
	"Clear the template variable cache. Call this when config changes."
	(interactive)
	(clrhash scriba--template-variable-cache)
	(message "Template variable cache cleared"))

;; Helper function to reset processing state if needed
(defun scriba--reset-template-processing ()
	"Reset template processing state. Use if you suspect a corrupted state."
	(interactive)
	(setq scriba--template-processing-depth 0)
	(scriba--clear-template-cache)
	(message "Template processing state reset"))

(defun scriba--get-note-template-for-category (category)
	"Retrieve the template string for a given note CATEGORY from book-config.org."
	(let* ((safe-category-name (upcase (scriba--sanitize-string category)))
				 (type-prop-name (concat scriba--prop-note-template-prefix safe-category-name))
				 (specific-template (scriba--get-raw-config-property type-prop-name))
				 (default-template (scriba--get-raw-config-property scriba--prop-note-template-default)))
		;; Process escape sequences in templates
		(when specific-template
			(setq specific-template (replace-regexp-in-string "\\\\n" "\n" specific-template)))
		(when default-template
			(setq default-template (replace-regexp-in-string "\\\\n" "\n" default-template)))

		;; Return the specific template, fallback to default, or a basic template
		(or specific-template
				default-template
				"#+TITLE: {{title}} ({{category}})\n#+CATEGORY: {{category}}\n\n* Details\n\n* Appearances in Content\n")))


(defun scriba--get-note-template-for-category (category)
	"Retrieve the template string for a given note CATEGORY from book-config.org."
	(let* ((safe-category-name (upcase (scriba--sanitize-string category)))
				 (type-prop-name (concat scriba--prop-note-template-prefix safe-category-name))
				 (specific-template (scriba--get-raw-config-property type-prop-name))
				 (default-template (scriba--get-raw-config-property scriba--prop-note-template-default)))
		(when specific-template
			(setq specific-template (replace-regexp-in-string "\\\\n" "\n" specific-template)))
		(when default-template
			(setq default-template (replace-regexp-in-string "\\\\n" "\n" default-template)))
		(or specific-template
				default-template
				"#+TITLE: {{title}} ({{category}})\n#+CATEGORY: {{category}}\n\n* Details\n\n* Appearances in Content\n")))

(defun scriba--get-raw-config-property (property-name)
	"The definitive raw property reader from book-config.org.
This is the new single source of truth for all config properties.
It finds the line ':PROPERTY_NAME: value' and returns 'value' as a raw string."
	(let* ((root (scriba--book-root-folder))
				 (config-file (concat root scriba--config-filename)))
		(when (file-exists-p config-file)
			(with-temp-buffer
				(insert-file-contents-literally config-file)
				(goto-char (point-min))
				;; Find the property key and grab everything after it on the same line.
				(when (re-search-forward (format "^[ \t]*:%s:[ \t]*\\(.*\\)$" property-name) nil t)
					(string-trim (match-string-no-properties 1)))))))

(defun scriba--get-list-config-property (property-name)
	"Get a config property and parse it as a comma-separated list."
	(let ((raw-value (scriba--get-raw-config-property property-name)))
		(when (and raw-value (not (string-empty-p raw-value)))
			(split-string raw-value "[ \t]*,[ \t]*" t))))

(defun scriba--get-template-for-type (type)
	"Retrieve the template string for a given content TYPE from book-config.org."
	(let* ((type-prop-name (concat scriba--prop-template-prefix (upcase type)))
				 (specific-template (scriba--get-raw-config-property type-prop-name))
				 (default-template (scriba--get-raw-config-property scriba--prop-template-default)))
		;; Process escape sequences in templates
		(when specific-template
			(setq specific-template (replace-regexp-in-string "\\\\n" "\n" specific-template)))
		(when default-template
			(setq default-template (replace-regexp-in-string "\\\\n" "\n" default-template)))

		;; Return the specific template, fallback to default, or a basic template
		(or specific-template
				default-template
				"#+TITLE: {{title}}\n\n* Overview\n\n")))

(defun scriba--sanitize-string (str)
	"Sanitize STR for use in file names and Org links by converting it to CamelCase."
	(let* ((trimmed (string-trim str))
				 (spaced (replace-regexp-in-string "[^[:alnum:]]+" " " trimmed))
				 (words (split-string spaced " +" t)))
		(mapconcat #'capitalize words "")))

(defun scriba--book-root-folder (&optional start-dir)
	"Return the root folder of the Scriba project."
	(let ((dir (or start-dir (when buffer-file-name (file-name-directory buffer-file-name)) default-directory)))
		(unless dir (error "Cannot determine current directory"))
		(let ((current-dir (expand-file-name dir)))
			(while (and current-dir (not (file-exists-p (concat current-dir scriba--folder-separator scriba--config-filename))))
				(let ((parent (file-name-directory (directory-file-name current-dir))))
					(if (equal parent current-dir)
							(setq current-dir nil)
						(setq current-dir parent))))
			(unless current-dir (error "Not in a Scriba project. Config file '%s' not found." scriba--config-filename))
			(file-name-as-directory current-dir))))

(defun scriba--get-property-value (property-name file-path)
	"Get the value of a keyword or property from any Org file.
PRIORITY: #+KEYWORD > :PROPERTIES: drawer.
This is for general use, not for book-config.org."
	(when (and file-path (file-readable-p file-path))
		(with-temp-buffer
			(insert-file-contents-literally file-path)
			(org-mode)
			(let (value)
				(goto-char (point-min))
				(when (re-search-forward
							 (format "^#\\+%s:[ \t]*\\(.*\\)" (upcase property-name))
							 nil t)
					(setq value (string-trim (match-string 1))))
				(unless value
					(goto-char (point-min))
					(when (re-search-forward org-property-drawer-re nil t)
						(setq value (org-entry-get (point) property-name t))))
				value))))

(defun scriba--get-customizable-folder-name (prop-key default-name)
	"Get folder name from config or use DEFAULT-NAME."
	(or (scriba--get-raw-config-property prop-key) default-name))

(defun scriba--get-project-path (type &optional root-dir)
	"Return the full path to a standard project directory TYPE."
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
	"Add a link to the specified INDEX-FILE."
	(let ((level (or heading-level 2)))
		(with-current-buffer (find-file-noselect index-file)
			(goto-char (point-max))
			(unless (bolp) (insert "\n"))
			(insert (make-string level ?*) " "
							(format "[[file:%s][%s]]\n" link-target link-description))
			(save-buffer)
			(unless (get-file-buffer index-file)
				(kill-buffer (current-buffer))))))

(defun scriba--string-to-file (str filename)
	"Write STR to FILENAME, creating parent directories if needed."
	(make-directory (file-name-directory filename) t)
	(with-temp-buffer
		(insert str)
		(write-file filename nil)))

(defun scriba--get-all-content-files ()
	"Return a list of all content files in the project, searching recursively."
	(let ((content-dir (scriba--get-project-path 'content)))
		(when (file-directory-p content-dir)
			(directory-files-recursively content-dir (concat scriba--file-ending "$")))))

(defun scriba--get-all-note-files ()
	"Return a list of all note files across all categories."
	(let* ((notes-root-dir (scriba--get-project-path 'notes))
				 (all-note-files '()))
		(when (file-directory-p notes-root-dir)
			(dolist (category-dir (directory-files notes-root-dir t "^[^.]"))
				(when (file-directory-p category-dir)
					(dolist (note-file (directory-files category-dir t (concat "^[^.].*\\" scriba--file-ending "$")))
						(push note-file all-note-files))))
			all-note-files)))

(defun scriba--get-note-categories ()
	"Return a list of note categories from the centralized config reader."
	(scriba--get-list-config-property scriba--note-categories-property))

(defun scriba--get-content-types ()
	"Return a list of content types from the centralized config reader."
	(scriba--get-list-config-property scriba--prop-content-types))

;;;; Core Functionality
(defun scriba-sync-folder-structure (&optional clean-unused)
	"Synchronize the project folder structure with book-config.org settings.
This creates any missing directories defined in the config."
	(interactive "P")
	(let* ((root (scriba--book-root-folder))
				 (created-dirs '()))
		(unless root (error "Not in a Scriba project"))
		(message "Syncing directory structure with config...")
		(let* (;; Get config values, providing defaults if they are nil.
					 (content-dir-name (or (scriba--get-raw-config-property scriba--prop-content-dir) scriba--default-content-folder))
					 (notes-dir-name (or (scriba--get-raw-config-property scriba--prop-notes-dir) scriba--default-notes-folder))
					 (indices-dir-name (or (scriba--get-raw-config-property scriba--prop-indices-dir) scriba--default-indices-folder))
					 (content-types (scriba--get-content-types))
					 (note-categories (scriba--get-note-categories))
					 (extra-dirs (scriba--get-list-config-property scriba--prop-extra-dirs))
					 ;; Create the absolute paths for the main directories.
					 (content-path (expand-file-name content-dir-name root))
					 (notes-path (expand-file-name notes-dir-name root))
					 (indices-path (expand-file-name indices-dir-name root)))
			;; --- Create main directories ---
			(dolist (path (list content-path notes-path indices-path))
				(when (scriba--ensure-directory-exists path)
					(push (file-relative-name path root) created-dirs)))
			;; --- Create content type subdirectories ---
			(dolist (type content-types)
				(when type ; Ensure we don't process a nil element
					(let ((path (expand-file-name (scriba--sanitize-string type) content-path)))
						(when (scriba--ensure-directory-exists path)
							(push (file-relative-name path root) created-dirs)))))
			;; --- Create note category subdirectories ---
			(dolist (category note-categories)
				(when category ; Ensure we don't process a nil element
					(let ((path (expand-file-name (scriba--sanitize-string category) notes-path)))
						(when (scriba--ensure-directory-exists path)
							(push (file-relative-name path root) created-dirs)))))
			;; --- Create extra directories (at the top level) ---
			(dolist (extra-dir extra-dirs)
				(when extra-dir ; Ensure we don't process a nil element
					(let ((path (expand-file-name (scriba--sanitize-string extra-dir) root)))
						(when (scriba--ensure-directory-exists path)
							(push (file-relative-name path root) created-dirs))))))
		(when clean-unused (message "Cleanup of unused directories is not yet implemented."))
		(if created-dirs
				(message "Scriba: Synced directories: %s" (string-join (nreverse created-dirs) ", "))
			(message "Scriba: All directories are up to date."))))

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
	"Create a new character profile using the 'Characters' note template.
This is a specialized wrapper around `scriba-new-note`."
	(interactive (list (read-string "New Character Name: ")))
	;; First, ensure 'Characters' is a valid category in the config file
	;; before attempting to create a note in it.
	(unless (member "Characters" (scriba--get-note-categories))
		(error "Note category 'Characters' not found in %s. Please add it to NOTE_CATEGORIES and run sync." scriba--config-filename))

	;; The entire logic is now just a call to the main note creation function
	;; with the category hardcoded to "Characters".
	(scriba-new-note "Characters" character-name))

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
	"Create a new Scriba project, including all configured subdirectories."
	(interactive
	 (list (read-string "Book Name: ")
				 (read-directory-name "Parent Directory for Book Project: ")))
	(let* ((safe-book-name (file-name-as-directory (scriba--sanitize-string book-name)))
				 (project-root (concat (file-name-as-directory book-path) safe-book-name)))

		(when (file-exists-p project-root)
			(error "Project directory '%s' already exists." project-root))

		;; 1. Create the project root directory.
		(make-directory project-root t)

		;; 2. Set the context to be inside the new project root for all subsequent operations.
		(let ((default-directory project-root))
			(let* ((config-file-path (expand-file-name scriba--config-filename))
						 (main-file-path (expand-file-name scriba--main-filename)))

				;; 3. Create the config file.
				(scriba--string-to-file
				 (format scriba-config-file-template
								 book-name scriba-author scriba-author)
				 config-file-path)

				;; 4. Sync the directory structure based on the config we just wrote.
				;;    This function is now responsible for ALL directory creation.
				(message "Syncing project structure based on config...")
				(scriba-sync-folder-structure)

				;; 5. Now that directories are guaranteed to exist, create the initial files.
				(let* ((indices-dir-name (or (scriba--get-raw-config-property scriba--prop-indices-dir) scriba--default-indices-folder))
							 (extra-dirs (scriba--get-list-config-property scriba--prop-extra-dirs)))

					;; Create main.org
					(let ((main-file-content (format scriba-main-file-template
																					 book-name scriba-author indices-dir-name
																					 scriba--sections-index-filename scriba--config-filename)))
						(when extra-dirs
							(let ((extra-links (mapconcat
																	(lambda (dir) (format "  ** [[file:%s/][%s]]" (scriba--sanitize-string dir) dir))
																	extra-dirs "\n")))
								(setq main-file-content
											(replace-regexp-in-string
											 "^\\* Other Project Areas\n  ;; Links to extra directories.*$"
											 (format "* Other Project Areas\n%s" extra-links)
											 main-file-content))))
						(scriba--string-to-file main-file-content main-file-path))

					;; Create sections-index.org
					(scriba--string-to-file
					 (format "#+TITLE: Content Sections Index for %s\n\n" book-name)
					 (expand-file-name scriba--sections-index-filename (expand-file-name indices-dir-name)))

					;; Create scriba-style.org
					(scriba--string-to-file
					 scriba-style-file-contents
					 (expand-file-name "scriba-style.org")))

				;; 6. Initialize Git repository.
				(when scriba-git-init-on-new-book
					(call-process "git" nil 0 0 "init")
					(scriba--string-to-file scriba-default-gitignore-contents scriba--gitignore-filename)
					(call-process "git" nil 0 0 "add" ".")
					(call-process "git" nil 0 0 "commit" "-m" "Initial project setup by Scriba.")
					(message "Git repository initialized."))

				(message "Scriba project '%s' created successfully at %s" book-name project-root)
				(find-file main-file-path)))))

(defun scriba--get-note-categories ()
	"Return a list of note categories from the centralized config reader."
	(scriba--get-list-config-property scriba--note-categories-property))

(defun scriba--get-content-types ()
	"Return a list of content types from the centralized config reader."
	(scriba--get-list-config-property scriba--prop-content-types))

;;;###autoload
(defun scriba-new-content (type name)
	"Create a new content file of a specific TYPE using the dynamic template system."
	(interactive
	 (let ((types (scriba--get-content-types)))
		 (unless types (error "No content types defined in %s." scriba--config-filename))
		 (list (completing-read "Content Type: " types nil t (car types))
					 (read-string "Content Name/Title: "))))
	(let* ((root (scriba--book-root-folder))
				 (safe-type-name (scriba--sanitize-string type))
				 (safe-file-name (scriba--sanitize-string name))
				 ;; Use expand-file-name for proper path construction
				 (content-base-dir (scriba--get-project-path 'content root))
				 (type-subdir (expand-file-name safe-type-name content-base-dir))
				 (file-path (expand-file-name (concat safe-file-name scriba--file-ending) type-subdir))
				 ;; Get the template for this content type
				 (raw-template (scriba--get-template-for-type type))
				 ;; Process the template with proper context
				 (final-content (scriba--format-template raw-template
																								 :context 'content
																								 :type type
																								 :title name))
				 ;; Extract the final title from the processed content
				 (final-title (with-temp-buffer
												(insert final-content)
												(goto-char (point-min))
												(if (re-search-forward "^#\\+TITLE:[ \t]*\\(.*\\)" nil t)
														(match-string 1)
													name))))
		;; Ensure the type subdirectory exists
		(make-directory type-subdir t)

		;; Check if file already exists
		(when (file-exists-p file-path)
			(error "%s file '%s' already exists." type name))

		;; Create the file with the processed template content
		(scriba--string-to-file final-content file-path)

		;; Add to index if indices are configured
		(let* ((indices-base-dir (scriba--get-project-path 'indices root))
					 (index-path (expand-file-name scriba--sections-index-filename indices-base-dir))
					 (link-target (file-relative-name file-path (file-name-directory index-path))))
			(when (and indices-base-dir (file-exists-p (file-name-directory index-path)))
				(scriba--add-link-to-index index-path link-target final-title)))

		(message "%s '%s' created." type name)
		(find-file file-path)))

;;;###autoload
(defun scriba-new-note (category note-name)
	"Create a new note using the dynamic template system."
	(interactive
	 (let ((categories (scriba--get-note-categories)))
		 (unless categories (error "No note categories defined in %s." scriba--config-filename))
		 (list (completing-read "Note Category: " categories nil t)
					 (read-string "New Note Name: "))))
	(let* ((root (scriba--book-root-folder))
				 (safe-category-name (scriba--sanitize-string category))
				 (safe-note-name (scriba--sanitize-string note-name))
				 ;; Use expand-file-name for proper path construction
				 (notes-base-dir (scriba--get-project-path 'notes root))
				 (category-dir (expand-file-name safe-category-name notes-base-dir))
				 (note-path (expand-file-name (concat safe-note-name scriba--file-ending) category-dir))
				 ;; Get the template for this note category
				 (raw-template (scriba--get-note-template-for-category category))
				 ;; Process the template with proper context
				 (final-content (scriba--format-template raw-template
																								 :context 'note
																								 :category category
																								 :title note-name)))
		;; Ensure the category directory exists
		(make-directory category-dir t)

		;; Check if note already exists
		(when (file-exists-p note-path)
			(error "Note file '%s' exists." note-path))

		;; Create the note file
		(scriba--string-to-file final-content note-path)

		;; Add to category index
		(let* ((indices-base-dir (scriba--get-project-path 'indices root))
					 (category-index-filename (concat scriba--note-category-index-prefix safe-category-name scriba--file-ending))
					 (category-index-path (expand-file-name category-index-filename indices-base-dir))
					 (link-target (file-relative-name note-path (file-name-directory category-index-path))))
			;; Ensure indices directory exists
			(when indices-base-dir
				(make-directory indices-base-dir t)

				;; Create category index if it doesn't exist
				(unless (file-exists-p category-index-path)
					(scriba--string-to-file (format "#+TITLE: %s Notes Index\n\n" category) category-index-path)
					(message "Created new index file: %s" category-index-path))

				;; Add link to the category index
				(scriba--add-link-to-index category-index-path link-target note-name)))

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
				 (backup-dir (read-directory-name "Save backup in directory: "
																					(file-name-directory (directory-file-name root))))
				 (zip-file (expand-file-name (format "%s-backup-%s.zip" project-name timestamp) backup-dir))
				 (default-directory root)) ; This sets the working directory for call-process

		(unless (executable-find "zip")
			(error "The 'zip' command is not in your system's PATH"))

		;; Ensure backup directory exists
		(unless (file-directory-p backup-dir)
			(error "Backup directory does not exist: %s" backup-dir))

		(message "Backing up project to %s ..." zip-file)

		;; Use call-process with proper working directory
		(let ((exit-code (call-process "zip" nil "*scriba-backup*" nil
																	 "-r" zip-file "."
																	 "-x" ".git/*" "*.zip" "*.elc" "*/.DS_Store"
																	 "*/auto-save-list/*" "*~" "*.bak")))
			(if (= exit-code 0)
					(message "Project backup created successfully at %s" zip-file)
				(progn
					(pop-to-buffer "*scriba-backup*")
					(error "Backup failed with exit code %d. Check *scriba-backup* buffer for details" exit-code))))))

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

;;;; Relationship Graph helpers and function
(defun scriba--write-dot-file (dot-file nodes edges graph-type &optional focus-node)
	"Write DOT file with clickable nodes and edges."
	(with-temp-file dot-file
		(insert "digraph relationship_graph {\n")
		(insert "  rankdir=LR;\n")
		(insert "  node [shape=box, style=filled, fontname=\"Arial\"];\n")
		(insert "  edge [fontname=\"Arial\", fontsize=10];\n")
		(insert "  overlap=false;\n")
		(insert "  splines=true;\n\n")

		;; Write nodes with clickable URLs
		(maphash (lambda (node-id node-info)
							 (let* ((label (plist-get node-info :title))
											(file-path (plist-get node-info :file))
											;; Create emacsclient URL for opening files
											(url (format "emacsclient://open?file=%s"
																	 (url-hexify-string file-path)))
											(color (scriba--get-node-color node-info focus-node node-id))
											(shape (scriba--get-node-shape node-info)))
								 (insert (format "  \"%s\" [label=\"%s\", URL=\"%s\", fillcolor=\"%s\", shape=%s, target=\"_parent\"];\n"
																 node-id
																 (scriba--escape-dot-string label)
																 url
																 color
																 shape))))
						 nodes)

		(insert "\n")

		;; Write edges (also clickable to show relationship details)
		(dolist (edge edges)
			(let* ((from (nth 0 edge))
						 (to (nth 1 edge))
						 (label (nth 2 edge))
						 ;; URL for edge could show relationship info or navigate
						 (edge-url (format "javascript:showRelationship('%s','%s','%s')"
															 from to (or label ""))))
				(insert (format "  \"%s\" -> \"%s\"" from to))
				(when (and label (not (string-empty-p label)))
					(insert (format " [label=\"%s\", URL=\"%s\", target=\"_parent\"]"
													(scriba--escape-dot-string label) edge-url)))
				(insert ";\n")))

		(insert "}\n")))

(defun scriba--get-node-color (node-info focus-node node-id)
	"Get color for node based on type and focus status."
	(cond
	 ((and focus-node (string= node-id focus-node)) "lightcoral")
	 ((string-match-p "/notes/" (plist-get node-info :file)) "lightblue")
	 ((string-match-p "/content/" (plist-get node-info :file)) "lightgreen")
	 (t "lightyellow")))

(defun scriba--get-node-shape (node-info)
	"Get shape for node based on file type."
	(let ((file (plist-get node-info :file)))
		(cond
		 ((string-match-p "/notes/" file) "ellipse")
		 ((string-match-p "/content/" file) "box")
		 (t "diamond"))))

(defun scriba--escape-dot-string (str)
	"Escape special characters in string for DOT format."
	(when str
		(replace-regexp-in-string
		 "\""
		 "\\\\\""
		 (replace-regexp-in-string "\\\\" "\\\\\\\\" str))))

(defun scriba--compile-svg (dot-file svg-file)
	"Compile DOT file to clickable SVG."
	(let ((cmd (format "dot -Tsvg -o %s %s"
										 (shell-quote-argument svg-file)
										 (shell-quote-argument dot-file))))
		(message "Compiling graph: %s" cmd)
		(shell-command cmd)
		(unless (file-exists-p svg-file)
			(error "Failed to generate SVG file"))

		;; Post-process SVG to enhance clickability
		(scriba--enhance-svg-clickability svg-file)))

(defun scriba--enhance-svg-clickability (svg-file)
	"Post-process SVG to add JavaScript and improve clickability."
	(let ((content (with-temp-buffer
									 (insert-file-contents svg-file)
									 (buffer-string))))

		;; Add JavaScript for enhanced interaction
		(setq content
					(replace-regexp-in-string
					 "</svg>"
					 (concat
						"<script type=\"text/javascript\">\n"
						"<![CDATA[\n"
						"function showRelationship(from, to, label) {\n"
						"  alert('Relationship: ' + from + ' → ' + to + \n"
						"        (label ? '\\nLabel: ' + label : ''));\n"
						"}\n"
						"\n"
						"// Add hover effects\n"
						"document.addEventListener('DOMContentLoaded', function() {\n"
						"  var nodes = document.querySelectorAll('g.node');\n"
						"  nodes.forEach(function(node) {\n"
						"    node.addEventListener('mouseenter', function() {\n"
						"      this.style.opacity = '0.8';\n"
						"    });\n"
						"    node.addEventListener('mouseleave', function() {\n"
						"      this.style.opacity = '1.0';\n"
						"    });\n"
						"  });\n"
						"});\n"
						"]]>\n"
						"</script>\n"
						"</svg>")
					 content))

		;; Write enhanced SVG back
		(with-temp-file svg-file
			(insert content))))

;; Alternative: Create HTML wrapper for better browser integration
(defun scriba--create-html-wrapper (svg-file)
	"Create HTML wrapper for SVG with enhanced JavaScript functionality."
	(let* ((html-file (concat (file-name-sans-extension svg-file) ".html"))
				 (svg-content (with-temp-buffer
												(insert-file-contents svg-file)
												(buffer-string))))

		(with-temp-file html-file
			(insert "<!DOCTYPE html>\n")
			(insert "<html>\n<head>\n")
			(insert "<title>Scriba Relationship Graph</title>\n")
			(insert "<style>\n")
			(insert "body { margin: 0; padding: 20px; font-family: Arial, sans-serif; }\n")
			(insert "svg { border: 1px solid #ccc; }\n")
			(insert ".tooltip { position: absolute; background: rgba(0,0,0,0.8); color: white; ")
			(insert "padding: 5px; border-radius: 3px; pointer-events: none; }\n")
			(insert "</style>\n")
			(insert "<script>\n")
			(insert "function openInEmacs(filepath) {\n")
			(insert "  // For local development, try to open in Emacs\n")
			(insert "  if (window.location.protocol === 'file:') {\n")
			(insert "    var xhr = new XMLHttpRequest();\n")
			(insert "    xhr.open('GET', 'http://localhost:9999/open?file=' + encodeURIComponent(filepath));\n")
			(insert "    xhr.send();\n")
			(insert "  } else {\n")
			(insert "    alert('File: ' + filepath);\n")
			(insert "  }\n")
			(insert "}\n")
			(insert "</script>\n")
			(insert "</head>\n<body>\n")
			(insert "<h1>Project Relationship Graph</h1>\n")
			(insert svg-content)
			(insert "</body>\n</html>")

			html-file)))

;; Enhanced main function
(defun scriba-generate-relationship-graph (&optional graph-type focus-node)
	"Generate a clickable SVG relationship graph of project files using Graphviz.
GRAPH-TYPE can be 'full, 'content-only, 'notes-only, or 'focused.
FOCUS-NODE specifies the central node for focused graphs.
The SVG is saved in the project root and opened in the default browser."
	(interactive
	 (list (intern (completing-read "Graph type: "
																	'("full" "content-only" "notes-only" "focused")
																	nil t "full"))
				 (when (string= "focused" (completing-read "Graph type: "
																									 '("full" "content-only" "notes-only" "focused")
																									 nil t "full"))
					 (completing-read "Focus on node: " (scriba--get-all-node-names)))))

	(unless (executable-find "dot")
		(error "Graphviz 'dot' command not found. Please install Graphviz"))

	(let* ((root (scriba--book-root-folder))
				 (graph-type (or graph-type 'full))
				 (timestamp (format-time-string "%Y%m%d-%H%M%S"))
				 (graph-name (if focus-node
												 (format "graph-%s-%s" focus-node timestamp)
											 (format "graph-%s-%s" graph-type timestamp)))
				 (dot-file-path (expand-file-name (concat graph-name ".dot") root))
				 (svg-file-path (expand-file-name (concat graph-name ".svg") root))
				 (html-file-path (expand-file-name (concat graph-name ".html") root))
				 (nodes (make-hash-table :test 'equal))
				 (edges '()))

		(message "Scanning project to build %s relationship graph..." graph-type)

		;; Collect all project files based on graph type
		(let ((all-files (scriba--get-files-by-type graph-type)))
			(scriba--extract-graph-data all-files nodes edges focus-node))

		;; Generate DOT file with clickable elements
		(scriba--write-dot-file dot-file-path nodes edges graph-type focus-node)

		;; Compile to SVG
		(scriba--compile-svg dot-file-path svg-file-path)

		;; Create HTML wrapper for better browser integration
		(scriba--create-html-wrapper svg-file-path)

		;; Open result (prefer HTML wrapper)
		(browse-url (format "file://%s" html-file-path))
		(message "Clickable graph generated: %s" html-file-path)))

(defun scriba--get-files-by-type (graph-type)
	"Get project files based on GRAPH-TYPE."
	(let ((root (scriba--book-root-folder)))
		(pcase graph-type
			('content-only
			 (directory-files-recursively (scriba--get-project-path 'content root) "\\.org$"))
			('notes-only
			 (scriba--get-all-note-files))
			('focused
			 (append (directory-files-recursively (scriba--get-project-path 'content root) "\\.org$")
							 (scriba--get-all-note-files)))
			(_ ; 'full or default
			 (append (directory-files-recursively (scriba--get-project-path 'content root) "\\.org$")
							 (scriba--get-all-note-files))))))

(defun scriba--extract-graph-data (files nodes edges &optional focus-node)
	"Extract nodes and edges from FILES, optionally focusing on FOCUS-NODE."
	(let ((focus-connections (when focus-node (make-hash-table :test 'equal))))

		(dolist (file files)
			(let* ((node-id (scriba--get-node-id file))
						 (node-info (scriba--get-node-info file)))

				;; Store node information
				(puthash node-id node-info nodes)

				;; Extract links from this file
				(with-temp-buffer
					(insert-file-contents file)
					(org-mode)
					(goto-char (point-min))

					;; Find all org links
					(while (re-search-forward "\\[\\[\\([^]]+\\)\\]\\[?\\([^]]*\\)\\]?\\]" nil t)
						(let* ((link-target (match-string 1))
									 (link-text (match-string 2))
									 (target-file (scriba--resolve-link-target link-target file))
									 (target-node-id (when target-file (scriba--get-node-id target-file))))

							(when (and target-node-id (gethash target-node-id nodes))
								(let ((edge (list node-id target-node-id link-text)))
									(push edge edges)

									;; Track focus connections
									(when focus-node
										(if (string= node-id focus-node)
												(puthash target-node-id t focus-connections))
										(if (string= target-node-id focus-node)
												(puthash node-id t focus-connections))))))))))

		;; Filter nodes for focused graph
		(when focus-node
			(let ((filtered-nodes (make-hash-table :test 'equal)))
				;; Keep focus node
				(when (gethash focus-node nodes)
					(puthash focus-node (gethash focus-node nodes) filtered-nodes))
				;; Keep connected nodes
				(maphash (lambda (node-id _)
									 (let ((node-info (gethash node-id nodes)))
										 (when node-info
											 (puthash node-id node-info filtered-nodes))))
								 focus-connections)
				;; Update nodes hash table
				(clrhash nodes)
				(maphash (lambda (k v) (puthash k v nodes)) filtered-nodes)

				;; Filter edges
				(setq edges (cl-remove-if-not
										 (lambda (edge)
											 (and (gethash (nth 0 edge) nodes)
														(gethash (nth 1 edge) nodes)))
										 edges))))))

(defun scriba--get-node-id (file)
	"Get a unique node ID for FILE."
	(let* ((relative-path (file-relative-name file (scriba--book-root-folder)))
				 (sanitized (replace-regexp-in-string "[^a-zA-Z0-9_]" "_" relative-path)))
		sanitized))

(defun scriba--get-node-info (file)
	"Extract node information from FILE."
	(let* ((title (scriba--extract-title file))
				 (category (scriba--get-file-category file))
				 (node-type (scriba--get-node-type file))
				 (url (format "file://%s" (expand-file-name file))))
		(list :title title
					:category category
					:type node-type
					:file file
					:url url)))

(defun scriba--extract-title (file)
	"Extract title from org FILE, fallback to filename."
	(with-temp-buffer
		(insert-file-contents file nil 0 1000) ; Read first 1000 chars for efficiency
		(if (re-search-forward "^#\\+TITLE:[ \t]*\\(.*\\)" nil t)
				(string-trim (match-string 1))
			(file-name-base file))))

(defun scriba--get-file-category (file)
	"Determine the category of FILE."
	(let ((dir-path (file-name-directory file))
				(root (scriba--book-root-folder)))
		(cond
		 ((string-match-p "/Content/" dir-path) "Content")
		 ((string-match-p "/Notes/" dir-path) "Notes")
		 ((string-match-p "/Worldbuilding/" dir-path) "Worldbuilding")
		 (t "Other"))))

(defun scriba--get-node-type (file)
	"Get the specific type of node (Chapter, Character, etc.)."
	(let ((dir-name (file-name-nondirectory
									 (directory-file-name
										(file-name-directory file)))))
		(if (member dir-name '("Content" "Notes" "Worldbuilding"))
				(scriba--get-file-category file)
			dir-name)))

(defun scriba--resolve-link-target (link-target current-file)
	"Resolve LINK-TARGET relative to CURRENT-FILE."
	(cond
	 ;; File link
	 ((string-prefix-p "file:" link-target)
		(let ((file-path (substring link-target 5)))
			(if (file-name-absolute-p file-path)
					file-path
				(expand-file-name file-path (file-name-directory current-file)))))
	 ;; Relative file link
	 ((string-suffix-p ".org" link-target)
		(expand-file-name link-target (file-name-directory current-file)))
	 ;; Other link types - return nil
	 (t nil)))

(defun scriba--write-dot-file (dot-path nodes edges graph-type focus-node)
	"Write DOT file to DOT-PATH with NODES and EDGES."
	(with-temp-buffer
		(insert (format "digraph Scriba_%s {\n" graph-type))
		(insert "  graph [rankdir=LR, splines=true, overlap=false, concentrate=true];\n")
		(insert "  node [style=\"rounded,filled\", fontname=\"Arial\"];\n")
		(insert "  edge [fontname=\"Arial\", fontsize=10];\n\n")

		;; Color scheme for different node types
		(let ((colors '(("Content" . "#E3F2FD")
										("Notes" . "#F3E5F5")
										("Worldbuilding" . "#E8F5E8")
										("Chapter" . "#FFE0B2")
										("Scene" . "#FFF3E0")
										("Characters" . "#FCE4EC")
										("Locations" . "#E0F2F1")
										("Other" . "#F5F5F5"))))

			;; Write nodes
			(maphash (lambda (node-id node-info)
								 (let* ((title (plist-get node-info :title))
												(node-type (plist-get node-info :type))
												(url (plist-get node-info :url))
												(color (or (cdr (assoc node-type colors)) "#F5F5F5"))
												(is-focus (and focus-node (string= node-id focus-node))))
									 (insert (format "  \"%s\" [label=\"%s\\n(%s)\", URL=\"%s\", fillcolor=\"%s\"%s];\n"
																	 node-id
																	 (scriba--escape-dot-string title)
																	 node-type
																	 url
																	 color
																	 (if is-focus ", penwidth=3, color=\"red\"" "")))))
							 nodes))

		(insert "\n")

		;; Write edges
		(dolist (edge (delete-dups edges))
			(let ((from (nth 0 edge))
						(to (nth 1 edge))
						(label (nth 2 edge)))
				(insert (format "  \"%s\" -> \"%s\"%s;\n"
												from to
												(if (and label (not (string-empty-p label)))
														(format " [label=\"%s\"]" (scriba--escape-dot-string label))
													"")))))

		(insert "}\n")
		(write-file dot-path)))

(defun scriba--escape-dot-string (str)
	"Escape string for DOT format."
	(replace-regexp-in-string "\"" "\\\\\""
														(replace-regexp-in-string "\n" "\\\\n" str)))

(defun scriba--compile-svg (dot-path svg-path)
	"Compile DOT file to SVG."
	(message "Compiling SVG from DOT file...")
	(let ((exit-code (call-process "dot" nil "*scriba-graph*" nil
																 "-Tsvg" dot-path "-o" svg-path)))
		(unless (= exit-code 0)
			(pop-to-buffer "*scriba-graph*")
			(error "Graphviz compilation failed with exit code %d" exit-code))))

(defun scriba--get-all-node-names ()
	"Get all node names for completion."
	(let ((files (scriba--get-files-by-type 'full)))
		(mapcar (lambda (file) (scriba--get-node-id file)) files)))

;;;###autoload
(defun scriba-generate-node-graph (node-name)
	"Generate a focused graph for a specific NODE-NAME."
	(interactive (list (completing-read "Focus on node: " (scriba--get-all-node-names))))
	(scriba-generate-relationship-graph 'focused node-name))

;;;###autoload
(defun scriba-generate-content-graph ()
	"Generate a graph showing only content relationships."
	(interactive)
	(scriba-generate-relationship-graph 'content-only))

;;;###autoload
(defun scriba-generate-notes-graph ()
	"Generate a graph showing only note relationships."
	(interactive)
	(scriba-generate-relationship-graph 'notes-only))


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
