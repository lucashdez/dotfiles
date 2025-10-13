;;; modus-custom-theme.el --- Elegant, highly legible theme with a black background  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  

;; Author:  <l.hernandez@PORTLHERNANDEZ>
;; Keywords: faces

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:



(unless (>= emacs-major-version 24)
  (error "The modus custom theme requires Emacs 24 or later!"))

(deftheme modus-custom "The modus color theme")

;; Monokai colors
(defcustom modus-theme-yellow "#E6DB74" "Primary colors - yellow" :type 'string :group 'monokai)
(defcustom modus-theme-orange "#FD971F" "Primary colors - orange" :type 'string :group 'monokai)
(defcustom modus-theme-red "#F92672" "Primary colors - red" :type 'string :group 'monokai)
(defcustom modus-theme-magenta "#FD5FF0" "Primary colors - magenta" :type 'string :group 'monokai)
(defcustom modus-theme-blue "#66D9EF" "Primary colors - blue" :type 'string :group 'monokai)
(defcustom modus-theme-green "#A6E22E" "Primary colors - green" :type 'string :group 'monokai)
(defcustom modus-theme-cyan "#A1EFE4" "Primary colors - cyan" :type 'string :group 'monokai)
(defcustom modus-theme-violet "#AE81FF" "Primary colors - violet" :type 'string :group 'monokai)

(let ((bg-main "#161616")
	  (fg-main "#a08563")
	  (bg-cursor-n "#40FF40")
	  (bg-cursor-i "#FF4040")
	  (bg-region "#5a5a5a")
	  (fg-region "#ffffff")
      (gutters    "#062329")
      (gutter-fg  "#062329")
      (gutters-active "#062329")
      (builtin      "#ffffff")
      (selection  "#0000ff")
      (text       "#d1b897")
      (comments   "#7d7d7d")
      (punctuation "#8cde94")
      (keywords "#cd950c")
      (variables "#c1d1e3")
      (functions "#de451f")
      (methods    "#c1d1e3")
      (strings    "#6b8e23")
	  (4-strings  "#50ff30")
	  (4-types    "#edb211")
      (constants "#6eb535")
      (macros "#dab98f")
      (numbers "#7ad0c6")
	  (4-numbers "#50ff30")
      (white     "#ffffff")
      (error "#ff0000")
      (warning "#ffaa00")
      (highlight-line "#121e12")
      (line-fg "#126367"))

  (custom-theme-set-faces
   'modus-custom

   ;; Default colors
   ;; *****************************************************************************

   `(default                          ((t (:foreground ,fg-main :background ,bg-main, :weight normal))))
   `(cursor                           ((t (:background ,white                        ))))
   `(region                           ((t (:foreground ,fg-region :background ,bg-region))))
   `(fringe                           ((t (:background ,bg-main   :foreground ,fg-main))))
   `(linum                            ((t (:background ,bg-main :foreground ,gutter-fg))))
   `(highlight ((t (:foreground nil :background ,selection))))

   ;; Font lock faces
   ;; *****************************************************************************

   `(font-lock-keyword-face           ((t (:foreground ,keywords))))
   `(font-lock-type-face              ((t (:foreground ,punctuation))))
   `(font-lock-constant-face          ((t (:foreground ,constants))))
   `(font-lock-variable-name-face     ((t (:foreground ,variables))))
   `(font-lock-builtin-face           ((t (:foreground ,strings))))
   `(font-lock-string-face            ((t (:foreground ,strings))))
   `(font-lock-comment-face           ((t (:foreground ,comments))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,comments))))
   `(font-lock-doc-face               ((t (:foreground ,comments))))
   `(font-lock-function-name-face     ((t (:foreground ,functions))))
   `(font-lock-function-call-face     ((t (:foreground ,functions))))
   `(font-lock-doc-string-face        ((t (:foreground ,strings))))
   `(font-lock-preprocessor-face      ((t (:foreground ,macros))))
   `(font-lock-warning-face           ((t (:foreground ,warning))))

   ;; Plugins
   ;; *****************************************************************************
   `(trailing-whitespace ((t (:foreground nil :background ,warning))))
   `(whitespace-trailing ((t (:background nil :foreground ,warning :inverse-video t))))

   `(linum ((t (:foreground ,line-fg :background ,bg-main))))
   `(linum-relative-current-face ((t (:foreground ,white :background ,bg-main))))
   `(line-number ((t (:foreground ,line-fg :background ,bg-main))))
   `(line-number-current-line ((t (:foreground ,white :background ,bg-main))))

   ;; hl-line-mode
   `(hl-line ((t (:background ,highlight-line))))
   `(hl-line-face ((t (:background ,highlight-line))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,modus-theme-violet))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,modus-theme-blue))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,modus-theme-green))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,modus-theme-yellow))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,modus-theme-orange))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,modus-theme-red))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,modus-theme-violet))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,modus-theme-blue))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,modus-theme-green))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,modus-theme-yellow))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,modus-theme-orange))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,modus-theme-red))))

   ;; which-func
   `(which-func ((t (:inverse-video unspecified
                                    :underline unspecified
                                    :foreground ,bg-main
                                    :weight bold
                                    :box nil))))

   ;; mode-line and powerline
   `(mode-line-buffer-id ((t (:foreground ,bg-main :distant-foreground ,fg-main :text ,fg-main :weight bold))))
   `(mode-line ((t (:inverse-video unspecified
                                   :underline unspecified
                                   :foreground ,bg-main
                                   :background ,fg-main
                                   :box nil))))
   `(powerline-active1 ((t (:background ,fg-main :foreground ,bg-main))))
   `(powerline-active2 ((t (:background ,fg-main :foreground ,bg-main))))

   `(mode-line-inactive ((t (:inverse-video unspecified
                                            :underline unspecified
                                            :foreground ,fg-main
                                            :background ,bg-main
                                            :box nil))))
   `(powerline-inactive1 ((t (:background ,bg-main :foreground ,fg-main))))
   `(powerline-inactive2 ((t (:background ,bg-main :foreground ,fg-main))))

    ;; better compatibility with default DOOM mode-line
   `(error ((t (:foreground nil :weight normal))))
   `(doom-modeline-project-dir ((t (:foreground nil :weight bold))))
   
   ;; js2-mode
   `(js2-function-call ((t (:inherit (font-lock-function-name-face)))))
   `(js2-function-param ((t (:foreground ,fg-main))))
   `(js2-jsdoc-tag ((t (:foreground ,keywords))))
   `(js2-jsdoc-type ((t (:foreground ,constants))))
   `(js2-jsdoc-value((t (:foreground ,fg-main))))
   `(js2-object-property ((t (:foreground ,fg-main))))
   `(js2-external-variable ((t (:foreground ,constants))))
   `(js2-error ((t (:foreground ,error))))
   `(js2-warning ((t (:foreground ,warning))))

   ;; highlight numbers
   `(highlight-numbers-number ((t (:foreground ,4-numbers))))

   ;; tab-bar-mode
   `(tab-bar ((t (:inherit modeline))))
   `(tab-bar-tab ((t (:foreground ,bg-main :background ,fg-main))))
   `(tab-bar-tab-inactive ((t (:foreground ,fg-main :background ,bg-main))))

   ;;~ TreeSitter
    
   ;; Operators
   `(tree-sitter-hl-face:operator ((t (:foreground ,"#bd2d2d"))))

   ;; Punctuation
   `(tree-sitter-hl-face:punctuation.delimiter ((t (:foreground ,fg-main))))
   `(tree-sitter-hl-face:punctuation.bracket   ((t (:foreground ,fg-main))))
   `(tree-sitter-hl-face:punctuation.special   ((t (:foreground ,fg-main))))

   ;; String
   `(tree-sitter-hl-face:string        ((t (:foreground ,strings))))
   `(tree-sitter-hl-face:string.regex  ((t (:foreground ,strings))))
   `(tree-sitter-hl-face:string.excape ((t (:foreground ,strings))))

   ;; Functions
   `(tree-sitter-hl-face:constructor ((t (:foreground ,functions))))
   `(tree-sitter-hl-face:function   ((t (:foreground ,functions))))
   `(tree-sitter-hl-face:function.macro   ((t (:foreground ,macros))))
   `(tree-sitter-hl-face:function.builtin ((t (:foreground ,functions))))
   `(tree-sitter-hl-face:function.call    ((t (:foreground ,functions))))
   `(tree-sitter-hl-face:method.call      ((t (:foreground ,functions)))) 
   `(tree-sitter-hl-face:type.parameter   ((t (:foreground ,fg-main))))

   ;; Keywords
   `(tree-sitter-hl-face:keyword ((t (:foreground ,keywords))))
   `(tree-sitter-hl-face:label ((t (:foreground ,fg-main))))

   ;; Type
   `(tree-sitter-hl-face:type                ((t (:foreground ,4-types))))
   `(tree-sitter-hl-face:type.builtin        ((t (:foreground ,keywords))))
   `(tree-sitter-hl-face:property            ((t (:foreground ,fg-main))))
   `(tree-sitter-hl-face:property.definition ((t (:foreground ,fg-main))))
   `(tree-sitter-hl-face:attribute           ((t (:foreground ,fg-main))))

   ;;Identifiers
   `(tree-sitter-hl-face:variable            ((t (:foreground ,fg-main))))
   `(tree-sitter-hl-face:variable.builtin    ((t (:foreground ,fg-main))))
  )

  (custom-theme-set-variables
    'modus-custom
    '(linum-format " %5i ")
  )
)
;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; *****************************************************************************
(provide-theme 'modus-custom)
(provide 'modus-custom)
;;; modus_custom.el ends here
;
; (load-theme 'modus-custom t)
;  (setq evil-normal-state-cursor '(box "#dd4040")
;		evil-insert-state-cursor '((bar . 2) "#40FF40"))
