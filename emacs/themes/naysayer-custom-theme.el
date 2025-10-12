;;; naysayer_custom.el --- The naysayer theme with tree-sitter  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  

;; Author:  <lucashdez>
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
  (error "The naysayer theme requires Emacs 24 or later!"))

(deftheme naysayer-custom "The naysayer color theme")

;; Monokai colors
(defcustom naysayer-theme-yellow "#E6DB74" "Primary colors - yellow" :type 'string :group 'monokai)
(defcustom naysayer-theme-orange "#FD971F" "Primary colors - orange" :type 'string :group 'monokai)
(defcustom naysayer-theme-red "#F92672" "Primary colors - red" :type 'string :group 'monokai)
(defcustom naysayer-theme-magenta "#FD5FF0" "Primary colors - magenta" :type 'string :group 'monokai)
(defcustom naysayer-theme-blue "#66D9EF" "Primary colors - blue" :type 'string :group 'monokai)
(defcustom naysayer-theme-green "#A6E22E" "Primary colors - green" :type 'string :group 'monokai)
(defcustom naysayer-theme-cyan "#A1EFE4" "Primary colors - cyan" :type 'string :group 'monokai)
(defcustom naysayer-theme-violet "#AE81FF" "Primary colors - violet" :type 'string :group 'monokai)

(let ((background "#062329")
      (gutters    "#062329")
      (gutter-fg  "#062329")
      (gutters-active "#062329")
      (builtin      "#ffffff")
      (selection  "#0000ff")
      (text       "#d1b897")
      (comments   "#44b340")
      (punctuation "#8cde94")
      (keywords "#ffffff")
      (variables "#c1d1e3")
      (functions "#ffffff")
      (methods    "#c1d1e3")
      (strings    "#2ec09c")
      (constants "#7ad0c6")
      (macros "#8cde94")
      (numbers "#7ad0c6")
      (white     "#ffffff")
      (error "#ff0000")
      (warning "#ffaa00")
      (highlight-line "#0b3335")
      (line-fg "#126367"))

  (custom-theme-set-faces
   'naysayer-custom

   ;; Default colors
   ;; *****************************************************************************

   `(default                          ((t (:foreground ,text :background ,background, :weight normal))))
   `(region                           ((t (:foreground nil :background ,selection))))
   `(cursor                           ((t (:background ,white                        ))))
   `(fringe                           ((t (:background ,background   :foreground ,white))))
   `(linum                            ((t (:background ,background :foreground ,gutter-fg))))
   `(highlight ((t (:foreground nil :background ,selection))))

   ;; Font lock faces
   ;; *****************************************************************************

   `(font-lock-keyword-face           ((t (:foreground ,keywords))))
   `(font-lock-type-face              ((t (:foreground ,punctuation))))
   `(font-lock-constant-face          ((t (:foreground ,constants))))
   `(font-lock-variable-name-face     ((t (:foreground ,variables))))
   `(font-lock-builtin-face           ((t (:foreground ,builtin))))
   `(font-lock-string-face            ((t (:foreground ,strings))))
   `(font-lock-comment-face           ((t (:foreground ,comments))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,comments))))
   `(font-lock-doc-face               ((t (:foreground ,comments))))
   `(font-lock-function-name-face     ((t (:foreground ,functions))))
   `(font-lock-doc-string-face        ((t (:foreground ,strings))))
   `(font-lock-preprocessor-face      ((t (:foreground ,macros))))
   `(font-lock-warning-face           ((t (:foreground ,warning))))

   ;; Plugins
   ;; *****************************************************************************
   `(trailing-whitespace ((t (:foreground nil :background ,warning))))
   `(whitespace-trailing ((t (:background nil :foreground ,warning :inverse-video t))))

   `(linum ((t (:foreground ,line-fg :background ,background))))
   `(linum-relative-current-face ((t (:foreground ,white :background ,background))))
   `(line-number ((t (:foreground ,line-fg :background ,background))))
   `(line-number-current-line ((t (:foreground ,white :background ,background))))

   ;; hl-line-mode
   `(hl-line ((t (:background ,highlight-line))))
   `(hl-line-face ((t (:background ,highlight-line))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,naysayer-theme-violet))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,naysayer-theme-blue))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,naysayer-theme-green))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,naysayer-theme-yellow))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,naysayer-theme-orange))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,naysayer-theme-red))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,naysayer-theme-violet))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,naysayer-theme-blue))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,naysayer-theme-green))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,naysayer-theme-yellow))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,naysayer-theme-orange))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,naysayer-theme-red))))

   ;; which-func
   `(which-func ((t (:inverse-video unspecified
                                    :underline unspecified
                                    :foreground ,background
                                    :weight bold
                                    :box nil))))

   ;; mode-line and powerline
   `(mode-line-buffer-id ((t (:foreground ,background :distant-foreground ,text :text ,text :weight bold))))
   `(mode-line ((t (:inverse-video unspecified
                                   :underline unspecified
                                   :foreground ,background
                                   :background ,text
                                   :box nil))))
   `(powerline-active1 ((t (:background ,text :foreground ,background))))
   `(powerline-active2 ((t (:background ,text :foreground ,background))))

   `(mode-line-inactive ((t (:inverse-video unspecified
                                            :underline unspecified
                                            :foreground ,text
                                            :background ,background
                                            :box nil))))
   `(powerline-inactive1 ((t (:background ,background :foreground ,text))))
   `(powerline-inactive2 ((t (:background ,background :foreground ,text))))

    ;; better compatibility with default DOOM mode-line
   `(error ((t (:foreground nil :weight normal))))
   `(doom-modeline-project-dir ((t (:foreground nil :weight bold))))
   
   ;; js2-mode
   `(js2-function-call ((t (:inherit (font-lock-function-name-face)))))
   `(js2-function-param ((t (:foreground ,text))))
   `(js2-jsdoc-tag ((t (:foreground ,keywords))))
   `(js2-jsdoc-type ((t (:foreground ,constants))))
   `(js2-jsdoc-value((t (:foreground ,text))))
   `(js2-object-property ((t (:foreground ,text))))
   `(js2-external-variable ((t (:foreground ,constants))))
   `(js2-error ((t (:foreground ,error))))
   `(js2-warning ((t (:foreground ,warning))))

   ;; highlight numbers
   `(highlight-numbers-number ((t (:foreground ,numbers))))

   ;; tab-bar-mode
   `(tab-bar ((t (:inherit modeline))))
   `(tab-bar-tab ((t (:foreground ,background :background ,text))))
   `(tab-bar-tab-inactive ((t (:foreground ,text :background ,background))))

   ;;~ TreeSitter
    
   ;; Operators
   `(tree-sitter-hl-face:operator ((t (:foreground ,"#bd2d2d"))))

   ;; Punctuation
   `(tree-sitter-hl-face:punctuation.delimiter ((t (:foreground ,text))))
   `(tree-sitter-hl-face:punctuation.bracket   ((t (:foreground ,text))))
   `(tree-sitter-hl-face:punctuation.special   ((t (:foreground ,text))))

   ;; String
   `(tree-sitter-hl-face:string        ((t (:foreground ,"#2ec09c"))))
   `(tree-sitter-hl-face:string.regex  ((t (:foreground ,"#2ec09c"))))
   `(tree-sitter-hl-face:string.excape ((t (:foreground ,"#2ec09c"))))

   ;; Functions
   `(tree-sitter-hl-face:constructor ((t (:foreground ,"#c1d1e3"))))
   `(tree-sitter-hl-face:function   ((t (:foreground ,"#c1d1e3"))))
   `(tree-sitter-hl-face:function.macro   ((t (:foreground ,"#ffffff"))))
   `(tree-sitter-hl-face:function.builtin ((t (:foreground ,"#c1d1e3"))))
   `(tree-sitter-hl-face:function.call    ((t (:foreground ,"#c1d1e3"))))
   `(tree-sitter-hl-face:method.call      ((t (:foreground ,"#c1d1e3")))) 
   `(tree-sitter-hl-face:type.parameter   ((t (:foreground ,text))))

   ;; Keywords
   `(tree-sitter-hl-face:keyword ((t (:foreground ,"#ffffff"))))
   `(tree-sitter-hl-face:label ((t (:foreground ,"#ffffff"))))

   ;; Type
   `(tree-sitter-hl-face:type                ((t (:foreground ,"#8cde94"))))
   `(tree-sitter-hl-face:type.builtin        ((t (:foreground ,"#ffffff"))))
   `(tree-sitter-hl-face:property            ((t (:foreground ,text))))
   `(tree-sitter-hl-face:property.definition ((t (:foreground ,text))))
   `(tree-sitter-hl-face:attribute           ((t (:foreground ,"#7ad0c6"))))

   ;;Identifiers
   `(tree-sitter-hl-face:variable            ((t (:foreground ,text))))
   `(tree-sitter-hl-face:variable.builtin    ((t (:foreground , "#ffffff"))))
  )

  (custom-theme-set-variables
    'naysayer-custom
    '(linum-format " %5i ")
  )
)

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; *****************************************************************************

(provide-theme 'naysayer-custom)


(provide 'naysayer-custom)
;;; naysayer_custom.el ends here
