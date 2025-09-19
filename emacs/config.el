;;######
;; PACKAGE MANAGEMENT
(setenv "LSP_USE_PLISTS" "true")
(require 'package)
(add-to-list 'package-archives
			 '("MELPA"
			   . "http://melpa.org/packages/"))
(defvar bootstrap-version)
(let ((bootstrap-file
	   (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	  (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
	(with-current-buffer
		(url-retrieve-synchronously
		 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
		 'silent 'inhibit-cookies)
	  (goto-char (point-max))
	  (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;;----

;;######
;; DISABLE MOUSE
(define-minor-mode disable-mouse-mode
  "A minor-mode that disables all mouse keybinds."
  :global t
  :lighter " mouse"
  :keymap (make-sparse-keymap))

(dolist (type '(mouse down-mouse drag-mouse
					  double-mouse triple-mouse))
  (dolist (prefix '("" C- M- S- M-S- C-M- C-S- C-M-S-))
	;; Yes, I actually HAD to go up to 7 here.
	(dotimes (n 7)
	  (let ((k (format "%s%s-%s" prefix type n)))
		(define-key disable-mouse-mode-map
					(vector (intern k)) #'ignore)))))
(disable-mouse-mode 1)
;;----

;;######
;; SPECIAL REQUIRES
(defvar rc/package-contents-refreshed nil)
(defvar rc/system (if (eq system-type 'windows-nt)
					  "~/../../drive/"
					"~/drive/"))

(defun rc/package-refresh-contents-once ()
  (when (not rc/package-contents-refreshed)
	(setq rc/package-contents-refreshed t)
	(package-refresh-contents)))
(defun rc/require-one-package (package)
  (when (not (package-installed-p package))
	(rc/package-refresh-contents-once)
	(package-install package)))

(defun rc/require (&rest packages)
  (dolist (package packages)
	(rc/require-one-package package)))



(defun rc/require-theme (theme)
  (let ((theme-name (symbol-name theme))
		(theme-package (intern (concat (symbol-name theme) "-theme"))))
	(rc/require theme-package)
	(load-theme theme t))
  )

(defun rc/toggle-window-transparency ()
  "Toggle transparency."
  (interactive)
  (let ((alpha-transparency 75))
	(pcase (frame-parameter nil 'alpha-background)
	  (alpha-transparency (set-frame-parameter nil 'alpha-background 100))
	  (t (set-frame-parameter nil 'alpha-background alpha-transparency)))))
;;----

;;######
;;FONT
(cond
 ((find-font (font-spec :name "FiraCode Nerd Font"))
  (set-frame-font "FiraCode Nerd Font-14")))
;;----

;;######
;; FRAME VARIABLES
(setq inhibit-startup-screen t)
(setq default-directory (if (eq system-type 'windows-nt)
							"~/../../drive/"
						  "~/drive/"))
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode -1)
(display-line-numbers-mode t) 
(global-display-line-numbers-mode t) ;; NUMEROS DE LINEA
(setq display-line-numbers-type 'relative)
(setq completion-at-point-functions '(elisp-completion-at-point comint-dynamic-complete-filename t))
(add-hook 'org-mode-hook (lambda () (visual-line-mode 1)))
(global-hl-line-mode 1)
(setq visible-bell 1)
(setq ring-bell-function 'ignore)
(setq inhibit-compacting-font-caches t)
(setq find-file-visit-truename t)
(setq display-time-mode t)
(setq make-backup-files nil) ;; DO NOT BACKUP
(set-frame-parameter nil 'alpha-background 90)
(add-to-list 'default-frame-alist '(alpha-background . 90))
;;----

;; ######
;; CUSTOM THEME LOAD AND FILE LOAD
(add-to-list 'custom-theme-load-path (concat rc/system "El-Arca/EmacsConfig/themes/"))
;;----

;;######
;; CONFIG
(setq-default tab-width 4)
(setq-default ident-tabs-mode nil)
;;----

;;######
;; THEMES
;;(load-theme 'solarized-seriuusly t)
;;(rc/require-theme 'gruber-darker)
;;(rc/require 'solarized-theme)
;;(load-theme 'solarized-dark-high-contrast t)
;;(rc/require-theme 'catppuccin)
;;(setq catppuccin-flavor 'mocha)
;;(rc/require 'modus-themes)
;;(load-theme 'modus-vivendi-deuteranopia 1)
(rc/require 'naysayer-theme) 
;;(rc/require-theme 'dracula)
;;(load-theme 'naysayer t)
;;(load-theme 'naysayer-custom t)
;;(set-cursor-color "#98fb98")
;;(rc/require 'nord-theme)
(load-theme 'naysayer t)
;;----

;;######
;; PLUGINS
(package-initialize)
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
	 (let ((bytecode (read (current-buffer))))
	   (when (byte-code-function-p bytecode)
		 (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
					   (fboundp 'json-parse-buffer))
				'json-parse-buffer
			  'json-read)
			:around
			#'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
	(if (and (not test?)                             ;; for check lsp-server-present?
			 (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
			 lsp-use-plists
			 (not (functionp 'json-rpc-connection))  ;; native json-rpc
			 (executable-find "emacs-lsp-booster"))
		(progn
		  (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
			(setcar orig-result command-from-exec-path))
		  (message "Using emacs-lsp-booster for %s!" orig-result)
		  (cons "emacs-lsp-booster" orig-result))
	  orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
;; ----

;;;;;;;;;;;;;;;;;
;; ALL PLUGINS ;; 
;;;;;;;;;;;;;;;;;


(rc/require 'tree-sitter)
(rc/require 'tree-sitter-langs)
(rc/require 'all-the-icons)
(rc/require 'doom-modeline)
(rc/require 'ligature)
(rc/require 'rainbow-delimiters)
(rc/require 'evil)
(rc/require 'evil-easymotion)
(rc/require 'evil-mc)
(rc/require 'magit)
(rc/require 'git-gutter+)
(rc/require 'helm)
(rc/require 'helm-git-grep)
(rc/require 'helm-ls-git)
(rc/require 'flycheck-rust)
(rc/require 'company)
(rc/require 'posframe)
(rc/require 'lsp-mode)
(rc/require 'lsp-ui)
(rc/require 'lsp-haskell)
(rc/require 'fancy-dabbrev)
(rc/require 'org-modern)
(rc/require 'org-super-agenda)
(rc/require 'mermaid-mode)
(rc/require 'smartparens)
(rc/require 'vimish-fold)
(rc/require 'evil-vimish-fold)
										; Treesitter
(global-tree-sitter-mode)				
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
(tree-sitter-require 'rust)

(setq treesit-language-source-alist
	  '((csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
		(elisp "https://github.com/emacs-tree-sitter/elisp-tree-sitter.git")
		(rust "https://github.com/tree-sitter/tree-sitter-rust.git")
		(lua "https://github.com/tjdevries/tree-sitter-lua.git")
		(typescript "https://github.com/tree-sitter/tree-sitter-typescript.git")
		(clojure "https://github.com/sogaiu/tree-sitter-clojure.git")
		(haskell "https://github.com/tree-sitter/tree-sitter-haskell.git")
		(css "https://github.com/tree-sitter/tree-sitter-css.git")
		(yaml "https://github.com/ikatyang/tree-sitter-yaml.git")
		))
										; Language modes
(rc/require 'rust-mode)				
(rc/require 'lua-mode)
(rc/require 'typescript-mode)
(rc/require 'clojure-mode)
(rc/require 'haskell-mode)

										; Autothemer
(use-package autothemer :ensure t :defer t)
										; All the icons
(use-package all-the-icons :ensure t :defer t)
										; Doom modeline 
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 20))) 

										; Ligatures
(use-package ligature :defer t)
(ligature-set-ligatures 't '("www"))
;; Enable ligatures in programming modes                                                           
(ligature-set-ligatures 'prog-mode '("www" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
									 ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
									 "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
									 "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
									 "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
									 "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
									 "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
									 "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
									 "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
									 "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

(global-ligature-mode 't)
										; Rainbow delimiters
(use-package rainbow-delimiters :defer t
  :hook (prog-mode . rainbow-delimiters-mode))
										; EVIL MODE
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-k") 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "C-S-p") 'delete-window)
  (define-key evil-normal-state-map (kbd "C-,") 'other-window)
  (define-key evil-normal-state-map (kbd "C--") 'split-window-horizontally)
  (define-key evil-normal-state-map (kbd "C-.") 'split-window-vertically)
  )
(evilem-default-keybindings "SPC")
(global-evil-mc-mode 1)
(global-set-key (kbd "C-S-c C-S-c") 'evil-mc-make-all-cursors)
(global-set-key (kbd "C->") 'evil-mc-make-and-goto-next-match)
(global-set-key (kbd "C-<") 'evil-mc-make-and-goto-prev-match)
										; GIT GUTTER
(add-hook 'prog-mode-hook 'git-gutter+-mode)
(setq git-gutter+-added-sign "|")
(setq git-gutter+-modified-sign "|")

  										; Flycheck
(with-eval-after-load 'rust-mode
  (add-hook 'rust-mode-hook #'flycheck-rust-setup))
										; Company
(global-company-mode 1)
										; LSP
(set 'lsp-use-plists t)
(use-package lsp-mode
  :straight t
  :init (add-to-list 'company-backends 'company-capf)
  :config
  (setq lsp-ui-doc-enable nil)
  :commands lsp)
(use-package lsp-ui :defer t)
;;HOOKS
(add-hook 'rust-mode-hook #'lsp-deferred)
(add-hook 'typescript-mode-hook #'lsp-deferred)
(add-hook 'lua-mode-hook #'lsp-deferred)
(add-hook 'c-mode-common-hook #'lsp-deferred)
(add-hook 'gdscript-mode-hook #'lsp-deferred)
;; CONFIG
(global-set-key (kbd "C-<tab>") 'lsp-ui-doc-toggle)
(global-set-key (kbd "C-<return>") 'lsp-ui-imenu)
(setq lsp-ui-doc-position 'at-point)
(setq lsp-ui-doc-show-with-cursor nil)
(setq lsp-lens-enable t)
(setq lsp-diagnostics-provider :flycheck)
(setq lsp-ui-sideline-enable t)
(setq lsp-ui-sideline-show-code-actions nil)
(setq lsp-signature-render-documentation nil)
(setq lsp-signature-function 'lsp-lv-message)
(setq lsp-completion-provider :company-mode)
(setq lsp-completion-show-detail t)
(setq lsp-completion-show-kind t)
(add-hook 'haskell-mode-hook #'lsp-deferred)
(add-hook 'haskell-literate-mode-hook #'lsp-deferred)
(global-fancy-dabbrev-mode t)
(global-set-key (kbd "<backtab>") 'fancy-dabbrev-expand)
(global-org-modern-mode t)
(setq
 org-modern-todo-faces nil 
 org-modern-todo nil)
(setq org-modern-star 'replace)

(setq evil-vimish-fold-target-modes '(prog-mode conf-mode text-mode))
(global-evil-vimish-fold-mode 1)
(use-package vimish-fold
  :ensure
  :after evil)

(use-package evil-vimish-fold
  :ensure
  :after vimish-fold
  :hook ((prog-mode conf-mode text-mode) . evil-vimish-fold-mode))

;;;;;;;;;;
;; ---- ;;
;;;;;;;;;;

;;######
;; ORG
(setq org-todo-keyword-faces
	  '(("TODO" . (:foreground "white" :background "seagreen" :weight bold))
		("IN PROGRESS" . (:foreground "white" :background "goldenrod" :weight bold))
		("DONE" . (:foreground "white" :background "DeepSkyBlue4"))))
(setq org-todo-keywords
	  '((sequence "TODO" "IN PROGRESS" "DONE")))


(setq org-todo-keyword-faces
	  '(("TODO" . (:foreground "white" :background "seagreen" :weight bold))
		("IN PROGRESS" . (:foreground "white" :background "goldenrod" :weight bold))
		("DONE" . (:foreground "white" :background "DeepSkyBlue4"))))
(setq org-todo-keywords
	  '((sequence "TODO" "IN PROGRESS" "DONE")))

(org-super-agenda-mode t)
(setq org-agenda-span 'day)
(setq org-super-agenda-groups
	  '(;; Each group has an implicit boolean OR operator between its selectors.
		(:name "TODAY"
			   :and (:time-grid t))
		(:name "IMPORTANT WORK"
			   :and (:tag "work" :priority "A"))
		(:name "WORK"
			   :tag "work")

		;; Groups supply their own section names when none are given
		(:todo "WAITING" :order 8)  ; Set order of this section
		(:priority<= "B"
					 ;; Show this section after "Today" and "Important", because
					 ;; their order is unspecified, defaulting to 0. Sections
					 ;; are displayed lowest-number-first.
					 :order 1)
		;; After the last group, the agenda will display items that didn't
		;; match any of these groups, with the default order position of 99
		))

(setq calendar-week-start-day 1)
(customize-set-variable 'holiday-bahai-holidays nil)
(customize-set-variable 'holiday-hebrew-holidays nil)
(customize-set-variable 'holiday-islamic-holidays nil)
(customize-set-variable 'holiday-general-holidays nil)
(customize-set-variable 'holiday-christian-holidays t)
(setq calendar-holidays
	  '(
		;; State holidays
		(holiday-fixed 1 1 "Fiesta Nacional: A�o Nuevo")
		(holiday-fixed 1 6 "Fiesta Nacional: Dia de Reyes/Epifania")
		(holiday-fixed 5 1 "Fiesta Nacional: International Labor Day")
		(holiday-fixed 5 24 "Fiesta Nacional: Segunda Pascua")
		(holiday-fixed 6 24 "Fiesta Nacional: San Juan")
		(holiday-fixed 8 15 "Fiesta Nacional: Asuncion de la Virgen")
		(holiday-fixed 10 12 "Fiesta Nacional: Dia de la Hispanidad")
		(holiday-fixed 11 1 "Fiesta Nacional: Todos los Santos")
		(holiday-fixed 12 6 "Fiesta Nacional: Dia de la Constitucion")
		(holiday-fixed 12 8 "Fiesta Nacional: Inmaculada Concepcion")
		(holiday-fixed 12 24 "Fiesta Nacional: Nochebuena")
		(holiday-fixed 12 25 "Fiesta Nacional: Navidad")
		;; floated holidays       
		(holiday-easter-etc  -7 "Domingo de Ramos")
		(holiday-easter-etc  -3 "Fiesta Nacional: Jueves Santo")
		(holiday-easter-etc  -2 "Fiesta Nacional: Viernes Santo")
		(holiday-easter-etc  0 "Domingo de Resurreccion")
		;; Canarias holidays
		(holiday-fixed 5 30 "Fiesta Local: Dia de Canarias")))

(setq org-log-done t)
(setq org-hide-emphasis-markers t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
(let* ((variable-tuple
		(cond ((x-list-fonts "Courier New")         '(:font "Courier New"))
			  (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
	   (base-font-color     (face-foreground 'default nil 'default))
	   (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline ,@variable-tuple))))
   `(org-level-7 ((t (,@headline ,@variable-tuple))))
   `(org-level-6 ((t (,@headline ,@variable-tuple))))
   `(org-level-5 ((t (,@headline ,@variable-tuple))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.05))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.1))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.25))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.5))))
   `(org-document-title ((t (,@headline ,@variable-tuple :height 1.75 :underline nil))))))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(global-set-key (kbd "C-l") nil)
(global-set-key (kbd "C-l") 'duplicate-line)
;;----

;;######
;;CUSTOM FACES
(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "FiraCode Nerd Font" :height 180 :weight thin))))
 '(fixed-pitch ((t ( :family "FiraCode Nerd Font" :height 160))))) 
(set-face-attribute 'org-table nil :inherit 'fixed-pitch)

(custom-theme-set-faces
 'user
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598" :family "Cascadia Code"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))
;;----

;;######
;; AUTOLOAD

(require 'autoinsert)
(auto-insert-mode t)
(defun get-days-in-week(year week-number)
  (let* ((jan-1 (encode-time 0 0 0 1 1 year))
		 (days-of-week '())
		 (day-format "%Y-%m-%d")
		 (seconds-in-a-day 86400))
	;; Calculate the first day of the specified year
	(let* ((start-of-year jan-1)
		   (iso-weekday (nth 6 (decode-time start-of-year)))
		   (days-to-add (- 1 iso-weekday)))
	  (setq start-of-year (time-add start-of-year (seconds-to-time (* seconds-in-a-day days-to-add))))
	  ;; Calculate the first day of the specified week
	  (let* ((start-of-week start-of-year)
			 (days-to-add (* 7 (1- week-number))))
		(setq start-of-week (time-add start-of-week (seconds-to-time (* seconds-in-a-day days-to-add))))
		;; Collect the date strings for all the days of that week
		(dotimes (i 7)
		  (setq days-of-week
				(cons (format-time-string day-format start-of-week)
					  days-of-week))
		  (setq start-of-week (time-add start-of-week (seconds-to-time seconds-in-a-day)))
		  ))
	  )
	(reverse days-of-week)))

(defun org-work-week-string (str-file-name)
  (setq wstr (split-string str-file-name "\\-"))
  (setq wstr1 (replace-regexp-in-string "W" "" (replace-regexp-in-string ".org" "" (nth 1 wstr)))) ;  WXX
  (setq wstr2 (nth 0 wstr)) ; YYYY.org
  (setq days(get-days-in-week (string-to-number wstr2) (string-to-number wstr1)))
  (setq block-title (concat wstr2 "-W" wstr1))
  (setq outputstr (concat "#+TITLE: " block-title "\n" "* CLOCKTABLE\n"))
  (setq outputstr (concat outputstr "#+BEGIN: clocktable :maxlevel 6 :scope agenda :block " block-title " :step day :stepskip0 (t) :fileskip0 :formater :link (t) :formula % :compact (t)\n#+END:\n"))
  (setq outputstr (concat outputstr "* Insercion en Partes\n"))
  (setq outputstr
		(concat outputstr
				(let (value) (dolist (day days value)
							   (setq value
									 (concat value (concat "** " day "\n")
											 (concat "|-\n")
											 (concat "|*P*|*TASK*|*FROM*|*TO*|*TOTAL*|*TOTALAQUA*|*ACCUM*|*ACCUM AQUA*|\n")
											 (concat "|-\n")
											 (concat "|||||||||\n")
											 (concat "|-\n")
											 (concat "#+TBLFM: $5=$4-$3;T::$6=$4-$3;t::$7=$5+vsum(@1$5..@-1$5);T::$8=$7;t\n\n"))))))))

(eval-after-load 'autoinsert
  '(define-auto-insert
	 '("\\([0-9]\\{4\\}\\)-W\\([0-9]\\{2\\}\\)\\.org" . "Week skeleton")
	 '("Comienza a partir de aqui"
	   (org-work-week-string (file-name-nondirectory (buffer-file-name)))
	   )))
;;----

;;######
;; LANGUAGE HOOKS
(defun my-c-mode-common-hook ()
  ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
  (c-set-offset 'substatement-open 0)
  ;; other customizations can go here

  (setq c++-tab-always-indent t)
  (setq c-basic-offset 4)                  ;; Default is 2
  (setq c-indent-level 4)                  ;; Default is 2

  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
  (setq tab-width 4)
  (setq indent-tabs-mode t)  ; use spaces only if nil
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
;;----



;; LOAD THE FILE FOR START
(with-eval-after-load 'org
  (defun org-agenda-files (&rest _)
	(directory-files-recursively
	 (if (eq system-type 'windows-nt)
		 "C:/projects/"
	   "~/projects/")
	 org-agenda-file-regexp)
	))

