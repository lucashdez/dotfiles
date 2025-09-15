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

(cond
 ((find-font (font-spec :name "FiraCode Nerd Font"))
      (set-frame-font "FiraCode Nerd Font-14")))

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
(set-frame-parameter nil 'alpha-background 90)
(add-to-list 'default-frame-alist '(alpha-background . 90))

(setq make-backup-files nil) ;; DO NOT BACKUP
(add-to-list 'custom-theme-load-path (concat rc/system "El-Arca/EmacsConfig/themes/"))

(setq-default tab-width 4)
(setq-default ident-tabs-mode nil)
(setq-default c-basic-offset 4)



;(load-theme 'solarized-seriuusly t)

										;(rc/require-theme 'gruber-darker)
										;(rc/require 'solarized-theme)
										;(load-theme 'solarized-dark-high-contrast t)
										;(rc/require-theme 'catppuccin)
										;(setq catppuccin-flavor 'mocha)
										;(rc/require 'modus-themes)
										;(load-theme 'modus-vivendi-deuteranopia 1)
										;(rc/require 'naysayer-theme) 
										;(rc/require-theme 'dracula)
										;(load-theme 'naysayer t)
										;(load-theme 'naysayer-custom t)
										;(set-cursor-color "#98fb98")
										;(rc/require 'nord-theme)
(load-theme '4coder t)
										;(setq evil-normal-state-cursor '(box "#dd4040")
										;evil-insert-state-cursor '((bar . 2) "#40FF40"))

;(set-cursor-color "#ffff80")

(rc/require 'evil 'evil-mc)
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
(global-evil-mc-mode 1)
(global-set-key (kbd "C-S-c C-S-c") 'evil-mc-make-all-cursors)
(global-set-key (kbd "C->") 'evil-mc-make-and-goto-next-match)
(global-set-key (kbd "C-<") 'evil-mc-make-and-goto-prev-match)

(rc/require 'all-the-icons)
(use-package all-the-icons :ensure t :defer t)

(rc/require 'ligature)
      (use-package ligature :defer t)
;; Enable the www ligature in every possible major mode
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

(rc/require 'rainbow-delimiters)
(use-package rainbow-delimiters :defer t
      :hook (prog-mode . rainbow-delimiters-mode))

(rc/require 'doom-modeline)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 20)))


; LSP THINGS
(setenv "LSP_USE_PLISTS" "true")
(rc/require 'lsp-mode)
(add-hook 'c-mode-hook #'lsp-deferred)

(ignore-errors
  (rc/require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
	(when (eq major-mode 'compilation-mode)
	  (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

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

;####
; TREESITTER
(rc/require 'tree-sitter)

(setq treesit-language-source-alist
	  '((zig "https://github.com/tree-sitter-grammars/tree-sitter-zig")))

;####

;####
; LSP SERVERS
;(add-hook 'rust-mode-hook #'lsp-deferred)
;(add-hook 'rust-mode-hook 'tree-sitter-mode)

;(add-hook 'zig-mode-hook #'lsp-deferred)
(add-hook 'zig-mode-hook 'tree-sitter-hl-mode)


(add-hook 'emacs-lisp-mode-hook 'company-mode)

; VISUAL BASIC
;(load-file "~/../../drive/El-Arca/EmacsConfig/visual-basic.el")

; ############


