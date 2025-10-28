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
;; Integrar straight.el con use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

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
(set-frame-parameter nil 'alpha-background 92)
(add-to-list 'default-frame-alist '(alpha-background . 90))
(setq split-width-threshold 999999)

(use-package helm
  :ensure t
  :init (helm-mode t))

(use-package org-superstar
  :ensure t
  :after org)

(use-package org-super-agenda
  :ensure t
  :init (org-super-agenda-mode)
  :bind (("C-c a" . org-agenda))
  )

(defun my-org-mode-hook ()
  (org-superstar-mode)
  (org-superstar-configure-like-org-bullets))

(add-hook 'org-mode-hook 'my-org-mode-hook)
