(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-archive-enable-alist '(("melpa" deft magit)))

;; DEFINE PACKAGES
(defvar gasser/packages '(auto-complete
                          ace-jump-mode
                          anaconda-mode
                          fiplr
                          pylint
                          jedi
                          autopair
                          helm
                          org
                          clojure-mode
                          coffee-mode
                          importmagic
                          flycheck
                          fill-column-indicator
                          htmlize
                          magit
                          marmalade
                          multiple-cursors
                          php-mode
                          py-autopep8
                          puppet-mode
                          solarized-theme
                          web-mode
                          writegood-mode
                          yaml-mode)
  "Default packages")

;; INSTALLING
(defun gasser/packages-installed-p ()
  (cl-loop for pkg in gasser/packages
        when (not (package-installed-p pkg)) do (cl-return nil)
        finally (cl-return t)))

(unless (gasser/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg gasser/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; INITIAL
(setq inhibit-splash-screen t
      initial-scratch-message nil)
(switch-to-buffer (get-buffer-create "emtpy"))
(delete-other-windows)

;; APPEARANCE
(set-face-attribute 'default nil :height 120)
(toggle-scroll-bar -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq column-number-mode t)
(if window-system
    (load-theme 'solarized-dark t)
  (load-theme 'wombat t))

;; MARKING TEXT
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; DISPLAY SETTINGS
(global-linum-mode 1)
(setq-default frame-title-format "%b (%f)")
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))
(require 'fill-column-indicator)
(setq-default fill-column 79)
(setq fci-rule-width 1)
(setq fci-rule-color "darkblue")
(add-hook 'prog-mode-hook 'fci-mode)

;; INDENTATION
(setq standard-indent 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq c-default-style "linux")
(setq c-basic-offset 4)
(c-set-offset 'comment-intro 0)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))

;; BACKUP FILES
(setq make-backup-files nil)

;; YES / NO
(defalias 'yes-or-no-p 'y-or-n-p)

;; KEY BINDINGS

;;  duplicate line
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-q") 'forward-word)
(global-set-key (kbd "C-x d") 'duplicate-line)

;; UNDO REDO
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-r") 'redo)

;; MISC
(show-paren-mode t)
(require 'autopair)
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook 'subword-mode)

;; ORG HABIT
(require 'org)
(require 'org-install)
(require 'org-habit)
(add-to-list 'org-modules "org-habit")
(setq org-habit-preceding-days 7
      org-habit-following-days 1
      org-habit-graph-column 80
      org-habit-show-habits-only-for-today t
      org-habit-show-all-today t)

;; TEMPORARY FILES
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; AUTOCOMPLETE
(require 'auto-complete-config)
(ac-config-default)

;; CONF MODE
(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))

;; WEB MODE
(add-to-list 'auto-mode-alist '("\\.hbs$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))

;; YAML
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; COFFE SCRIPT
(defun coffee-custom ()
  "coffee-mode-hook"
  (make-local-variable 'tab-width)
  (set 'tab-width 2))

(add-hook 'coffee-mode-hook 'coffee-custom)

;; JAVASCRIPT MODE
(defun js-custom ()
  "js-mode-hook"
  (setq js-indent-level 2))

(add-hook 'js-mode-hook 'js-custom)

;; MARKDOWN MODE
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-hook 'markdown-mode-hook
          (lambda ()
            (visual-line-mode t)
            (writegood-mode t)
            (flyspell-mode t)))
(setq markdown-command "pandoc --smart -f markdown -t html")

;; ANACONDA
(add-hook 'python-mode-hook 'anaconda-mode)

;; FUZZY SEARCH
(setq fiplr-root-markers '(".hg" ".git"))
(setq fiplr-ignored-globs '((directories (".hg" "uploads"))
                            (files ("*.jpg" "*.pyc" "*.po" "*.png" "*.zip" "*~"))))
(global-set-key (kbd "C-x p") 'fiplr-find-file)


;; ACE MODE
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
(global-set-key (kbd "C-ñ") 'ace-jump-line-mode)

;; MULTIPLE CURSOR
(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c w") 'mc/mark-next-like-this)

;; PEP 8
;(require 'py-autopep8)
;(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

;; FLYCHECK
;(require 'flycheck)
;;(global-flycheck-mode t)

;; IMPORT MAGIC
(require 'importmagic)
(add-hook 'python-mode-hook 'importmagic-mode)


;; JEDI
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'jedi:ac-setup)

;; AUTO GENERATE
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yaml-mode writegood-mode web-mode solarized-theme puppet-mode php-mode marmalade magit htmlize flycheck coffee-mode clojure-mode autopair auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
