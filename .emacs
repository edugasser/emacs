;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(load "package")
(setq highlight-indentation-mode nil)
(set-face-attribute 'default nil :height 117 :width 'semi-condensed)

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
                          elpy
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
(set-face-attribute 'default nil :height 117 :width 'semi-condensed)
(toggle-scroll-bar -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq column-number-mode t)
(if window-system
    (load-theme 'solarized-dark t)
  (load-theme 'wombat t))


(add-to-list 'load-path "~/.emacs.d/lisp/drag-stuff.el-master")
(require 'drag-stuff)
(drag-stuff-mode t)


; Collapse
(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-set-key (kbd "C-c <right>") 'hs-show-block)
(global-set-key (kbd "C-c <left>")  'hs-hide-block)
(global-set-key (kbd "C-c <up>")    'hs-hide-all)
(global-set-key (kbd "C-c <down>")  'hs-show-all)

;; powerline
(powerline-default-theme)
;;
;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

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
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)
(setq fci-rule-column 80)
(setq fci-rule-width 2)
(setq fci-rule-color "darkred")

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


(global-set-key (kbd "<f1>") 'prettyxml)

;; UNDO REDO
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "M-z") 'undo-tree-redo)
(global-set-key (kbd "C-x t") 'undo-tree-visualize)

;; MISC
(global-set-key (kbd "C-.") 'repeat)
(show-paren-mode t)
(require 'autopair)
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-subword-mode 1)

;; UTF8
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8)


; Whitespace
(require 'whitespace)
(add-hook 'before-save-hook 'delete-trailing-whitespace 'whitespace-mode)
(setq-default whitespace-line-column 80)

;; Elpy
(require 'package)
(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))
(elpy-enable)
(setq elpy-rpc-timeout nil)

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

;; PEP 8
;(require 'py-autopep8)
;(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

;; FLYCHECK
;(require 'flycheck)
;;(global-flycheck-mode t)


;; Guardar ficheros temporales en otra carpeta
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
    (setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

;; IMPORT MAGIC
(require 'importmagic)
(add-hook 'python-mode-hook 'importmagic-mode)


;; JEDI
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'jedi:ac-setup)


;; FORMATTING
(defun prettyxml (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))
  (message "Ah, much better!"))

(global-set-key (kbd "C-h") 'prettyxml)


;; ;; Formateado de codigo

(setq standard-indent 4)
;; ;; Tab with
(setq-default tab-width 4)
;; ;; Tab width is 4
(setq tab-width 4)
;; ;; Use spaces always.
(setq indent-tabs-mode nil)
;; ;; make tab key always call a indent command.
;(setq-default tab-always-indent t)
;Jump by 4.
(setq c-basic-offset 4)
;this defaulted to 4 and had to be reset to 3.
(setq perl-indent-level 4)
;Manually set by x4
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(require 'fill-column-indicator)


; Columna 80
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)
(setq fci-rule-column 80)
(setq fci-rule-width 2)
(setq fci-rule-color "darkred")
; Ver numero columnas
(setq column-number-mode t)
; Num lineas
(global-linum-mode 1)
(show-paren-mode 1)

;; Crear nuevo fichero
;; FILES

(defun my/new-scratch ()
  (interactive)
  (switch-to-buffer (get-buffer-create  (make-temp-name "new-file-"))))
(global-set-key (kbd "C-x C-n") 'my/new-scratch)

;; COPY CURRENT LINE
(defun xah-copy-line-or-region ()
  "Copy current line, or text selection.
When `universal-argument' is called first, copy whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2015-05-06"
  (interactive)
  (let (ξp1 ξp2)
    (if current-prefix-arg
        (progn (setq ξp1 (point-min))
               (setq ξp2 (point-max)))
      (progn (if (use-region-p)
                 (progn (setq ξp1(add-hook 'window-setup-hook 'delete-other-windows) (region-beginning))
                        (setq ξp2 (region-end)))
               (progn (setq ξp1 (line-beginning-position))
                      (setq ξp2 (line-end-position))))))
    (kill-ring-save ξp1 ξp2)
    (if current-prefix-arg
        (message "buffer text copied")
      (message "text copied"))))

(global-set-key (kbd "C-j") 'xah-copy-line-or-region)

;; DUPLICATE LINE
;; ; duplicate line
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)
(global-set-key (kbd "C-x d") 'duplicate-line)

;; BUFFERo
(setq inhibit-startup-buffer-menu t)
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer)))))
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; ELPY
(elpy-enable)
; al acceder a una funcion que no haya tiempo limite
(setq elpy-rpc-timeout nil)


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

 ;; Multiple cursors
 (require 'multiple-cursors)

 (global-set-key (kbd "C-x w") 'mc/mark-next-like-this-word)
 (global-set-key (kbd "C-x n") 'mc/mark-more-like-this-extended)
 (global-set-key (kbd "C->") 'mc/mark-next-like-this)
 (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
 (global-set-key (kbd "M-n") 'mc/edit-beginnings-of-lines)

 (global-set-key (kbd "<f3>") 'mc/mark-all-in-region)
 (global-set-key (kbd "<f4>") 'mc/mark-all-like-this-in-defun)
 (global-set-key (kbd "<f5>") 'mc/edit-ends-of-lines)
 (global-set-key (kbd "<f6>") 'mc/mark-all-like-this)
 (global-set-key (kbd "<f7>") 'set-rectangular-region-anchor)
 (global-set-key (kbd "<f8>") 'mc/edit-lines)

 ;; Buffers
 (global-set-key (kbd "C-x b") 'switch-to-buffer-other-window)
 ;; Makes *scratch* empty.
 (setq initial-scratch-message "")

 ;; Removes *scratch* from buffer after the mode has been set.
 (defun remove-scratch-buffer ()
 (if (get-buffer "*scratch*")
 (kill-buffer "*scratch*")))
 (add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

 ;; Removes *messages* from the buffer.
 (setq-default message-log-max nil)
 (kill-buffer "*Messages*")

 ;; Removes *Completions* from buffer after you've opened a file.
 (add-hook 'minibuffer-exit-hook
 '(lambda ()
 (let ((buffer "*Completions*"))
 (and (get-buffer buffer)
 (kill-buffer buffer)))))

 ;; Don't show *Buffer list* when opening multiple files at the same time.
 (setq inhibit-startup-buffer-menu t)

 ;; Show only one active window when opening multiple files at the same time.
 (add-hook 'window-setup-hook 'delete-other-windows)

 ;; No more typing the whole yes or no. Just y or n will do.
 (fset 'yes-or-no-p 'y-or-n-p)

 (global-set-key "\C-x b" (lambda () (interactive)(ibuffer) (other-window 1)))
 (global-set-key "\C-x C-b" (lambda () (interactive)(ibuffer) (other-window 1)))
 (global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
 (global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))



 (eval-after-load "vc-annotate"
 '(defun vc-annotate-get-time-set-line-props ()
 (let ((bol (point))
 (date (vc-call-backend vc-annotate-backend 'annotate-time))
 (inhibit-read-only t))
 (assert (>= (point) bol))
 (put-text-property bol (point) 'invisible 'vc-annotate-annotation)
 (when (string-equal "Git" vc-annotate-backend)
 (save-excursion
 (goto-char bol)
 (search-forward "(")
 (let ((p1 (point)))
 (re-search-forward " [0-9]")
 (remove-text-properties p1 (1- (point)) '(invisible nil))
 )))
 date)))


 (require 'which-func)
 (which-function-mode t)

 (require 'package) ;; You might already have this line
 (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
 (not (gnutls-available-p))))
 (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
 (add-to-list 'package-archives (cons "melpa" url) t))
 (when (< emacs-major-version 24)
 ;; For important compatibility libraries like cl-lib
 (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
 (package-initialize) ;; You might already have this line


(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)


;; Snippets
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")
(yas-minor-mode 1)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "M-SPC") 'yas-expand)

;; (ac-set-trigger-key "TAB")
;; (define-key yas-minor-mode-map (kbd "SPC") 'yas-expand)
;; (define-key yas-minor-mode-map (kbd "TAB") nil)
