;; Mouse settings
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq mouse-wheel-progressive-speed nil)

(setq highlight-indentation-mode nil)
(set-face-attribute 'default nil :height 117 :width 'semi-condensed)



;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq make-backup-files nil)

(add-to-list 'load-path "~/.emacs.d/lisp")

(add-to-list 'load-path "~/.emacs.d/lisp/drag-stuff.el-master")
(require 'drag-stuff)
(drag-stuff-mode t)


; Collapse
(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-set-key (kbd "C-c {")  'hs-hide-block)
(global-set-key (kbd "C-c }") 'hs-show-block)
(global-set-key (kbd "C-{")    'hs-hide-all)
(global-set-key (kbd "C-}")  'hs-show-all)

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

(global-set-key (kbd "C-ñ") 'ace-jump-line-mode)


(require 'git)

(autoload 'egit "egit" "Emacs git history" t)
(autoload 'egit-file "egit" "Emacs git history file" t)
(autoload 'egit-dir "egit" "Emacs git history directory" t)
(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)

(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -mjson.tool" (current-buffer) t)))

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

(global-set-key (kbd "<f1>") 'prettyxml)

;;CamelCase
(global-subword-mode 1)


(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

; utf8
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

;; Jedi
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)                 ; optional

;; Fuzzy Search
(setq default-directory "~/roi/bookcore/bookcore/apps/" )
(setq fiplr-root-markers '(".git"))
(setq fiplr-ignored-globs '((directories (".git" "uploads" "autopyxb" "docs" "tmp" "migrations" "locale" "node_modules" "bootstrap" "analytics" "bootstrap-notify" "chosen" "components" "fancybox" "fonts" "imgs" "jqueryui" "select2"))
                            (files ("*.jpg" "*.pyc" "*.po" "*.png" "*.zip" "*~" "*.orig" "*.tmp" "*.md" "*.ini" "*.dia" "*.txt" "*.rst" "*.editorconfig" "*.flake8" "*.coverage" "__init__.py" "*.min.js" "*.gif" "*.GIF" "*.json"))))
(global-set-key (kbd "C-x p") 'fiplr-find-file)

;; blank page when init
(setf inhibit-splash-screen t)
(switch-to-buffer (get-buffer-create "emtpy"))
(delete-other-windows)

;; Guardar ficheros temporales en otra carpeta
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
    (setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

;; Renombrar fichero
(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))
(global-set-key (kbd "C-c r")  'rename-file-and-buffer)

;; Utilidades de copiado de lineas
(defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end)) (message
  "Copied line") (list (line-beginning-position) (line-beginning-position
  2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
            (line-beginning-position 2)))))

(defun copy-word (&optional arg)
      "Copy words at point into kill-ring"
       (interactive "P")
       (copy-thing 'backward-word 'forward-word arg)
       ;;(paste-to-mark arg)
     )

(global-set-key (kbd "C-c w") (quote copy-word))

(global-set-key (kbd "C-c l")
                (quote kill-ring-save))

(global-set-key (kbd "C-x k")
                (quote kill-region))

(global-set-key (kbd "M-;")
                (quote comment-region))

;; Pretty XML
(defun pretty-xml (begin end)
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
(defun my/new-scratch ()
  (interactive)
  (switch-to-buffer (get-buffer-create  (make-temp-name "new-file-"))))
(global-set-key (kbd "C-x C-n") 'my/new-scratch)

;; Repetir like a VIM
(global-set-key (kbd "C-.") 'repeat)

;; customize window
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(setq-default frame-title-format "%b (%f)")

;; Undo tree mode
(require 'undo-tree)
(global-undo-tree-mode)

; undo y redo
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "M-z") 'undo-tree-redo)
(global-set-key (kbd "C-x t") 'undo-tree-visualize)

(setq read-only-mode t)

; copy current line
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
                 (progn (setq ξp1 (region-beginning))
                        (setq ξp2 (region-end)))
               (progn (setq ξp1 (line-beginning-position))
                      (setq ξp2 (line-end-position))))))
    (kill-ring-save ξp1 ξp2)
    (if current-prefix-arg
        (message "buffer text copied")
      (message "text copied"))))

(global-set-key (kbd "C-j") 'xah-copy-line-or-region)

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
(global-set-key (kbd "C-x C-a") 'pop-global-mark)

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/themes")
(load-theme 'solarized t)
(setf inhibit-splash-screen t)
(switch-to-buffer (get-buffer-create "emtpy"))
(delete-other-windows)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-sane-defaults)))
 '(undo-tree-visualizer-diff t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

 ;; Multiple cursors
 (require 'multiple-cursors)

 (global-set-key (kbd "C-;") 'set-rectangular-region-anchor)
 (global-set-key (kbd "M-s n") 'mc/mark-all-words-like-this-in-defun)
 (global-set-key (kbd "M-s M-n") 'mc/mark-all-words-like-this)
 (global-set-key (kbd "M-n") 'mc/mark-next-like-this-word)
 (global-set-key (kbd "M-p") 'mc/mark-previous-like-this-word)
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
