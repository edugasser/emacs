(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-archive-enable-alist '(("melpa" deft magit)))

;; DEFINE PACKAGES
(defvar gasser/packages '(ace-jump-mode
                          spacemacs-theme
                          sphinx-doc
                          spaceline
                          auto-complete
                          anaconda-mode
                          autopair
                          clojure-mode
                          coffee-mode
                          counsel
                          drag-stuff
                          elpy
                          fill-column-indicator
                          fiplr
                          flycheck
                          helm
                          htmlize
                          importmagic
                          jedi
                          json-mode
                          magit
                          marmalade
                          multiple-cursors
                          keyfreq
                          php-mode
                          puppet-mode
                          py-autopep8
                          pylint
                          solarized-theme
                          swiper
                          web-mode
                          writegood-mode
                          yaml-mode
                          yasnippet
                          yasnippet-snippets)
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
(which-function-mode 1)
(setq inhibit-splash-screen t
      initial-scratch-message nil)
(switch-to-buffer (get-buffer-create "emtpy"))
(delete-other-windows)

;; Key Freq
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;; YASSNIPPETS
(yas-global-mode 1)


;; SPHINX docstring
; usage C-c M-d
(add-hook 'python-mode-hook (lambda ()
                                  (require 'sphinx-doc)
                                  (sphinx-doc-mode t)))
;; APPEARANCE
(toggle-scroll-bar -1)

;; Highlight some keywords in prog-mode
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\<\\(FIXME\\|TODO\\|NOTE\\|BUG\\|DONE\\)" 1 font-lock-warning-face t)
               )
             )
            )
          )

;font										;
(set-face-attribute 'default nil :height 117 :width 'semi-condensed)
;; set font for all windows. keep window size fixed
;(set-frame-font "DejaVu Sans Mono-10" nil t)
;(set-frame-font "Source Code Pro-10" nil t)
;(set-frame-font "Inconsolata-11" t t)


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
; Camel Case
(global-subword-mode 1)

;; UTF8
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8)

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
;; Guardar ficheros temporales en otra carpeta
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

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

(global-set-key (kbd "C-;") 'set-rectangular-region-anchor)
(global-set-key (kbd "M-s n") 'mc/mark-all-words-like-this-in-defun)
(global-set-key (kbd "M-s M-n") 'mc/mark-all-words-like-this)
(global-set-key (kbd "C-?") 'mc/mark-next-like-this-word)
(global-set-key (kbd "C-¿") 'mc/mark-previous-like-this-word)
(global-set-key (kbd "<f8>") 'mc/edit-lines)

;; ;; PEP 8
;; (require 'py-autopep8)
;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

;; ;; FLYCHECK
;; (require 'flycheck)
;; (global-flycheck-mode t)

;; IMPORT MAGIC
(require 'importmagic)
(add-hook 'python-mode-hook 'importmagic-mode)

;; JEDI
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'jedi:ac-setup)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)                 ; optional


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

(global-set-key (kbd "<f1>") 'prettyxml)

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
(global-set-key (kbd "C-x SPC") 'rectangle-mark-mode)

;; SWIPER
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c j") 'counsel-git-grep)

;; JSON
(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -mjson.tool" (current-buffer) t)))

;; MOUSE
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq mouse-wheel-progressive-speed nil)

;; BUFFER
(global-set-key [f4] 'kill-this-buffer)   ;; Close the current buffer
; Move focus to new buffer
(define-key global-map [remap list-buffers] 'buffer-menu-other-window)
(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

(setq inhibit-startup-buffer-menu t)
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer)))))
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; COLLAPSE
(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-set-key (kbd "C-c }")  'hs-hide-block)
(global-set-key (kbd "C-c {") 'hs-show-block)
(global-set-key (kbd "C-}")    'hs-hide-all)
(global-set-key (kbd "C-{")  'hs-show-all)


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
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(package-selected-packages
   (quote
    (yasnippet-snippets hydra yaml-mode writegood-mode web-mode solarized-theme puppet-mode php-mode marmalade magit htmlize flycheck coffee-mode clojure-mode autopair auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; DRAG-STUFF (move lines)
(require 'drag-stuff)
(drag-stuff-global-mode 1)
(global-set-key (kbd "M-p") 'drag-stuff-up)
(global-set-key (kbd "M-n") 'drag-stuff-down)

;; Highlight the line we are currently on
(global-hl-line-mode t)
(delete 'elpy-module-highlight-indentation elpy-modules)

(setq default-directory "~/roi/bookcore/" )

(put 'scroll-left 'disabled nil)

(add-to-list 'load-path "~/.emacs.d/lisp")

(load-theme 'spacemacs-dark t)
(setq spacemacs-theme-org-agenda-height nil)
(setq spacemacs-theme-org-height nil)


(setq powerline-default-separator 'arrow-fade)
(require 'spaceline-config)
(spaceline-spacemacs-theme)


;; NOTES
; C-u C-SPACE mark ring previous
; C-x C-x return last ring
; C-SPC set mark
; C-x C-f /ssh:tron@ovhtron:/
; C-x SPC seleccionar rectangulo
