(add-to-list 'load-path "/home/edu/.emacs.d/lisp")
(require 'web-mode)
(autoload 'php-mode "php-mode" "Major mode for editing PHP code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(defun xah-syntax-color-hex ()
  "Syntax color text of the form 「#ff1100」 in current buffer.
URL `http://ergoemacs.org/emacs/emacs_CSS_colors.html'
Version 2015-06-11"
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[abcdef[:digit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))))
  (font-lock-fontify-buffer))

(add-hook 'css-mode-hook 'xah-syntax-color-hex)
(add-hook 'php-mode-hook 'xah-syntax-color-hex)
(add-hook 'html-mode-hook 'xah-syntax-color-hex)

(toggle-scroll-bar -1)

(set-face-attribute 'default nil :height 120)
;(global-set-key (kbd "C-i") 'kill-whole-line)
;; auto close bracket insertion. New in emacs 24
;(electric-pair-mode 1)
(setq show-paren-style 'parenthesis)
(setq show-paren-stynle 'expression) ; highlight entire
(setq show-paren-style 'mixed) ; highlight brackets if visible, else entire expression
;; turn on highlight matching brackets when cursor is on one
(show-paren-mode 1)

(global-hi-lock-mode 1)
(set-face-attribute 'default nil :height 118)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq c-default-style "linux")
(setq c-basic-offset 4)
(c-set-offset 'comment-intro 0)
(add-to-list 'load-path "/home/edu/")

;; Disable loading of “default.el” at startup,
;; in Fedora all it does is fix window title which I rather configure differently
(setq inhibit-default-init t)

;; SHOW FILE PATH IN FRAME TITLE
(setq-default frame-title-format "%b (%f)")

; Ver numero columnas
(setq column-number-mode t)
; Fordward word
(global-set-key (kbd "C-q") 'forward-word)


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

; duplicate line
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

; undo y redo
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-r") 'redo)

; anaconda
(add-hook 'python-mode-hook 'anaconda-mode)

; utf8
(set-language-environment 'utf-8)
; no backup
(setq backup-directory-alist `(("." . "~/.saves")))
;Jedi - autocomplete for python
;(add-hook 'python-mode-hook 'jedi:setup)
;(setq jedi:complete-on-dot t)

; Num lineas
(global-linum-mode 1)

; Whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;(global-whitespace-mode 1)

;; Fuzzy Search
(global-set-key (kbd "C-x i") 'helm-locate)
;(global-set-key (kbd "C-x p") 'helm-projectile)
(add-to-list 'load-path "/home/edu/helm/")
(require 'helm-config)

;;;; Tab settings ;;;;
(setq standard-indent 4)
;; Tab with

(setq-default tab-width 4)
;; Tab width is 4
(setq tab-width 4)
;; Use spaces always.
(setq indent-tabs-mode nil)

;(setq-default tab-always-indent t)
;Jump by 4.
(setq c-basic-offset 4)
;this defaulted to 4 and had to be reset to 3.
(setq perl-indent-level 4)
;Manually set by x4
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))

;; melpa packages
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

(require 'helm-ls-hg)

(add-to-list 'load-path "/home/edu/grizzl")
(require 'projectile)
(require 'grizzl)
(setq projectile-indexing-method 'native)
;(setq projectile-completion-system 'grizzl)
(add-to-list 'projectile-globally-ignored-files "*.pyc" "*#")
(add-to-list 'projectile-globally-ignored-directories ".*")
(projectile-global-mode)

;; Fuzzy mejorado 2
(setq fiplr-root-markers '(".hg"))
(setq fiplr-ignored-globs '((directories (".hg" "uploads"))
                            (files ("*.jpg" "*.pyc" "*.po" "*.png" "*.zip" "*~"))))
(global-set-key (kbd "C-x p") 'fiplr-find-file)

;; customize window
(menu-bar-mode -1)
(tool-bar-mode -1)

;; blank page when init
(setf inhibit-splash-screen t)
(switch-to-buffer (get-buffer-create "emtpy"))
(delete-other-windows)

(require 'fill-column-indicator)
(setq fci-rule-width 1)
(setq fci-rule-color "darkblue")


;; theme solarized
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
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
    ("c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(safe-local-variable-values (quote ((encoding . utf-8)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
    (setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

;(add-hook 'python-mode-hook 'jedi:setup)
;(setq jedi:complete-on-dot t)                 ; optional
;(autoload 'jedi:setup "jedi" nil t)

(package-initialize)
(put 'set-goal-column 'disabled nil)

(defun bf-pretty-print-xml-region (begin end)
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

(add-hook 'prog-mode-hook 'subword-mode)




;; (global-set-key (kbd "C-x b") 'helm-buffers-list)
;; (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
;; (global-set-key (kbd "C-x r l") 'helm-bookmarks)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)

;;
;; ace jump mode major function
;;
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)



;;
;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

(global-set-key (kbd "C-ñ") 'ace-jump-line-mode)

;; buffer-move
(global-set-key (kbd "C-c C-p")    'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c w") 'mc/mark-next-like-this)
