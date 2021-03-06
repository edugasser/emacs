;; NOTES
; C-c C-o show def/class
; C-u C-SPACE mark ring previous
; C-x C-x return last ring
; C-SPC set mark
; C-x C-f /ssh:tron@ovhtron:/
; C-x SPC seleccionar rectangulo
; M-x profiler-start | pofiler-report
; M-r anaconda-mode-find-references
; m-, anaconda-mode-find-assignments
; pip install flake8!
; alias ..="cd .." \
;      ...="cd ../.." \
;      ....="cd ../../.." \
;      .....="cd ../../../.."

(load "package")
(package-initialize)
             '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(setq package-archive-enable-alist '(("melpa" deft magit)))


(add-to-list 'package-archives


;; DEFINE PACKAGES
(defvar gasser/packages '(ace-jump-mode
                          super-save
                          dockerfile-mode
                          expand-region
                          smartscan
                          real-auto-save
                          elpy
                          fill-column-indicator
                          fiplr
                          flycheck
                          helm
                          helm-swoop
                          htmlize
                          importmagic
                          jedi
                          json-mode
                          magit
                          git-timemachine
                          marmalade
                          multiple-cursors
                          keyfreq
                          php-mode
                          puppet-mode
                          pyvenv
                          py-autopep8
                          pylint
                          all-the-icons ;; REMBEMBER M-x all-the-icons-install-fonts
                          nlinum
                          js2-refactor
                          xref-js2
                          floobits
                          neotree
                          doom-themes
                          crux
                          js2-mode
                          sphinx-doc
                          git-gutter
                          spaceline
                          projectile
                          auto-complete
                          anaconda-mode
                          sr-speedbar
                          autopair
                          clojure-mode
                          coffee-mode
                          counsel
                          drag-stuff
                          solarized-theme
                          vimish-fold
                          swiper
                          web-mode
                          writegood-mode
                          undo-tree
                          yaml-mode
                          yasnippet
                          dumb-jump
                          virtualenvwrapper
                          xcscope
                          helm-cscope
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
(setq inhibit-splash-screen t
      initial-scratch-message nil)
(switch-to-buffer (get-buffer-create "emtpy"))
(delete-other-windows)

(add-to-list 'load-path "~/.emacs.d/lisp")

;; Jump to definition
(dumb-jump-mode 1)

;; YASSNIPPETS
(yas-global-mode 1)

;; Smartscan
(smartscan-mode 1)

(setq auto-save-default t)
(setq auto-save-visited-file-name t)

;; SPHINX docstring
; usage C-c M-d
(add-hook 'python-mode-hook (lambda ()
                                  (require 'sphinx-doc)
                                  (sphinx-doc-mode t)))
;; APPEARANCE
(toggle-scroll-bar -1)
(setq powerline-default-separator 'arrow-fade)
(require 'spaceline-config)
(spaceline-spacemacs-theme)


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
(set-face-attribute 'default nil :height 117 :width 'normal)
;;; menu-set-font !
;(set-frame-font "Droid Sans Mono-10.8" nil t)
;(set-frame-font "Droid Sans Mono-10.8" nil t)
;(set-frame-font "DejaVu Sans Mono-10" nil t)
;(set-frame-font "Source Code Pro-10" nil t)
;(set-frame-font "Operator Mono Light-13" nil t)
;(set-frame-font "Inconsolata-11" t t)
;(set-frame-font "Consolas" t t)


(tool-bar-mode -1)
(menu-bar-mode -1)
(setq column-number-mode t)

;; Solarized them
(if window-system
    (load-theme 'solarized-dark t)
  (load-theme 'wombat t))

;; MARKING TEXT
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; DISPLAY SETTINGS
(global-display-line-numbers-mode t)

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

(add-hook 'before-save-hook 'whitespace-cleanup)

; Camel Case
(global-subword-mode nil)

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
(setq default-directory "~/roi/bookcore/bookcore" )
(setq fiplr-root-markers '(".hg" ".git"))
(setq fiplr-ignored-globs '((directories (".git" "uploads" "autopyxb" "tmp" "docs" "locale" "node_modules"))
                            (files ("*.jpg" "*.pyc" "*.xml" "*.po" "*.png" "*.zip" "*~" "*.xsd" "*.min.js" "*.xls" "*.log" "*.md" "*.dia" "*.diff" "*.txt"))))
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

(global-set-key (kbd "C-;") 'mc/edit-lines)
(global-set-key (kbd "M-s n") 'mc/mark-all-words-like-this-in-defun)
(global-set-key (kbd "M-s M-n") 'mc/mark-all-words-like-this)
(global-set-key (kbd "C-?") 'mc/mark-next-like-this-word)
(global-set-key (kbd "C-¿") 'mc/mark-previous-like-this-word)
(global-set-key (kbd "C-=") 'mc/skip-to-next-like-this)
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

;; ;; PEP 8
;; (require 'py-autopep8)
;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

;; ;; FLYCHECK
;; (require 'flycheck)
;; (global-flycheck-mode t)

;; JEDI
(require 'jedi)

(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'jedi:ac-setup)
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
(global-set-key (kbd "C-M-d") 'duplicate-line)
(global-set-key (kbd "C-x SPC") 'rectangle-mark-mode)

;; SWIPER
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;(global-set-key "\C-s" 'swiper)
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
(global-set-key (kbd "C-¡") 'my-hs-toggle-all)
(global-set-key (kbd "C-'") 'hs-hide-level)
(global-set-key (kbd "C-0") 'hs-toggle-hiding)

(defun my-hs-toggle-all ()
  "If anything isn't hidden, run `hs-hide-all', else run `hs-show-all'."
  (interactive)
  (let ((starting-ov-count (length (overlays-in (point-min) (point-max)))))
    (hs-hide-all)
    (when (equal (length (overlays-in (point-min) (point-max))) starting-ov-count)
      (hs-show-all))))


(elpy-enable)
(setq elpy-rpc-timeout nil)

;; AUTO GENERATE
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "100e7c5956d7bb3fd0eebff57fde6de8f3b9fafa056a2519f169f85199cc1c96" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(package-selected-packages
   (quote
    (dart-mode pyenv-mode realgud rope-read-mode jedi ace-window flymake-python-pyflakes yasnippet-snippets hydra yaml-mode writegood-mode web-mode solarized-theme puppet-mode php-mode marmalade magit htmlize flycheck coffee-mode clojure-mode autopair auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "#073642" :weight light :height 1.0 :width normal)))))

;; DRAG-STUFF (move lines)
(require 'drag-stuff)
(drag-stuff-global-mode 1)
(global-set-key (kbd "M-p") 'drag-stuff-up)
(global-set-key (kbd "M-n") 'drag-stuff-down)

;; Highlight the line we are currently on
(global-hl-line-mode t)
(delete 'elpy-module-highlight-indentation elpy-modules)

(put 'scroll-left 'disabled nil)



;; Neo tree
(global-set-key [f8] 'neotree-toggle)
(global-set-key [f6] 'git-timemachine)

;;EMACS so slow? Adjust the idle delay before which eldoc ask for documentation under point with:
(setq eldoc-idle-delay 1)  ;; in second

;; Behave like vi's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

(global-set-key (kbd "C-o") 'open-next-line)

(defun open-previous-line (arg)
  "1Open a new line before the current one.
     See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))

(global-set-key (kbd "M-o") 'open-previous-line)
(global-set-key (kbd "C-M-k") 'kill-whole-line)
(global-set-key (kbd "M-ñ") 'er/expand-region)
(global-set-key (kbd "M-l") 'er/mark-inside-quotes)

;; autoindent open-*-lines
(defvar newline-and-indent t
  "modify the behavior of the open-*-line functions to cause them to autoindent.")

;; js framework
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(require 'js2-refactor)
(require 'xref-js2)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.

(add-hook 'js2-mode-hook (lambda ()
                           (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

;; higlight changes in documents
(global-highlight-changes-mode t)
(setq highlight-changes-visibility-initial-state nil); initially hide
;; toggle visibility
(global-set-key (kbd "<f6>")      'highlight-changes-visible-mode) ;; changes
;; remove the change-highlight in region
(global-set-key (kbd "S-<f6>")    'highlight-changes-remove-highlight)
;; if you're not already using it for something else...
(global-set-key (kbd "<M-prior>") 'highlight-changes-next-change)
(global-set-key (kbd "<M-next>")  'highlight-changes-previous-change)
(set-face-foreground 'highlight-changes nil)
(set-face-background 'highlight-changes "#382f2f")
(set-face-foreground 'highlight-changes-delete nil)
(set-face-background 'highlight-changes-delete "#916868")

;; Movement
(global-set-key (kbd "C-h")  'move-to-window-line-top-bottom)

(global-set-key (kbd "<f12>") 'kill-some-buffers)
(global-set-key (kbd "<f9>") 'magit-blame)
(global-set-key (kbd "<f10>") 'magit-blame-quit)


(defun selection-or-thing-at-point ()
  (cond
   ;; If there is selection use it
   ((and transient-mark-mode
         mark-active
         (not (eq (mark) (point))))
    (let ((mark-saved (mark))
          (point-saved (point)))
      (deactivate-mark)
      (buffer-substring-no-properties mark-saved point-saved)))
   ;; Otherwise, use symbol at point or empty
   (t (format "%s"
              (or (thing-at-point 'symbol)
                  "")))))

;; https://github.com/abo-abo/swiper-helm.git
(require 'swiper-helm)
(defun swiper-helm-at-point ()
    "Custom function to pick up a thing at a point for swiper-helm
    If a selected region exists, it will be searched for by swiper-helm
    If there is a symbol at the current point, its textual representation is
    searched. If there is no symbol, empty search box is started."
    (interactive)
    (swiper-helm (selection-or-thing-at-point)))

(global-set-key (kbd "M-s ,") 'helm-swoop)

;; Grep at symbol or selection point
(defun grepme ()
   (interactive)
   (counsel-git-grep nil
                     (selection-or-thing-at-point)))

(global-set-key (kbd "M-¿") 'grepme)
(electric-pair-mode 1)
(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?\{ . ?\})
                            ) )

;; Emacs slow
(setq history-length 100)
(put 'minibuffer-history 'history-length 50)
(put 'evil-ex-history 'history-length 50)
(put 'kill-ring 'history-length 25)
;; I want to see at most the first 4 errors for a line.
(setq flymake-number-of-errors-to-display 4)
;; Let's run 8 checks at once instead.
(setq flymake-max-parallel-syntax-checks 8)
(setq vc-handled-backends nil)
(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)
(setq auto-window-vscroll nil)
(global-company-mode nil)
(desktop-save-mode 1)
(setq mouse-drag-and-drop-region t)

;; Go back buffers
(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "C-c b") 'switch-to-previous-buffer)

;; auto-save
;(auto-save-visited-mode 1)
(super-save-mode +1)
(setq super-save-auto-save-when-idle t)

;; Show the current function name in the header line
;; (which-function-mode)
;;             ;; We remove Which Function Mode from the mode line, because it's mostly
;;             ;; invisible here anyway.
;;             (assq-delete-all 'which-func-mode mode-line-misc-info))
(global-auto-revert-mode nil)
(global-set-key (kbd "C-S-n") 'smartscan-symbol-go-forward)
(global-set-key (kbd "C-S-p") 'smartscan-symbol-go-backward)
(global-set-key (kbd "C-S-'") 'smartscan-symbol-replace)

;; Activate bookcore virtualenv
(pyvenv-activate "~/virtualenvs/bookcore/")

(cscope-minor-mode 1)
(global-set-key (kbd "M-t") 'cscope-find-global-definition-no-prompting)

(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

;; Copy current file buffer
(global-unset-key (kbd "M-<down-mouse-3>"))
(global-set-key (kbd "M-<mouse-3>") 'my-put-file-name-on-clipboard)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;;By default Emacs doesn't read from the same environment variables set in your terminal.

(global-git-gutter-mode +1)
(add-hook 'python-mode-hook 'git-gutter-mode)

(global-set-key (kbd "C-a") 'crux-move-beginning-of-line)
(global-set-key (kbd "C-c h") 'find-grep-dired)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
