(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(add-to-list 'default-frame-alist '(font . "Mononoki Nerd Font-10" ))

(set-fontset-font "fontset-default" 'han "Noto Serif JP")
(set-fontset-font "fontset-default" 'arabic "Arabic")

(setq package-selected-packages
      '(neotree
		all-the-icons
		all-the-icons-dired
		emmet-mode
		lua-mode
		lsp-mode
		vterm
		counsel
		markdown-mode
		typescript-mode
		company
		which-key
		atom-one-dark-theme
		spacemacs-theme
		doom-themes
		doom-modeline
		angular-mode
		svelte-mode
		slime
		evil
		org-modern
		key-chord
		vertico
		restart-emacs
		centaur-tabs
		tree-sitter
		tree-sitter-langs
		lsp-java))


(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'xml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)

(defun company-predictive (command &optional arg &rest ignored)
  (case command
    (prefix (let* ((text (downcase (word-at-point))))
              (set-text-properties 0 (length text) nil text)
              text))
    (candidates (predictive-complete arg))))

(defun window-half-height ()
  (max 1 (/ (1- (window-height (selected-window))) 2)))

(defun scroll-up-half ()
  (interactive)
  (next-line (window-half-height)))

(defun scroll-down-half ()
  (interactive)
  (previous-line (window-half-height)))

(add-hook 'after-init-hook 'global-company-mode)
(setq company-minimum-prefix-length 1)

;; (ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

(setq-default which-key-idle-delay 0.3)
(which-key-mode)

(set-default 'truncate-lines t)

;; ============ transparency
(set-frame-parameter (selected-frame) 'alpha '(97 . 97))
(add-to-list 'default-frame-alist '(alpha . (97 . 97)))

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-window-fixed-size nil)

(blink-cursor-mode 0)

(doom-modeline-mode 1)

(vertico-mode 1)

;; ============ org mode
(setq
 ;; edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-ellipsis "…"

 ;; agenda styling
 org-agenda-tags-column 0
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 "⭠ now ─────────────────────────────────────────────────")
(with-eval-after-load 'org (global-org-modern-mode))

;; ============ indentation
(setq-default tab-width 4) ; or any other preferred value
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;; ============ default ivy config; don't touch!
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-c M-x") 'counsel-m-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "<f2> j") 'counsel-set-variable)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c v") 'ivy-pop-view)

(global-set-key (kbd "C-c c") 'counsel-compile)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c l") 'counsel-git-log)
(global-set-key (kbd "C-c k") 'counsel-rg)
(global-set-key (kbd "C-c m") 'counsel-linux-app)
(global-set-key (kbd "C-c n") 'counsel-fzf)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-c j") 'counsel-file-jump)
(global-set-key (kbd "C-s-o") 'counsel-rhythmbox)
(global-set-key (kbd "C-c w") 'counsel-wmctrl)

(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-c b") 'counsel-bookmark)
(global-set-key (kbd "C-c d") 'counsel-descbinds)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c o") 'counsel-outline)
(global-set-key (kbd "C-c t") 'counsel-load-theme)
(global-set-key (kbd "C-c f") 'counsel-org-file)

;; ============ disable startup screen
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)

;; ============ key binding
(global-set-key (kbd "C-c \$") 'toggle-truncate-lines)
(global-set-key (kbd "C-c f") 'neotree-toggle)
(global-set-key (kbd "C-c i") 'ivy-mode)

;; ============ centaur tabs
(require 'centaur-tabs)
(centaur-tabs-mode t)
(global-set-key (kbd "C-<prior>")  'centaur-tabs-backward)
(global-set-key (kbd "C-<next>") 'centaur-tabs-forward)
(centaur-tabs-headline-match)
(setq centaur-tabs-style "bar")
(setq centaur-tabs-set-icons t)
(setq centaur-tabs-set-modified-marker t)
(setq centaur-tabs-show-navigation-buttons t)
(setq centaur-tabs-set-bar 'left)
;; (setq x-underline-at-descent-line t)

(key-chord-mode 1)
;; (evil-mode 1)

;; (set-cursor-color "MediumOrchid2")

;; (setq evil-emacs-state-cursor '("orange" box))
;; (setq evil-normal-state-cursor '(box "SteelBlue2")
      ;; evil-insert-state-cursor '(bar "SteelBlue2")
      ;; evil-visual-state-cursor '(box "orange"))

;; (define-key evil-normal-state-map (kbd "C-r") 'undo-redo)
;; (define-key evil-normal-state-map (kbd "C-d") 'scroll-up-half)
;; (define-key evil-normal-state-map (kbd "C-u") 'scroll-down-half)
;; (define-key evil-visual-state-map ">" (lambda ()
;;     (interactive)
;;     ; ensure mark is less than point
;;     (when (> (mark) (point)) 
;;         (exchange-point-and-mark)
;;     )
;;     (evil-normal-state)
;;     (evil-shift-right (mark) (point))
;;     (evil-visual-restore) ; re-select last visual-mode selection
;; ))

;; (define-key evil-visual-state-map "<" (lambda ()
;;     (interactive)
;;     ; ensure mark is less than point
;;     (when (> (mark) (point)) 
;;         (exchange-point-and-mark)
;;     )
;;     (evil-normal-state)
;;     (evil-shift-left (mark) (point))
;;     (evil-visual-restore) ; re-select last visual-mode selection
;; ))
;; ;; (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

(global-set-key [next] 'scroll-up-half)
(global-set-key [prior] 'scroll-down-half)

;; ============ jsp
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . common-lisp-mode))

;; ============ LSP
(require 'lsp-mode)

(add-hook 'lsp-mode-hook #'lsp-headerline-breadcrumb-mode)

(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)

;; ============ Tree Sitter
(global-tree-sitter-mode)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode 1)
;; (global-linum-mode 1)
;; (setq linum-format " %d ")
;; (setq linum-format " %d \u2502")

(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; ============== Custom Config
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-enabled-themes '(spacemacs-dark))
 '(custom-safe-themes
   '("7fd8b914e340283c189980cd1883dbdef67080ad1a3a9cc3df864ca53bdc89cf" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" default))
 '(display-line-numbers-type 'relative)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(global-display-line-numbers-mode t)
 '(menu-bar-mode nil)
 '(org-agenda-files '("~/Programming/Agenda.org"))
 '(size-indication-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "MediumOrchid2")))))
(put 'scroll-left 'disabled nil)
