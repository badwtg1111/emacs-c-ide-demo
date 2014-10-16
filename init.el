(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(push '("marmalade" . "http://marmalade-repo.org/packages/")
    package-archives )

(package-initialize)

(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)

(defconst demo-packages
  '(anzu
    company
    duplicate-thing
    ggtags
    helm
    helm-gtags
    helm-swoop
    function-args
    clean-aindent-mode
    comment-dwim-2
    dtrt-indent
    ws-butler
    iedit
    yasnippet
    smartparens
    sml-mode
    projectile
    volatile-highlights
    undo-tree
    zygospore))

(defun install-packages ()
  "Install all required packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package demo-packages)
    (unless (package-installed-p package)
      (package-install package))))

(install-packages)

;; this variables must be set before load helm-gtags
;; you can change to any prefix key of your choice
(setq helm-gtags-prefix-key "\C-cg")

(add-to-list 'load-path "~/.emacs.d/custom")

(require 'setup-helm)
(require 'setup-helm-gtags)
;; (require 'setup-ggtags)
(require 'setup-cedet)
(require 'setup-editing)

(windmove-default-keybindings)

;; function-args
(require 'function-args)
(fa-config-default)
(define-key c-mode-map  [(tab)] 'moo-complete)
(define-key c++-mode-map  [(tab)] 'moo-complete)

;; company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(delete 'company-semantic company-backends)
(define-key c-mode-map  [(control tab)] 'company-complete)
(define-key c++-mode-map  [(control tab)] 'company-complete)

;; company-c-headers
(add-to-list 'company-backends 'company-c-headers)

;; hs-minor-mode for folding source code
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; Available C style:
;; “gnu”: The default style for GNU projects
;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; “stroustrup”: What Stroustrup, the author of C++ used in his book
;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist and Mats Henricson, Ellemtel
;; “linux”: What the Linux developers use for kernel development
;; “python”: What Python developers use for extension modules
;; “java”: The default style for java-mode (see below)
;; “user”: When you want to define your own style
(setq
 c-default-style "linux" ;; set style to "linux"
 )

(global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; set for load-file
(global-set-key (kbd "C-c l") 'load-file)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)

;; Compilation
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

;; setup GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

;; Package: clean-aindent-mode
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

;; Package: dtrt-indent
(require 'dtrt-indent)
(dtrt-indent-mode 1)

;; Package: ws-butler
(require 'ws-butler)
(add-hook 'prog-mode-hook 'ws-butler-mode)

;; Package: yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Package: smartparens
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)

(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

;; Package: projejctile
(require 'projectile)
(projectile-global-mode t)
(setq projectile-enable-caching t)
(global-set-key "\C-cf" 'projectile-find-file)
;(global-set-key (kbd "C-x C-f") 'projectile-find-file-dwim)
(let ((ack_path "~/bin/ack"))
  (if (file-exists-p ack_path)
      (setq ack-and-a-half-executable ack_path)
   )
 )

;; Package zygospore
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes (quote ("57f8801351e8b7677923c9fe547f7e19f38c99b80d68c34da6fa9b94dc6d3297" "5ea20171762b3f9682fbf507ee4b4018ce7b6cc65415fa99799a125f112b2cdb" default)))
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors (quote ("#ecf0f1" "#ecf0f1" "#c0392b")))
 '(safe-local-variable-values (quote ((indent-tabs-mode . true))))
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; helm-cmd-t
(push "~/.emacs.d/elpa/helm-cmd-t-20140828.412" load-path)
(require 'helm-config)
(require 'helm-cmd-t)
(global-set-key (kbd "M-t") 'helm-cmd-t)
(setq helm-ff-lynx-style-map nil helm-input-idle-delay 0.1 helm-idle-delay 0.1)



;; for some ido
(require 'ido)
;; Improved flex matching
(require 'flx-ido)

(setq ido-everywhere nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-file-extensions-order '(".c" ".cpp" ".cxx" ".h" ".hpp" ".java" ".js" ".el" ".xml")
      ido-use-filename-at-point 'guess
      ido-use-faces t
    )
(ido-mode 'buffer)

;; Vertical completion menu
(require 'ido-vertical-mode)
(ido-vertical-mode)

;; IDO support pretty much everwhere, including eclim-java-implement
(require 'ido-ubiquitous)
(ido-ubiquitous)


;; Enhanced M-x
(require 'smex)
(global-set-key (kbd "C-x x") 'smex)
(global-set-key (kbd "C-x C-f") 'ido-find-file)

;; theme monokai from textmate
(load-theme 'monokai t)

;; for shell completion
;(require 'shell-command) (shell-command-completion-mode)




;; 标题栏显示文件路径
(setq frame-title-format
'("%S" (buffer-file-name "%f"
(dired-directory dired-directory "%b"))))

;;设置光标为线条状
(setq-default cursor-type 'bar)

 
;;高亮当前行
(require 'hl-line)
(global-hl-line-mode t)
 
;;在标题栏显示buffer的名字(默认不显示)
;(setq frame-title-format "%b@emacs")


;; bash completion
;;----------------------------------------------------------------------------
;; Venom's Emacs24 Configuration
;; Website : http://venmos.com
;; Contact : me[at]venmos.com
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Enable bash-completion ~/.emacs.d/plugin/emacs-bash-completion
;;----------------------------------------------------------------------------
;(add-to-list 'load-path "~/.emacs.d/plugin/emacs-bash-completion")
(require 'bash-completion)
(bash-completion-setup)

(autoload 'bash-completion-dynamic-complete 
  "bash-completion"
  "BASH completion hook")
(add-hook 'shell-dynamic-complete-functions
  'bash-completion-dynamic-complete)
(add-hook 'shell-command-complete-functions
  'bash-completion-dynamic-complete)

(provide 'init-bash-completion)

;; autojump
(require 'eshell-autojump)

(eval-after-load 'eshell
  '(require 'eshell-autojump nil t))

;;; auto install el-get
;(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;(unless (require 'el-get nil 'noerror)
  ;(with-current-buffer
      ;(url-retrieve-synchronously
       ;"https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    ;(let (el-get-master-branch)
      ;(goto-char (point-max))
      ;(eval-print-last-sexp))))

;(el-get 'sync)

;;;for el-get install some packages
;(setq my-el-get-packages
;'(
    ;color-theme
    ;;; Any package you like
    ;))

;(el-get 'sync my-el-get-packages)

;; for tabbar
(require 'tabbar)
(tabbar-mode)
(define-prefix-command 'lwindow-map)
(global-set-key (kbd "<M-up>") 'tabbar-backward-group)
(global-set-key (kbd "<M-down>") 'tabbar-forward-group)
(global-set-key (kbd "<M-left>") 'tabbar-backward)
(global-set-key (kbd "<M-right>") 'tabbar-forward)

;; for vi mode
(require 'evil)
(evil-mode 1)
(add-hook 'evil-insert-state-entry-hook 'evil-emacs-state)
(define-key evil-emacs-state-map (kbd "C-[") 'evil-normal-state)
(define-key evil-visual-state-map (kbd "i") 'evil-emacs-state)


;;不要生成临时文件
;(setq-default make-backup-files nil)
;不生成 #filename# 临时文件
;(setq auto-save-default nil)

;; store all backup and autosave files in the tmp dir
;(setq backup-directory-alist
      ;`((".*" . ,temporary-file-directory)))
;(setq auto-save-file-name-transforms
      ;`((".*" ,temporary-file-directory t)))
(setq backup-directory-alist
      '((".*" . "~/emacs_tmp")))
(setq auto-save-file-name-transforms
      '((".*" "~/emacs_tmp" t)))

;最大化
(defun my-maximized ()
(interactive)
(x-send-client-message
nil 0 nil "_NET_WM_STATE" 32
'(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
(x-send-client-message
nil 0 nil "_NET_WM_STATE" 32
'(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
)
;启动时最大化
;(my-maximized)

;(require 'sr-speedbar)
;(setq sr-speedbar-right-side nil)
;(setq sr-speedbar-width 25)
;(setq dframe-update-speed t)
;(global-set-key (kbd "<f3>") (lambda()
          ;(interactive)
          ;(sr-speedbar-toggle)))

(require 'dirtree)

(require 'neotree)
(global-set-key [f3] 'neotree-toggle)

(xterm-mouse-mode)

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(require 'helm-projectile)
(helm-projectile-on)

;; highlight indention mode
;(require 'highlight-indentation)
;(set-face-background 'highlight-indentation-face "#e3e3d3")
;(set-face-background 'highlight-indentation-current-column-face "#c3b3b3")
;(add-hook 'c-mode-hook '(highlight-indentatoin-mode t))
;(add-hook 'c-mode-hook '(highlight-indentatoin-current-column-mode t))
;(add-hook 'c++-mode-hook '(highlight-indentatoin-mode t))
;(add-hook 'c++-mode-hook '(highlight-indentatoin-current-column-mode t))
;(add-hook 'java-mode-hook '(highlight-indentatoin-mode t))
;(add-hook 'java-mode-hook '(highlight-indentatoin-current-column-mode t))
;(dolist (hook1 (list
;'c-mode-hook
;'c++-mode-hook
;'emacs-lisp-mode-hook
;'lisp-interaction-mode-hook
;'lisp-mode-hook
;'emms-playlist-mode-hook
;'java-mode-hook
;'asm-mode-hook
;'haskell-mode-hook
;'rcirc-mode-hook
;'emms-lyrics-mode-hook
;'erc-mode-hook
;'sh-mode-hook
;'makefile-gmake-mode-hook
;))
;(add-hook hook1 (lambda () (highlight-indentation-mode 1)))
;(add-hook hook1 (lambda () (highlight-indentation-current-column-mode 1)))
;)

(evilnc-default-hotkeys)

;;在buffer左侧显示行号
(dolist (hook (list
'c-mode-hook
'c++-mode-hook
'emacs-lisp-mode-hook
'lisp-interaction-mode-hook
'lisp-mode-hook
'emms-playlist-mode-hook
'java-mode-hook
'asm-mode-hook
'haskell-mode-hook
'rcirc-mode-hook
'emms-lyrics-mode-hook
'erc-mode-hook
'sh-mode-hook
'makefile-gmake-mode-hook
))
(add-hook hook (lambda () (linum-mode 1))))

;; for git
(require 'magit-gerrit)

;; if remote url is not using the default gerrit port and
;; ssh scheme, need to manually set this variable
;(setq-default magit-gerrit-ssh-creds "chenchunsheng@http://172.16.0.9:8080/")

;; if necessary, use an alternative remote instead of 'origin'
(setq-default magit-gerrit-remote "smartisan")

(global-set-key (kbd "C-SPC") 'nil)
;(global-set-key (kbd "C-..") 'nil)

;(require 'graphene)

(require 'helm-config)
(require 'helm-ack)

(custom-set-variables
 ;; Does not insert '--type' option
 '(helm-ack-base-command "ack -iH")
 '(helm-ack-auto-set-filetype nil)
 ;; Insert "thing-at-point 'symbol" as search pattern
 '(helm-ack-thing-at-point 'symbol))

;; for exec path from shell 

;(exec-path-from-shell-copy-env "PYTHONPATH")
(when (memq window-system '(mac ns))
   (exec-path-from-shell-initialize))
;make shell clickable
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)


(global-set-key (kbd "C-h C-/") 'fasd-find-file)
(global-fasd-mode 1)


;; (setq fasd-completing-read-function nil)


(add-to-list 'load-path "~/.emacs.d/download/fasd-shell")
(require 'fasd-shell)
(fasd-shell-mode t)

(require 'irony)
;(add-hook 'java-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)

(require 'company-irony)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator
;;     std::|
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

(require 'company-c-headers)
(add-to-list 'company-backends 'company-c-headers)

(require 'highlight-symbol)
(global-set-key [(control f4)] 'highlight-symbol-at-point)
(global-set-key [f4] 'highlight-symbol-next)
(global-set-key [(shift f4)] 'highlight-symbol-prev)
(global-set-key [(meta f4)] 'highlight-symbol-query-replace)
