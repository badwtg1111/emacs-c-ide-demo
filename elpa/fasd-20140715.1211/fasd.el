;;; fasd.el --- Emacs integration for the command-line productivity booster `fasd'
;; Version: 20140715.1211

;; Copyright (C) 2013 steckerhalter

;; Author: steckerhalter
;; Package-Requires: ((grizzl "0"))
;; URL: https://github.com/steckerhalter/emacs-fasd
;; Keywords: cli bash zsh autojump

;;; Commentary:

;; Hooks into to `find-file-hook' to add all visited files and directories to `fasd'.
;; Adds the function `fasd-find-file' to prompt and fuzzy complete available candidates

;;; Requirements:

;; `fasd' command line tool, see: https://github.com/clvv/fasd
;; `grizzl' for fuzzy completion

;;; Usage:

;; (require 'fasd)
;; (global-fasd-mode 1)

;; Optionally bind `fasd-find-file' to a key:
;; (global-set-key (kbd "C-h C-/") 'fasd-find-file)

;;; Code:

(require 'grizzl)

(defgroup fasd nil
  "Navigate previously-visited files and directories easily"
  :group 'tools
  :group 'convenience)

(defcustom fasd-enable-initial-prompt t
  "Specify whether to enable prompt for the initial query.

When set to nil, all fasd results are returned for completion")

(defcustom fasd-completing-read-function 'helm-completing-read
  "The completion function to use for `fasd' completion.
Default is `grizzl-completing-read'.  If set to `nil' it will
fall back to the standard `completing-read-function', which could
be using `helm' or `ido' depending on what you are using.  To use
e.g. `ido' explicitly set it to `ido-completing-read'.")

;;;###autoload
(defun fasd-find-file (prefix &optional query)
  "Use fasd to open a file, or a directory with dired.
If PREFIX is non-nil consider only directories.  QUERY can be
passed optionally to avoid the prompt."
  (interactive "P")
  (if (not (executable-find "fasd"))
      (error "Fasd executable cannot be found.  It is required by `fasd.el'.  Cannot use `fasd-find-file'")
    (unless query (setq query (if fasd-enable-initial-prompt
                                  (read-from-minibuffer "Fasd query: ")
                                "")))
    (let* ((prompt "Fasd query: ")
           (grizzlp (equal fasd-completing-read-function 'grizzl-completing-read))
           (results
            (split-string
             (shell-command-to-string
              (concat "fasd -l" (unless grizzlp " -R ") (if prefix " -d " " -a ") query)) "\n" t))
           (file (if (> (length results) 1)
                     (if grizzlp
                         (grizzl-completing-read prompt (grizzl-make-index results))
                       (let ((completing-read-function
                              (or fasd-completing-read-function
                                  completing-read-function)))
                         (completing-read prompt results nil t)))
                   (car results))))
      (if file
          (if (file-readable-p file)
              (if (file-directory-p file)
                  (dired file)
                (find-file file))
            (message "Directory or file `%s' doesn't exist" file))
        (message "Fasd found nothing for query `%s'" query)))))

;;;###autoload
(defun fasd-add-file-to-db ()
  "Add current file or directory to the Fasd database."
  (if (not (executable-find "fasd"))
      (message "Fasd executable cannot be found. It is required by `fasd.el'. Cannot add file/directory to the fasd db")
    (let ((file (if (string= major-mode "dired-mode")
                    dired-directory
                  (buffer-file-name))))
      (start-process "*fasd*" nil "fasd" "--add" file))))

;;;###autoload
(define-minor-mode global-fasd-mode
  "Toggle fasd mode globally.
   With no argument, this command toggles the mode.
   Non-null prefix argument turns on the mode.
   Null prefix argument turns off the mode."
  :global t
  :group 'fasd

  (if global-fasd-mode
      (progn (add-hook 'find-file-hook 'fasd-add-file-to-db)
             (add-hook 'dired-mode-hook 'fasd-add-file-to-db))
    (remove-hook 'find-file-hook 'fasd-add-file-to-db)
    (remove-hook 'dired-mode-hook 'fasd-add-file-to-db))
  )

(provide 'fasd)
;;; fasd.el ends here
