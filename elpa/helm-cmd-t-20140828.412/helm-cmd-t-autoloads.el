;;; helm-cmd-t-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (helm-cmd-t-grep helm-cmd-t-repos helm-cmd-t) "helm-cmd-t"
;;;;;;  "helm-cmd-t.el" (21558 23237 257003 336000))
;;; Generated autoloads from helm-cmd-t.el

(autoload 'helm-cmd-t "helm-cmd-t" "\
Choose file from current repo.

With prefix arg C-u, run `helm-cmd-t-repos'.

\(fn &optional ARG)" t nil)

(autoload 'helm-cmd-t-repos "helm-cmd-t" "\
Manage helm-cmd-t caches.

\(fn &optional PRESELECT-ROOT)" t nil)

(autoload 'helm-cmd-t-grep "helm-cmd-t" "\
Grep action run from `helm-cmd-t-repos'.

Redirect to the correct grep based on `candidate-buffer'.

\(fn CANDIDATE-BUFFER &optional GLOBS)" t nil)

;;;***

;;;### (autoloads nil nil ("helm-C-x-b.el" "helm-cmd-t-find.el" "helm-cmd-t-pkg.el")
;;;;;;  (21558 23237 296824 407000))

;;;***

(provide 'helm-cmd-t-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-cmd-t-autoloads.el ends here
