;;; straight-ui.el --- Some description. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; URL:
;; Keywords:
;; Package-Requires ((emacs "27.0.50"))
;; Package-Version: 20190708
;; Version: 0.1.0


;;; Commentary:
;;

;;; Code:

(require 'pkg-info)
(require 'straight)
(require 'straight-x)

(defun straight-ui--get-package-version (pkg)
  ""
  (or (ignore-errors (pkg-info-version-info pkg))
      "unknown"))

(defun straight-ui--create-entry (data)
  ""
  (when-let* ((pkg (map-elt data :package))
              (ver (straight-ui--get-package-version pkg))
              (upstream "-"))
    `[,pkg ,ver ,upstream]))

(defvar straight-ui--pkg-cache () "")

(defun straight-ui--add-pkg-to-cache (pkg)
  ""
  (add-to-list 'straight-ui--pkg-cache pkg 'append))

(defun straight-ui--update-pkg-cache-async ()
  ""
  (setq straight-ui--pkg-cache nil)     ;empty cache first


  (let ((pkgs (straight-x-existing-repos)))
    (message (format "fetching %s packages..." (length pkgs)))
    (dolist (p (-partition-all 31 pkgs))
      (async-start
       `(lambda ()
          ,(async-inject-variables "^load-path$")
          (require 'straight-ui)
          (require 'dash)
          (require 'dash-functional)

          (-map #'straight-ui--create-entry ',p))
       `(lambda (pkg)
          ,(async-inject-variables "^load-path$")
          (require 'straight-ui)

          (dolist (p pkg)
            (straight-ui--add-pkg-to-cache p))

          (straight-ui-refresh))))))

(defun straight-ui--refresh ()
  ""
  (setq tabulated-list-entries nil)
  (let ((idx 0)
        (packages straight-ui--pkg-cache))
    (dolist (p packages)
      (add-to-list 'tabulated-list-entries `(,idx ,p) t)
      (cl-incf idx))))

(defun straight-ui--revert-buffer ()
  ""
  (interactive)
  (revert-buffer))

(defun straight-ui-refresh-packages ()
  ""
  (interactive)
  (straight-ui--update-pkg-cache-async))

(defun straight-ui-refresh ()
  ""
  (interactive)
  (when-let ((buf (get-buffer "*straight-ui*")))
    (with-current-buffer buf
      (straight-ui--refresh)
      (straight-ui--revert-buffer))))

(defun straight-ui-pull-package-at-point ()
  ""
  (interactive)
  (let* ((entry (tabulated-list-get-entry))
         (pkg (aref entry 0)))
    (straight-pull-package pkg t)
    (message "pulled %s" pkg)))

(defun straight-ui-fetch-package-at-point ()
  ""
  (interactive)
  (let* ((entry (tabulated-list-get-entry))
         (pkg (aref entry 0)))
    (straight-fetch-package pkg t)
    (message "fetched %s" pkg)))

;;;; The mode

(defvar straight-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "g") #'straight-ui-refresh)
    (define-key map (kbd "u") #'straight-ui-refresh)
    (define-key map (kbd "U") #'straight-ui-refresh-packages)

    (define-key map (kbd "p") #'straight-ui-pull-package-at-point)
    (define-key map (kbd "P") #'straight-pull-all)
    (define-key map (kbd "f") #'straight-ui-fetch-package-at-point)
    (define-key map (kbd "F") #'straight-fetch-all)
    map)
  "Local keymap for `straigh-ui-mode' buffers.")

(define-derived-mode straight-ui-mode
  tabulated-list-mode "straight-ui"
  "UI for `Straight.el', based on `tabulated-list-mode'."
  (setq tabulated-list-format [("Name" 50 t) ("Version" 15 t) ("Upstream" 15 nil)])
  (add-to-list 'tabulated-list-entries '(0 ["loading packages..." "" ""]) t)

  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header)

  (straight-ui-refresh))

;;;###autoload
(defun straight-ui ()
  "Open the `notmuch-mojn' mailbox."
  (interactive)
  (switch-to-buffer "*straight-ui*" nil)
  (straight-ui-mode)

  (straight-ui--update-pkg-cache-async)
  (straight-ui-refresh)

  (tabulated-list-print t))


(provide 'straight-ui)
;;; straight-ui.el ends here
