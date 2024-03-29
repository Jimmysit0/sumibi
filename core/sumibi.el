;;; sumibi -*- lexical-binding: t -*-

; Setup debug mode
(eval-and-compile
  (defvar sumibi-debug-mode
    (or (getenv "DEBUG") init-file-debug)
    "Debug mode, enable through DEBUG=1 or use --debug-init.")
  (setq debug-on-error (and (not noninteractive) sumibi-debug-mode)
        jka-compr-verbose sumibi-debug-mode))

(defvar sumibi-verbose-byte-compile-warnings nil)

;; Disable certain byte compiler warnings to cut down on the noise.
(if (or sumibi-debug-mode sumibi-verbose-byte-compile-warnings)
    (setq byte-compile-warnings t)
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)))

;; Startup & package manager
(defvar file-name-handler-alist-old file-name-handler-alist)

(setq file-name-handler-alist nil
      byte-compile--use-old-handlers nil
      load-prefer-newer t
      ;; Don't load site packages
      site-run-file nil)

;; Advanced logging
;; (setq-default message-log-max 16384)


;; Ensure `sumibi' is in `load-path'
(add-to-list 'load-path (file-name-directory load-file-name))

(autoload #'straight-x-pull-all "straight-x" "" t)
(autoload #'straight-x-fetch-all "straight-x" "" t)
(autoload #'straight-x-freeze-versions "straight-x" "" t)

;; Require necessary sumibi things
(require 'sumibi-core)

;; Straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Straight settings
(setq straight-check-for-modifications nil
      ;; Don't clone the whole repo
      straight-vc-git-default-clone-depth 1
      straight-recipes-emacsmirror-use-mirror t

      ;; Configure build directory considering Emacs version
      ;; straight-build-dir (format "build-%s" emacs-version)

      ;; We have it in early-init.el
      straight-enable-package-integration nil
      )

;; Security settings
;; Emacs is essentially one huge security vulnerability, what with all the
;; dependencies it pulls in from all corners of the globe. Let's try to be at
;; least a little more discerning.
(setq gnutls-verify-error (not (getenv-internal "INSECURE"))
      gnutls-algorithm-priority
      (when (boundp 'libgnutls-version)
        (concat "SECURE128:+SECURE192:-VERS-ALL"
                (if (and (not (version< emacs-version "26.3"))
                         (>= libgnutls-version 30605))
                    ":+VERS-TLS1.3")
                ":+VERS-TLS1.2"))
      ;; `gnutls-min-prime-bits' is set based on recommendations from
      ;; https://www.keylength.com/en/4/
      gnutls-min-prime-bits 3072
      tls-checktrust gnutls-verify-error
  ;; Emacs is built with `gnutls' by default, so `tls-program' would not be
  ;; used in that case. Otherwise, people have reasons to not go with
  ;; `gnutls', we use `openssl' instead. For more details, see
  ;; https://redd.it/8sykl1
      tls-program '("openssl s_client -connect %h:%p -CAfile %t -nbio -no_ssl3 -no_tls1 -no_tls1_1 -ign_eof"
                    "gnutls-cli -p %p --dh-bits=3072 --ocsp --x509cafile=%t \
--strict-tofu --priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:+VERS-TLS1.3' %h"
                    ;; compatibility fallbacks
                    "gnutls-cli -p %p %h"))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t
      use-package-enable-imenu-support t)

;; Display use-package debug stuff when debug-on-error is t
(if sumibi-debug-mode
    (setq use-package-expand-minimally nil
          use-package-verbose t
          use-package-compute-statistics t
          message-log-max t)
  (setq use-package-expand-minimally t
        use-package-verbose nil))


(use-package gcmh
  :hook (after-init . gcmh-mode)
  :config
  (setq gcmh-verbose             nil
        gcmh-idle-delay          5 ;; old is 300
        ;; Don’t compact font caches during GC.
        inhibit-compacting-font-caches t
        gc-cons-percentage 0.1))

(use-package subr-x
  :straight nil
  :defer t)

;; Define directories
(eval-and-compile
  (defvar home-directory (getenv "HOME")
    "User $HOME.")

  (defvar sumibi-root (file-truename user-emacs-directory)
    "Root of the sumibi.")

  (defvar sumibi-dir (concat sumibi-root "core/")
    "The main directory of sumibi-emacs configuration."))

;; Don't load any other files besides this config
(setq inhibit-default-init t
      ;; Set initial mode to text-mode instead of elisp
      initial-major-mode 'fundamental-mode)

;; Use Common Lisp library
(use-package cl-lib :defer t)

;; Add configuration directories to `load-path'
(setq load-path (append '("~/.emacs.d/core/"
            		      "~/.emacs.d/visuals/"
                          "~/.emacs.d/packages"
		                  "~/.emacs.d/user")
                        load-path))

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless *IS-MAC*   (setq command-line-ns-option-alist nil))
(unless *IS-LINUX* (setq command-line-x-option-alist nil))

;; Custom file
(setq custom-file (concat user-emacs-directory "/visuals/custom.el"))
(when (file-exists-p custom-file)
  (load custom-file t t))

;; Here we go
(defmacro sumibi/init (&rest body)
  (declare (indent defun))
  (let ((gc-cons-threshold most-positive-fixnum))
    (add-to-list 'body 'env-fun t)
    (dolist (pkg body)
      (require pkg nil t))))

(require 'sumibi-settings)

(provide 'sumibi)
