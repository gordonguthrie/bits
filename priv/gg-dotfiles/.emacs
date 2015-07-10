
;; make emacs work under ssh from Mac by switching
;; off international key entries and letting
;; ALT come through as META
(set-keyboard-coding-system nil)

;; for when you have 2 files in different paths with
;; the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Enable modes
(ido-mode 1)
(line-number-mode 1)
(column-number-mode 1)

;; make [CRTL][k] delete a line forward 
;; and [CTRL][SHIFT][Backspace] backward
(setq kill-whole-line t)

;; Set the font
;;(set-face-attribute 'default nil :height 90)
(set-face-background 'mode-line "black")
(set-face-foreground 'mode-line "white")

;; open a single window only (No startup)
(setq inhibit-startup-message 0)

;; Use clipboard
(setq x-select-enable-clipboard t) 
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; setup all the Erlang things
(setq load-path (cons "/usr/local/lib/erlang/lib/tools-2.6.12/emacs" load-path))
(require 'erlang-start)
(add-to-list 'auto-mode-alist '("\\.escript?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.yrl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.xrl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.part?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.config?$" . erlang-mode))

(defun build-riak-kv ()
  (interactive)
  (cd "/home/vagrant/riak_kv/")
  (compile "./rebar compile skip_deps=true")
  (other-window 1)
  (goto-char (point-max))
  (other-window -1))

(defun build-riak ()
  (interactive)
  (cd "/home/vagrant/riak/")
  (compile "./rebar compile")
  (other-window 1)
  (goto-char (point-max))
  (other-window -1))

(defun build-riak-core ()
  (interactive)
  (cd "/home/vagrant/riak_core/")
  (compile "./rebar compile skip_deps=true")
  (other-window 1)
  (goto-char (point-max))
  (other-window -1))

(defun build-riak-ql ()
  (interactive)
  (cd "/home/vagrant/riak_ql/")
  (compile "./rebar compile skip_deps=true")
  (other-window 1)
  (goto-char (point-max))
  (other-window -1))

(defun build-bits ()
  (interactive)
  (cd "/home/vagrant/bits/")
  (compile "./rebar compile skip_deps=true")
  (other-window 1)
  (goto-char (point-max))
  (other-window -1))

(global-set-key [f9] 'build-riak)
(global-set-key [f8] 'build-riak-core)
(global-set-key [f7] 'build-riak-kv)
(global-set-key [f6] 'build-riak-ql)
(global-set-key [f5] 'build-bits)


