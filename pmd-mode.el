;;; Copyright (c) 2015 Seiji Ohashi <sayzbrdg@gmail.com>
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;  1. Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;  2. Redistributions in binary form must reproduce the above copyright
;;;     notice, this list of conditions and the following disclaimer in the
;;;     documentation and/or other materials provided with the distribution.
;;;  3. Neither the name of the authors nor the names of its contributors
;;;     may be used to endorse or promote products derived from this
;;;     software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(require 'generic-x)

(define-generic-mode pmd-mode
  '(";")
  nil
  '(("^#[a-zA-Z0-9]+" . font-lock-builtin-face)
    ("^R[0-9]+" . font-lock-constant-face)
    ("^[a-zA-Z]+" . font-lock-function-name-face)
    ("![a-zA-Z]" . font-lock-variable-name-face)
    ("\\\\[bschti]p?" . font-lock-function-name-face)
    ("\\\\v[bschti][+-]?" . font-lock-keyword-face)
    ("\\\\V[+-]?" . font-lock-keyword-face)
    ("\\\\[lmr][bschti]" . font-lock-type-face)
    ("[][:]" . font-lock-warning-face)
    )
  '("\\.mml$")
  '(pmd-mode-setup)
  "P.M.D. mode")

(defvar pmd-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'pmd-save-and-compile-buffer)
    (define-key map "\C-c\C-p" 'pmd-play-file)
    map)
  "pmd-modeのキーマップ")

(defconst pmd-default-file-extension ".M"
  "PMDデータファイルのデフォルト拡張子")
(defconst pmd-compilation-buffer-name "*pmd-compilation*"
  "コンパイル結果を表示するバッファ名")

(defcustom pmd-mode-hook nil
  "pmd-modeのフック"
  :type '(hook))
(defcustom pmd-compile-program-name "MC.EXE"
  "PMDコンパイラのプログラム名"
  :type '(string))
(defcustom pmd-compile-program-options '("/V" "/C")
  "PMDコンパイラのコマンドラインオプション"
  :type '(repeat string))
(defcustom pmd-player-program-name nil
  "PMDファイルプレイヤー"
  :type '(restricted-sexp :match-alternatives (stringp 'nil)))
(defcustom pmd-player-program-options nil
  "PMDファイルプレイヤーのコマンドラインオプション"
  :type '(repeat string))
(defcustom pmd-play-after-compile nil
  "コンパイル後に再生する場合はtを指定"
  :type
  '(boolean))

(defun pmd-mode-setup ()
  (use-local-map pmd-mode-map))

(defun pmd-search-filename (&optional buffer)
  (let ((basefilename (buffer-file-name buffer))
        (defined-name
          (with-current-buffer (or buffer (current-buffer))
            (save-excursion
              (goto-char (point-min))
              (when (re-search-forward "^#filename[\s\t]+\\(.+\\)$" nil t 1)
                (match-string-no-properties 1))))))
    (cond ((eq basefilename nil)
           nil)
          ((and defined-name (string-match "^\\." defined-name))
           (concat (file-name-sans-extension basefilename) defined-name))
          (defined-name
            (concat (file-name-directory basefilename) defined-name))
          (t
           (concat (file-name-sans-extension basefilename)
                   pmd-default-file-extension)))))

(defun pmd-play-file (&optional file)
  (interactive)
  (catch 'error
    (let ((filename (or file (pmd-search-filename))))
      (unless pmd-player-program-name
        (error "pmd-player-program-name is not set")
        (throw 'error nil))
      (unless filename
        (error "%s is not a file buffer." (buffer-name))
        (throw 'error nil))
      (apply 'call-process pmd-player-program-name nil 0 nil
             (append pmd-player-program-options `(,filename))))))

(defun pmd-compile-buffer-file (&optional buffer)
  (catch 'error
    (let ((filename (buffer-file-name buffer))
          (outbuffer (get-buffer-create pmd-compilation-buffer-name)))
      (unless pmd-compile-program-name
        (error "pmd-compile-program-name is not set")
        (throw 'error nil))
      (unless filename
        (error "%s is not a file buffer." (buffer-name buffer))
        (throw 'error nil))
      (when (one-window-p)
        (split-window-vertically))
      (set-window-buffer (or (get-buffer-window outbuffer)
                             (next-window))
                         outbuffer)
      (with-current-buffer outbuffer
        (unwind-protect
            (progn
              (setq buffer-read-only nil)
              (erase-buffer)
              (apply 'call-process pmd-compile-program-name nil outbuffer nil
                     (append pmd-compile-program-options `(,filename))))
          (setq buffer-read-only t))))))

(defun pmd-save-and-compile-buffer (&optional buffer)
  (interactive)
  (catch 'error
    (let* ((targetbuffer (or buffer (current-buffer)))
           (filename (buffer-file-name targetbuffer)))
      (unless pmd-compile-program-name
        (error "pmd-compile-program-name is not set")
        (throw 'error nil))
      (unless filename
        (error "%s is not a file buffer." (buffer-name buffer))
        (throw 'error nil))
      (with-current-buffer targetbuffer
        (save-buffer))
      (when (and (= (pmd-compile-buffer-file buffer) 0)
                 pmd-play-after-compile)
		(pmd-play-file (pmd-search-filename buffer))))))

(provide 'pmd-mode)

;; Local variables:
;; coding: utf-8
;; end:
