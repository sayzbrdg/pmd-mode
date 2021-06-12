;;; Copyright (c) 2015-2021 Seiji Ohashi <sayzbrdg@gmail.com>
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


;; generic mode
;;
(define-generic-mode pmd-mode
  '(";")                                ; コメント開始文字列
  nil                                   ; キーワード
  ;; font-lock の設定
  '(("^#[a-zA-Z0-9]+" . font-lock-builtin-face)
    ("^R[0-9]+" . font-lock-constant-face)
    ("^[a-zA-Z]+" . font-lock-function-name-face)
    ("![a-zA-Z]" . font-lock-variable-name-face) ; PMD3.3は変数名が一文字
    ("\\\\[bschti]p?" . font-lock-function-name-face)
    ("\\\\v[bschti][+-]?" . font-lock-keyword-face)
    ("\\\\V[+-]?" . font-lock-keyword-face)
    ("\\\\[lmr][bschti]" . font-lock-type-face)
    ("[][:]" . font-lock-warning-face)
    )
  '("\\.mml$")                          ; モードを有効にするファイル名
  '(pmd-mode-setup)                     ; モード開始時に呼ばれる関数
  "P.M.D. mode")


;; key map
;;
(defvar pmd-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'pmd-save-and-compile-buffer)
    (define-key map "\C-c\C-p" 'pmd-play-file)
    map)
  "pmd-modeのキーマップ")


;; variables
;;
(defconst pmd-default-file-extension ".M"
  "PMDデータファイルのデフォルト拡張子")
(defconst pmd-compilation-buffer-name "*pmd-compilation*"
  "コンパイル結果を表示するバッファ名")

(defcustom pmd-mode-hook nil
  "pmd-modeのフック"
  :type '(hook))
(defcustom pmd-after-compile-hook nil
  "コンパイルコマンド正常終了時のフック"
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
  :type '(boolean))
(defcustom pmd-normalize-filename-function 'convert-standard-filename
  "ファイル名の正規化関数"
  :type '(restricted-sexp))


;; functions
;;
(defun pmd-mode-setup ()
  (use-local-map pmd-mode-map))


(defun pmd-search-filename (&optional buffer)
  "バッファから#filenameを検索してコンパイル後のファイル名として返す。
#filenameが見つからない場合にはバッファファイル名の拡張子を
'pmd-default-file-extension'に置き換えたものを返す。
バッファがファイルではない場合は nil を返す。
BUFFER が指定されなければ、カレントバッファを対象とする"
  (let ((basefilename (buffer-file-name buffer))
        (defined-name
          (with-current-buffer (or buffer (current-buffer))
            (save-excursion
              (goto-char (point-min))
              (when (re-search-forward "^#filename[\s\t]+\\(.+\\)$" nil t 1)
                (match-string-no-properties 1))))))
    (cond ((eq basefilename nil)
           nil)
          ;; #filenameが"."で始まるパターンは拡張子のみ置き換える
          ((and defined-name (string-match "^\\." defined-name))
           (concat (file-name-sans-extension basefilename) defined-name))
          ;; #filenameが"."で始まらないパターンはファイル名全体を置き換える
          (defined-name
            defined-name)
          (t
           (concat (file-name-sans-extension basefilename)
                   pmd-default-file-extension)))))


(defun pmd-play-file (&optional file)
  "FILEで指定されるコンパイル後のPMDデータを再生する。
FILEが指定されなければ、カレントバッファから推測される
ファイル名を使用する。'pmd-player-program-name'が設定されて
いなかったり、FILEが指定されず、カレントバッファがファイル
ではない場合はエラーになる。コンパイル後のPMDデータファイルが
存在するかのチェックは行わない。"
  (interactive)
  (catch 'error
    (let ((filename (or file (pmd-search-filename))))
      (unless pmd-player-program-name
        (error "pmd-player-program-name is not set")
        (throw 'error nil))
      (unless filename
        (error "%s is not a file buffer." (buffer-name))
        (throw 'error nil))
      ;; 起動した再生プロセスの終了を待つと、再生中の操作ができなくなる
      ;; ので待たない
      (apply 'call-process pmd-player-program-name nil 0 nil
             (append pmd-player-program-options
                     `(,(funcall pmd-normalize-filename-function filename)))))))


(defun pmd-compile-buffer-file (&optional buffer)
  "バッファのファイルを'pmd-compile-program-name'でコンパイルする。
'pmd-compile-program-name'が設定されていなかったり、バッファ
がファイルではない場合はエラーになる。事前にバッファの保存は
行わない。 BUFFER が指定されなければ、カレントバッファを
対象とする"
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
      ;; コンパイル結果出力先のバッファがウィンドウにあればそれを利用
      ;; 無ければ next-window を使用
      (set-window-buffer (or (get-buffer-window outbuffer)
                             (next-window))
                         outbuffer)
      (if (= 0 (with-current-buffer outbuffer
                 ;; コンパイル結果出力先のバッファは常に read only に
                 (let ((coding-system-for-read 'cp932-dos))
                   (unwind-protect
                       (progn
                         (setq buffer-read-only nil)
                         (erase-buffer)
                         (apply 'call-process pmd-compile-program-name nil
                                outbuffer nil
                                (append pmd-compile-program-options
                                        `(,(funcall pmd-normalize-filename-function filename)))
                                ))
                     (setq buffer-read-only t)))))
          ;; コンパイル正常終了時はフックを実行
          (progn
            (run-hooks 'pmd-after-compile-hook)
            t)
        nil))))


(defun pmd-save-and-compile-buffer (&optional buffer)
  "バッファを保存しコンパイルする。指定があればその後再生する。
'pmd-compile-program-name'が設定されていなかったり、バッファ
がファイルではない場合はエラーになる。
BUFFER が指定されなければ、カレントバッファを対象とする"
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
      (when (and (pmd-compile-buffer-file buffer) ; 異常時は継続しない
                 pmd-play-after-compile)
        (pmd-play-file (pmd-search-filename buffer))))))


(provide 'pmd-mode)

;; Local variables:
;; coding: utf-8
;; end:
