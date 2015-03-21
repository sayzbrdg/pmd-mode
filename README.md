# P.M.D. mode
generic-modeを利用して作成した、[FM音源ドライバPMD](https://sites.google.com/site/kajapon/pmd)
用のちっちゃなメジャーモードです。

# できる事
 - 変数やループ命令、OPNAドラム命令文字列に色が付いて見易くなります。
 - MML保存からコンパイル、曲の再生までを1コマンドで実行します。
 - 再生するファイル名は`#filename`を検索して、自動で適切な名前を生成します。

# 使い方
pmd-modeを使用する為には[emacs](https://www.gnu.org/software/emacs/)が必要です。

## インストール方法

今のところインストーラーは用意していません。emacs の load-pathに
設定されているパスのどれかに pmd-mode.el をコピーして下さい。

そして emacs のコンフィグ に`(autoload 'pmd-mode "pmd-mode" nil t)`と
`(add-to-list 'auto-mode-alist '("\\.mml$" . pmd-mode))`を追記します。

## キーバインド

pmd-mode用に2つのキーバインドが設定されています。

- C-c C-c  
現在のバッファを保存して、MMLコンパイラを呼び出します。コンパイル後に
自動再生する設定になっている場合は、コンパイルが正常終了していれば
再生します。
- C-c C-p
コンパイル後のファイルを再生します。バッファを保存したり、MMLコンパイラ
を呼び出したりはしません。ファイルの存在も確認しないため、もしファイルが
無い場合は再生プログラム側でエラーになるでしょう。

## カスタマイズ変数

以下のカスタマイズ変数を用意しています。

### pmd-mode-hook
pmd-mode起動時に実行されるフック変数です。

### pmd-compile-program-name
MMLのコンパイルに使用するプログラム名です。オプションは次のカスタマイズ
変数で設定します。

### pmd-compile-program-options
pmd-compile-program-name で指定したプログラムに渡すオプションを文字列の
リストで設定します。

### pmd-player-program-name
コンパイルしたファイルを再生するプログラム名です。オプションは次の
カスタマイズ変数で設定します。コンパイル後に自動で再生したくない場合は
後述の pmd-play-after-compile をnilに設定します。

### pmd-player-program-options
pmd-player-program-name で指定したプログラムに渡すオプションを文字列の
リストで設定します。

### pmd-play-after-compile
nil以外を指定すると、MMLコンパイル後に自動で pmd-player-program-name に
設定したプログラムを呼び出して再生します。

## サンプル

作者の設定を載せておきます。64bit Windows環境では MC.EXE を直接実行
できないので、[MS-DOS Player](http://homepage3.nifty.com/takeda-toshiya/msdos/)
を使用して実行しています。再生プログラムは[FMPMD2000](http://c60.la.coocan.jp/fmpmd.html)
で動作を確認しています。

    (autoload 'pmd-mode "pmd-mode" nil t)
    (add-to-list 'auto-mode-alist '("\\.mml$" . pmd-mode))
    (setq pmd-compile-program-name "msdos")
    (setq pmd-compile-program-options '("-e" "mc" "/V" "/C"))
    (setq pmd-player-program-name "fmpmd")
    (setq pmd-player-program-options nil)
    (setq pmd-play-after-compile t)

# 既知の不具合
パスの区切り文字を **\\(バックスラッシュ|円マーク)** ではなく **/(スラッシュ)**
でプログラムに渡します。この影響でFMPMD2000が正常終了しない等の不具合が
あることがわかっています。

エクスプローラーからファイルをFMPMD2000にドラッグアンドドロップで再生
しなおせば回避できますが、嬉しくない問題ですので修正予定です。
