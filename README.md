conf-emacs
====

## Description

Emacsの設定ファイル。環境は以下。

* Windows7 or 10 64bit
* [gnupack](https://osdn.jp/projects/gnupack/)に付属のEmacs
* gnupackのバージョンはgnupack_devel-13.06-2015.11.08

### Font

[MeiryoKe](http://www.geocities.jp/meir000/meiryoKe/) を使用する。

### org-mode

[Org Mode - Organize Your Life In Plain Text!](http://doc.norang.ca/org-mode.html) の設定を拝借している。動作を遅らせる設定項目をOFFにした。

orgデータファイルの置き場所やorg-captureのテンプレートは、`my-init.el`で設定するようにした。

### Python

Cygwinのpythonではなく、WindowsのAnacondaを使用するため、gnupackの`startup_config.ini`のPATH設定を以下のように変更する。

```
ANACONDA_PATH = %USERPROFILE%\Anaconda3
PYTHON_PATH = %ANACONDA_PATH%;%ANACONDA_PATH%\Scripts;%ANACONDA_PATH%\Library\bin
...
PATH = %PATH%;%PYTHON_PATH%
```

## License

[Released under the MIT license](http://opensource.org/licenses/mit-license.php)

## Author

[u-yuta](https://github.com/u-yuta)
