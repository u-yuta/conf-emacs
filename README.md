conf-emacs
====

## Description

Windowsの[gnupack](https://osdn.jp/projects/gnupack/)に付属するEmacsの設定ファイル。

### Font

[MeiryoKe](http://www.geocities.jp/meir000/meiryoKe/) を使用する。

### org-mode

[Org Mode - Organize Your Life In Plain Text!](http://doc.norang.ca/org-mode.html) を拝借している。動作を遅らせる設定項目をOFFにした。

orgデータファイルの置き場所等は、`my-init.el`で設定するようにした。

### Python

Cygwinのpythonではなく、WindowsのAnacondaを使用するため、gnupackの`startup_config.ini`のPATH設定を以下のように変更する。

```
ANACONDA_PATH = %USERPROFILE%\Anaconda3
PYTHON_PATH = %ANACONDA_PATH%;%ANACONDA_PATH%\Scripts;%ANACONDA_PATH%\Library\bin
PATH = %PATH%
PATH = %PATH%;%PYTHON_PATH%
```

## License

[Released under the MIT license](http://opensource.org/licenses/mit-license.php)

## Author

[u-yuta](https://github.com/u-yuta)
