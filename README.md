# Weechat-log-to-html

A script to turn Weechat logs into nicely formatted HTML.
Should work both with the contents of the `.weechat/logs` directory and the output of `bufsave.py`.

## Compile

You can use any Haskell compiler to compile this, eg. `ghc`.

## Usage

```Bash
./weechat-log-to-html < somelog.weechatlog > somelog.html
```

## Features

This script generates a clean HTML file from a Weechat log file, handling nicknames coloring, …
