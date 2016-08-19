# Weechat-log-to-html

A script to turn Weechat logs into nicely formatted HTML.
Should work both with the contents of the `.weechat/logs` directory and the output of `bufsave.py`.

## Compile

Run

```Bash
make
```
to compile this script. Beware! This Makefile does non-standard things, and
does not simply call `ghc --make`!

## Usage

```Bash
./weechat-log-to-html < somelog.weechatlog > somelog.html
```

## Features

This script generates a clean HTML file from a Weechat log file, handling nicknames coloring, …
