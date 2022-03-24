# weechat-log-to-html

## Ongoing Development/PRs etc.

For PRs/Issues et please see the project's main repository at: [hg.sr.ht~anelki/weechat-log-html](https://hg.sr.ht/~anelki/weechat-log-html)

---

A script to turn Weechat logs into nicely formatted HTML. Should work both with the contents of the `.weechat/logs` directory and the output of `bufsave.py`.

## Compile

You can use any Haskell compiler to compile this, eg. `ghc`.

## Usage

```sh
./weechat-log-to-html < somelog.weechatlog > somelog.html
```

## Features

This script generates a clean HTML file from a Weechat log file.

### v1.3
* further HTML cleanup, somewhat improved styling and tweaks to the color palette.


### v1.2
* edited the way HTML `<meta>` tags are written


### v1.1
* now inserts log metadata html ("title", "players", etc.) so you only need to edit the values in each field.
* minor fixes here and there

### v1.0
* made html output responsive for mobile devices
* edits to nick colors

**Full commit log can be found on [hg.sr.ht](https://hg.sr.ht/~anelki/weechat-log-html)**

## To-do

* use Haskell text instead of linked lists of characters
* provide a way to use logs from rooms that are bridged, eg., via [Matterbridge](https://github.com/42wim/matterbridge)

Matterbridge support is important because as logs will just show up as the bridge user. In this example, the bridge user's nick is 'matterbridge:'

```
2021-08-05 08:33:28     matterbridge    <xmpp user> I love XMPP and selfhosting is easy
2021-08-05 08:37:41     matterbridge    <matrix user> nah, Matrix is so secure and flawless
2021-08-05 08:41:04     IRC User	maybe i'm just oldschool, but IRC is still where it's at!
```

As it is now, this tool will attribute all of the bridged users as a single user, making a log harder to grok. Patches to fix this would be welcome.

## License
See LICENSE.txt

