# dotfiles

This repository contains the configuration files for the tools I use day-to-day.

These configurations are designed to work out-of-the-box on both MacOS and Void
Linux, but can be easily adapted to other distributions. 

## Installation

I use a simple tool called [yadm][yadm] to manage my dotfiles, but it's not
required! However if you're interested in using yadm it's as simple as:

```sh
yadm clone https://github.com/gf3/dotfiles.git
yadm status
yadm bootstrap
```

Otherwise you can copy over individual files and directories as needed.

## Tools

* [bat][bat] - `cat` replacement with syntax highlighting
* [diffr][diff] - `diff` replacement with better highlighting
* [Fish][fish] - Pretty and easy to use shell
* [Kakoune][kak] - Vim-like editor
* [Kitty][kitty] - GPU-accelerated terminal emulator
* [lsd][lsd] - `ls` replacement
* [pywal][pywal] - Colour scheme generation and management

[bat]: https://github.com/sharkdp/bat
[diffr]: https://github.com/mookid/diffr
[fish]: https://fishshell.com/
[kak]: http://kakoune.org/
[kitty]: https://sw.kovidgoyal.net/kitty/
[lsd]: https://github.com/Peltoche/lsd
[pywal]: https://github.com/dylanaraps/pywal
[yadm]: https://yadm.io/