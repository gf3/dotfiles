# dotfiles

This repository contains the configuration files for the tools I use day-to-day.

**Note**: these dotfiles are now for the [fish shell][fish] and [Kakoune][kak]
only, if you're looking for the *bash* or *zsh* or *vim* or *neovim* versions 
you'll want to  check out [version 2.2.0][v2].

These configurations are designed to work out-of-the-box on both MacOS and
Ubuntu Linux, but can be easily adapted to other distributions. 

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

* [bat][bat] - A cat(1) clone with wings. 
* [delta][delta] - A viewer for git and diff output
* [espanso][espanso] - Cross-platform Text Expander written in Rust
* [fd][fd] - A simple, fast and user-friendly alternative to 'find'
* [Fish][fish] - The user-friendly command line shell.
* [fzf][fzf] - 🌸 A command-line fuzzy finder
* [glow][glow] - Render markdown on the CLI, with pizzazz! 💅🏻
* [jq][jq] - Command-line JSON processor 
* [Kakoune][kak] - Modal editor — Faster as in fewer keystrokes — Multiple selections — Orthogonal design
* [Kitty][kitty] - the fast, featureful, GPU based terminal emulator
* [lazygit][lazygit] - simple terminal UI for git commands 
* [lsd][lsd] - The next gen ls command
* [pywal][pywal] - 🎨 Generate and change color-schemes on the fly.
* [ripgrep][ripgrep] - ripgrep recursively searches directories for a regex pattern

## Fonts

I use the [Hack Nerd Font][hack-nerd-font] which is the [Hack][hack-font] font
which has been patched with additional glyphs.

[bat]: https://github.com/sharkdp/bat
[delta]: https://github.com/dandavison/delta
[espanso]: https://github.com/federico-terzi/espanso
[fd]: https://github.com/sharkdp/fd
[fish]: https://fishshell.com/
[fzf]: https://github.com/junegunn/fzf
[glow]: https://github.com/charmbracelet/glow
[hack-font]: https://sourcefoundry.org/hack/
[hack-nerd-font]: https://github.com/ryanoasis/nerd-fonts#patched-fonts
[jq]: https://github.com/stedolan/jq
[kak]: http://kakoune.org/
[kitty]: https://sw.kovidgoyal.net/kitty/
[lazygit]: https://github.com/jesseduffield/lazygit
[lsd]: https://github.com/Peltoche/lsd
[pywal]: https://github.com/dylanaraps/pywal
[ripgrep]: https://github.com/BurntSushi/ripgrep
[v2]: https://github.com/gf3/dotfiles/tree/v2.2.0
[yadm]: https://yadm.io/
