# dotfiles

This repository contains the configuration files for the tools I use day-to-day.

![Screenshot](/.config/yadm/Screenshot.png?raw=true "Some of the tools in use")

**Note**: these dotfiles are now for the [fish shell][fish] and
[Spacemacs][spacemacs] only, if you're looking for the *bash* or *zsh* or *vim*
or *neovim* versions you'll want to check out [version 2.2.0][v2].

These configurations are designed to work out-of-the-box on both MacOS and Arch
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

* [bat][bat] - A cat(1) clone with wings
* [delta][delta] - A viewer for git and diff output
* [fd][fd] - A simple, fast and user-friendly alternative to 'find'
* [Fish][fish] - The user-friendly command line shell
* [fzf][fzf] - 🌸 A command-line fuzzy finder
* [glow][glow] - Render markdown on the CLI, with pizzazz! 💅🏻
* [imv][imv] - Image viewer for X11/Wayland
* [jq][jq] - Command-line JSON processor 
* [Kitty][kitty] - the fast, featureful, GPU based terminal emulator
* [lazygit][lazygit] - simple terminal UI for git commands 
* [lsd][lsd] - The next gen ls command
* [Qute][qute] - A keyboard-driven, vim-like browser based on PyQt5
* [ripgrep][ripgrep] - ripgrep recursively searches directories for a regex pattern
* [Spacemacs][spacemacs] - A community-driven Emacs distribution
* [Sway][sway] - i3-compatible Wayland compositor
* [Waybar][waybar] - Highly customizable Wayland bar for Sway and Wlroots based compositors
* [wlsunset][wlsunset] - Day/night gamma adjustments for Wayland compositors supporting wlr-gamma-control-unstable-v1
* [wluma][wluma] - Automatic brightness adjustment based on screen contents
* [Zathura][zathura] - a document viewer

## Fonts

I use the [Hack Nerd Font][hack-nerd-font] which is the [Hack][hack-font] font
which has been patched with additional glyphs.

Additionally, I use [San Francisco][sf] for as my sans-serif font.

[bat]: https://github.com/sharkdp/bat
[delta]: https://github.com/dandavison/delta
[fd]: https://github.com/sharkdp/fd
[fish]: https://fishshell.com/
[fzf]: https://github.com/junegunn/fzf
[glow]: https://github.com/charmbracelet/glow
[hack-font]: https://sourcefoundry.org/hack/
[hack-nerd-font]: https://github.com/ryanoasis/nerd-fonts#patched-fonts
[imv]: https://github.com/eXeC64/imv
[jq]: https://github.com/stedolan/jq
[kitty]: https://sw.kovidgoyal.net/kitty/
[lazygit]: https://github.com/jesseduffield/lazygit
[lsd]: https://github.com/Peltoche/lsd
[qute]: https://qutebrowser.org/
[ripgrep]: https://github.com/BurntSushi/ripgrep
[sf]: https://developer.apple.com/fonts/
[spacemacs]: https://www.spacemacs.org/
[sway]: https://swaywm.org/
[v2]: https://github.com/gf3/dotfiles/tree/v2.2.0
[waybar]: https://github.com/Alexays/Waybar
[wlsunset]: https://sr.ht/~kennylevinsen/wlsunset/
[wluma]: https://github.com/maximbaz/wluma
[yadm]: https://yadm.io/
[zathura]: https://github.com/pwmt/zathura
