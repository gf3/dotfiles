# dotfiles

This repository contains the configuration files for the tools I use day-to-day.

![Light](/.config/yadm/screenshot-light.png?raw=true "Light mode")
![Dark](/.config/yadm/screenshot-dark.png?raw=true "Dark mode")

**Note**: these dotfiles are now for the [fish shell][fish] and
emacs only, if you're looking for the _bash_ or _zsh_ or _vim_
or _neovim_ versions you'll want to check out [version 2.2.0][v2].

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

- [bat][bat] - A cat(1) clone with wings
- [delta][delta] - A viewer for git and diff output
- [fd][fd] - A simple, fast and user-friendly alternative to 'find'
- [Fish][fish] - The user-friendly command line shell
- [fzf][fzf] - 🌸 A command-line fuzzy finder
- [glow][glow] - Render markdown on the CLI, with pizzazz! 💅🏻
- [jq][jq] - Command-line JSON processor
- [lazygit][lazygit] - simple terminal UI for git commands
- [lsd][lsd] - The next gen ls command
- [Qute][qute] - A keyboard-driven, vim-like browser based on PyQt5
- [ripgrep][ripgrep] - ripgrep recursively searches directories for a regex pattern
- [WezTerm][wezterm] - A GPU-accelerated cross-platform terminal emulator and multiplexer

## Fonts

I use the [JetBrains Mono][jetbrains-mono] which is a beautiful font designed for developers. It has all sorts of fun features, ligatures, and powerline symbols.

[bat]: https://github.com/sharkdp/bat
[delta]: https://github.com/dandavison/delta
[fd]: https://github.com/sharkdp/fd
[fish]: https://fishshell.com/
[fzf]: https://github.com/junegunn/fzf
[glow]: https://github.com/charmbracelet/glow
[jetbrains-mono]: https://www.jetbrains.com/lp/mono/
[jq]: https://github.com/stedolan/jq
[lazygit]: https://github.com/jesseduffield/lazygit
[lsd]: https://github.com/Peltoche/lsd
[ripgrep]: https://github.com/BurntSushi/ripgrep
[v2]: https://github.com/gf3/dotfiles/tree/v2.2.0
[wezterm]: https://github.com/wez/wezterm
[yadm]: https://yadm.io/
