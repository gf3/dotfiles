# Installation

**Note**: these dotfiles are now for the [fish shell][fish] only, if you're
looking for the *bash* or *zsh* versions you'll want to check out [version
1.0.0][v1].

## Things You Should Install

* Git (2.0+ recommended)
* [Fish Shell][fish]
* [fzf][fzf]
* [NeoVim][neovim]
* [Pygments][pygments]
* [rbenv][rbenv]
* [Silver Searcher][ag]
* [Tree][tree]

(all of these things are available on [Homebrew][brew], btw)


## NeoVim

Sensible defaults for all the things! Check out [my vimrc][vimrc] for more
info, it's pretty well documented.

Vim plugins are managed with [vim-plug][plug], so on first run in vim you'll
need to do a `:PlugInstall`.


## Fonts

If you want to take full advantage of
[airline.vim](https://github.com/bling/vim-airline) you'll want to install and
use one of the [Powerline fonts](https://github.com/Lokaltog/powerline-fonts).
I recommend either `Meslo` or `Deja Vu Sans Mono`. If you choose something
other than `Meslo` don't forget to update your [.gvimrc](.gvimrc).


[ag]:       http://geoff.greer.fm/ag/
[brew]:     http://brew.sh
[fish]:     http://fishshell.com
[fzf]:      https://github.com/junegunn/fzf
[neovim]:   http://neovim.io
[plug]:     https://github.com/junegunn/vim-plug
[pygments]: http://pygments.org
[rbenv]:    https://github.com/sstephenson/rbenv
[tree]:     http://mama.indstate.edu/users/ice/tree/
[v1]:       https://github.com/gf3/dotfiles/tree/v1.0.0
[vimrc]:    .nvimrc
