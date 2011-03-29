taglist-plus.vim
================

This is a fork of the popular taglist.vim plugin.

Taglist-plus provides excellent Javascript support via jsctags. See [here][1]
for examples.

This plugin can also be obtained through [vim.org][2].

Installation
------------

Unzip the plugin files to your `.vim` directory.

For great Javascript support, jsctags is required as well.

jsctags can be obtained via [github][4]. It requires a recent version of
node.js -- the default one provided in Maverick Meerkat won't cut it. You can
[build it from source][6] or get it via [this PPA][5].

If you're going the PPA route, you'll need to map `nodejs` to `node`. Here are
the commands:
<pre>
sudo add-apt-repository ppa:richarvey/nodester
sudo apt-get update
sudo apt-get install nodejs
sudo ln -s /usr/bin/nodejs /usr/local/bin/node
</pre>

Usage
-----

`:TlistToggle` brings up the taglist window.  More commands can be found via
`:help taglist-commands`.

[1]:http://discontinuously.com/2011/03/vim-support-javascript-taglist-plus/
[2]:https://github.com/int3/vim-taglist-plus
[3]:http://www.vim.org/scripts/script.php?script_id=3504
[4]:https://github.com/mozilla/doctorjs
[5]:https://launchpad.net/~richarvey/+archive/nodester
[6]:http://nodejs.org/#download
