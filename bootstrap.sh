# Clone repo
echo "=> Downloading"
git clone git://github.com/gf3/dotfiles.git ~/.dotfiles

# Install!
echo "=> Installing"
cd ~/.dotfiles
rake backup
rake install

echo "=> Done"

