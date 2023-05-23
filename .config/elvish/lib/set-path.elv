# Set paths
use path

var optpaths = [
  ~/.bin
  ~/.cabal/bin
  ~/.cargo/bin
  ~/.local/bin
  $E:GOPATH/bin
  /Applications/Postgres.app/Contents/Versions/15/bin
  /opt/homebrew/bin
  /usr/local/bin
  /usr/bin
  /bin
  /usr/sbin
  /sbin
]

set paths = [(each {|p|
  if (path:is-dir $p) { put $p }
} $optpaths)]

