# --- Functions --- #
# Notice title
function notice { echo  "\033[1;32m=> $1\033[0m"; }

# Error title
function error { echo "\033[1;31m=> Error: $1\033[0m"; }

# List item
function c_list { echo  "  \033[1;32m✔\033[0m $1"; }

# Error list item
function e_list { echo  "  \033[1;31m✖\033[0m $1"; }

# Check for dependency
function dep {
  # Check installed
  local i=true
  type -p $1 &> /dev/null || i=false

  # Check version
  if $i ; then
    local version=$($1 --version | grep -oE -m 1 "[[:digit:]]+\.[[:digit:]]+\.?[[:digit:]]?")
    [[ $version < $2 ]] && local msg="$1 version installed: $version, version needed: $2"
  else
    local msg="Missing $1"
  fi
  
  # Save if dep not met
  if ! $i || [ -n "$msg" ] ; then
    missing+=($msg)
  fi
}

# --- INIT --- #
current_pwd=$(pwd)
missing=()

# --- Check deps --- #
notice "Checking dependencies"
dep "git"  "1.7"
dep "hg"   "1.6"
dep "ruby" "1.8"
dep "vim" "7.3"
dep "tree" "1.5"

if [ "${#missing[@]}" -gt "0" ]; then
  error "Missing dependencies"
  for need in "${missing[@]}"; do
    e_list "$need."
  done
  exit 1
fi

# Assumes ~/.dotfiles is *ours*
if [ -d ~/.dotfiles ]; then
  # --- Update Repo --- #
  notice "Updating"
  cd ~/.dotfiles
  git pull origin master
  git submodule init
  git submodule update

  # --- Install --- #
  notice "Installing"
  rake install
else
  # --- Clone Repo --- #
  notice "Downloading"
  git clone --recursive git://github.com/gf3/dotfiles.git ~/.dotfiles

  # --- Install --- #
  notice "Installing"
  cd ~/.dotfiles
  rake backup
  rake install
fi

# --- Finished --- #
cd $current_pwd
exec $SHELL
notice "Done"

