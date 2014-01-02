#!/bin/bash

#-----------------------------------------------------------------------------
# Functions
#-----------------------------------------------------------------------------
# The different colours as variables
Y="\033[01;33m" # YELLOW
C="\033[01;36m" # CYAN
W="\033[01;37m" # WHITE
B="\033[01;34m" # BLUE
G="\033[01;32m" # GREEN
D="\033[01;31m" # RED
X="\033[00;37m" # Not sure...
R="\033[0m"


# Notice title
notice() { echo -e "${G}=> $1${R}"; }

# Error title
error() { echo -e "${D}=> Error: $1${R}"; }

# List item
c_list() { echo -e "  ${G}+${R} $1"; }

# Error list item
e_list() { echo -e "  ${D}-${R} $1"; }

# Check for dependency
dep() {
  type -p $1 &> /dev/null
  local installed=$?
  if [ $installed -eq 0 ]; then
    c_list $1
  else
    e_list $1
  fi
  return $installed
}

backup() {
  set -e
  mkdir -p $backupdir

  local files=( $(ls -a) )
  for file in "${files[@]}"; do
    in_array $file "${excluded[@]}" || mv -f "$HOME/$file" "$backupdir/$file"
  done
  set +e
}

install() {
  local files=( $(ls -a) )
  for file in "${files[@]}"; do
    in_array $file "${excluded[@]}"
    should_install=$?
    if [ $should_install -gt 0 ]; then
      [ -d "$HOME/$file" ] && rm -rf "$HOME/$file"
      cp -Rf "$file" "$HOME/$file"
    fi
  done
}

in_array() {
  local hay needle=$1
  shift
  for hay; do
    [[ $hay == $needle ]] && return 0
  done
  return 1
}


#-----------------------------------------------------------------------------
# Initialize
#-----------------------------------------------------------------------------

backupdir="$HOME/.dotfiles-backup/$(date "+%Y%m%d%H%M.%S")"
dependencies=(git hg pygmentize tree vim xmllint)
excluded=(. .. .git .gitignore .gitmodules bootstrap.sh Gemfile Gemfile.lock Rakefile README.md)


#-----------------------------------------------------------------------------
# Dependencies
#-----------------------------------------------------------------------------

notice "Checking dependencies"

not_met=0
for need in "${dependencies[@]}"; do
  dep $need
  met=$?
  not_met=$(echo "$not_met + $met" | bc)
done

if [ $not_met -gt 0 ]; then
  error "$not_met dependencies not met!"
  exit 1
fi


#-----------------------------------------------------------------------------
# Install
#-----------------------------------------------------------------------------

# Assumes $HOME/.dotfiles is *ours*
if [ -d $HOME/.dotfiles ]; then
  pushd $HOME/.dotfiles

  # Update Repo
  notice "Updating"
  git pull origin master
  git submodule init
  git submodule update

  # Backup
  notice "Backup up old files ($backupdir)"
  backup

  # Install
  notice "Installing"
  install
else
  # Clone Repo
  notice "Downloading"
  git clone --recursive git://github.com/gf3/dotfiles.git $HOME/.dotfiles

  pushd $HOME/.dotfiles

  # Backup
  notice "Backup up old files ($backupdir)"
  backup

  # Install
  notice "Installing"
  install
fi


#-----------------------------------------------------------------------------
# Finished
#-----------------------------------------------------------------------------

popd
notice "Done"
exec $SHELL -l

