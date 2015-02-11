# Open the current repository in github. Will automatically detect the current
# branch. Does *not* need to be run from the repository root.
#
# @param file [String?] optionally pass in a file to view, can even pass
#                       a specific line-number
#
# @example
#     gh
#
#     gh lib/server.js
#
#     gh config/development.rb:12
#
function gh
  set url 'https://github.com'
  set branch (git symbolic-ref HEAD ^/dev/null | sed -e 's|^refs/heads/||')
  set repo (git remote show -n origin | grep "Fetch URL" | perl -ne 'print $1 if /(\w+\/\w+)(?=\.git)/')

  # Build GitHub URL
  set url "$url/$repo"

  if test (count $argv) -gt 0
    # Find file path relative to git root
    pushd (dirname $argv[1])
      set pwd (pwd)
      set git_dir (git rev-parse --show-toplevel)
      set file (echo $pwd | sed 's#'$git_dir'##')
      set file $file'/'(basename $argv[1])
      set file (echo $file | sed 's/:\([0-9]*\)$/#L\1/')

      set url (echo "$url/blob/$branch/$file" | sed 's#//#/#g')
    popd
  else
    set url "$url/tree/$branch"
  end

  # Go go go
  open $url
end
