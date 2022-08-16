function fish_title --argument-names last_cmd
    string replace --ignore-case -- ~ \~ $PWD |
    string replace --regex --all -- "(\.?[^/]{"(
        string replace --regex --all -- '^$' 1 1
    )"})[^/]*/" "\$1/"
end
