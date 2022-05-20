function fish_title --argument-names last_cmd
    # string replace --all --regex -- \s\*\n "; " $last_cmd
    # string replace ~ " ~" $PWD
    string unescape "$_hydro_pwd\x1b[0m "
end
