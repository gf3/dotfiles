status is-interactive || exit

set --global _hydro_git _hydro_git_$fish_pid

function $_hydro_git --on-variable $_hydro_git
    commandline --function repaint
end

function _hydro_pwd --on-variable PWD
    set --local root (command git rev-parse --show-toplevel 2>/dev/null |
        string replace --all --regex -- "^.*/" "")
    set --global _hydro_pwd (
        string replace --ignore-case -- ~ \~ $PWD |
        string replace -- "/$root/" /:/ |
        string replace --regex --all -- "(\.?[^/]{1})[^/]*/" \$1/ |
        string replace -- : "$root" |
        string replace --regex -- '([^/]+)$' "\x1b[1m\$1\x1b[22m" |
        string replace --regex --all -- '(?!^/$)/' "\x1b[2m/\x1b[22m"
    )
    test "$root" != "$_hydro_git_root" &&
        set --global _hydro_git_root $root && set $_hydro_git
end

function _hydro_postexec --on-event fish_postexec
    test "$CMD_DURATION" -lt 1000 && set _hydro_cmd_duration && return

    set --local secs (math --scale=1 $CMD_DURATION/1000 % 60)
    set --local mins (math --scale=0 $CMD_DURATION/60000 % 60)
    set --local hours (math --scale=0 $CMD_DURATION/3600000)

    test $hours -gt 0 && set --local --append out $hours"h"
    test $mins -gt 0 && set --local --append out $mins"m"
    test $secs -gt 0 && set --local --append out $secs"s"

    set --global _hydro_cmd_duration "$out "
end

function _hydro_prompt --on-event fish_prompt
    set --local last_status $pipestatus
    set --query _hydro_pwd || _hydro_pwd
    set --global _hydro_prompt "$_hydro_color_prompt$hydro_symbol_prompt"

    for code in $last_status
        if test $code -ne 0
            set _hydro_prompt "$_hydro_color_error"[(string join "\x1b[2mǀ\x1b[22m" $last_status)]
            break
        end
    end

    command kill $_hydro_last_pid 2>/dev/null

    fish --private --command "
        ! command git --no-optional-locks rev-parse 2>/dev/null && set $_hydro_git && exit

        set branch (
            command git symbolic-ref --short HEAD 2>/dev/null ||
            command git describe --tags --exact-match HEAD 2>/dev/null ||
            command git rev-parse --short HEAD 2>/dev/null |
                string replace --regex -- '(.+)' '@\$1'
        )

        test -z \"\$$_hydro_git\" && set --universal $_hydro_git \"\$branch \"

        ! command git diff-index --quiet HEAD 2>/dev/null ||
            count (command git ls-files --others --exclude-standard) >/dev/null &&
            set info \"$hydro_symbol_git_dirty\"

        for fetch in $hydro_fetch false
            command git rev-list --count --left-right @{upstream}...@ 2>/dev/null |
                read behind ahead

            switch \"\$behind \$ahead\"
                case \" \" \"0 0\"
                case \"0 *\"
                    set upstream \" $hydro_symbol_git_ahead\$ahead\"
                case \"* 0\"
                    set upstream \" $hydro_symbol_git_behind\$behind\"
                case \*
                    set upstream \" $hydro_symbol_git_ahead\$ahead $hydro_symbol_git_behind\$behind\"
            end

            set --universal $_hydro_git \"\$branch\$info\$upstream \"

            test \$fetch = true && command git fetch --no-tags 2>/dev/null
        end
    " &

    set --global _hydro_last_pid (jobs --last --pid)
end

function _hydro_fish_exit --on-event fish_exit
    set --erase $_hydro_git
end

function _hydro_uninstall --on-event hydro_uninstall
    set --names |
        string replace --filter --regex -- "^(_?hydro_)" "set --erase \$1" |
        source
    functions --erase (functions --all | string match --entire --regex "^_?hydro_")
end

for color in hydro_color_{pwd,git,error,prompt,duration}
    function $color --on-variable $color --inherit-variable color
        set --query $color && set --global _$color (set_color $$color)
    end && $color
end

set --query hydro_color_error || set --global hydro_color_error $fish_color_error
set --query hydro_symbol_prompt || set --global hydro_symbol_prompt ❱
set --query hydro_symbol_git_dirty || set --global hydro_symbol_git_dirty •
set --query hydro_symbol_git_ahead || set --global hydro_symbol_git_ahead ↑
set --query hydro_symbol_git_behind || set --global hydro_symbol_git_behind ↓

