# ╭─────────────╥────────────────────╮
# │ Author:     ║ File:              │
# │ Andrey Orst ║ plug.kak           │
# ╞═════════════╩════════════════════╡
# │ plug.kak is a plugin manager for │
# │ Kakoune. It can install plugins  │
# │ keep them updated and uninstall  │
# ╞══════════════════════════════════╡
# │ GitHub repo:                     │
# │ GitHub.com/andreyorst/plug.kak   │
# ╰──────────────────────────────────╯

# Public options
declare-option -docstring \
"Path where plugins should be installed.

    Defaults to the plug.kak installation directory" \
str plug_install_dir %sh{ echo "${kak_source%%/rc*}/../" }

declare-option -docstring \
"Default domain to access git repositories. Can be changed to any preferred domain, like gitlab, bitbucket, gitea, etc.

    Default value: 'https://github.com'" \
str plug_git_domain 'https://github.com'

declare-option -docstring \
"Sort sourced files by depth" \
bool plug_depth_sort false

declare-option -docstring \
"Sort sourced files by depth" \
bool plug_profile false

declare-option -docstring \
"Maximum amount of simultaneously active downloads when installing or updating all plugins
    Default value: 10
" \
int plug_max_active_downloads 10

declare-option -docstring \
"Always ensure that all plugins are installed. If this option specified, all uninstalled plugins are being installed when Kakoune starts." \
bool plug_always_ensure false

declare-option -docstring "name of the client in which utilities display information" \
str toolsclient

# Private options
declare-option -hidden -docstring \
"Array of all plugins, mentioned in any configuration file.
Empty by default, and erased on reload of main Kakoune configuration, to track if some plugins were disabled
Should not be modified by user." \
str plug_plugins ''

declare-option -hidden -docstring \
"List of loaded plugins. Has no default value.
Should not be cleared during update of configuration files. Should not be modified by user." \
str plug_loaded_plugins

declare-option -hidden -docstring \
"List of post update/install hooks to be executed" \
str-list plug_post_hooks

declare-option -hidden -docstring \
"List of post update/install hooks to be executed" \
str-list plug_domains

# since we want to add highlighters to kak filetype we need to require kak module
# using `try' here since kakrc module may not be available in rare cases
try %@
    require-module kak

    try %$
        add-highlighter shared/kakrc/code/plug_keywords   regex \b(plug|do|config|subset|domain|defer|demand|load-path|branch|tag|commit)\b 0:keyword
        add-highlighter shared/kakrc/code/plug_attributes regex \b(noload|ensure|theme|(no-)?depth-sort)\b 0:attribute
        add-highlighter shared/kakrc/plug_post_hooks1     region -recurse '\{' '\bdo\K\h+%\{' '\}' ref sh
        add-highlighter shared/kakrc/plug_post_hooks2     region -recurse '\[' '\bdo\K\h+%\[' '\]' ref sh
        add-highlighter shared/kakrc/plug_post_hooks3     region -recurse '\(' '\bdo\K\h+%\(' '\)' ref sh
        add-highlighter shared/kakrc/plug_post_hooks4     region -recurse '<'  '\bdo\K\h+%<'  '>'  ref sh
    $ catch %$
        echo -debug "plug.kak: Can't declare highlighters for 'kak' filetype."
        echo -debug "          Detailed error: %val{error}"
    $
@ catch %{
    echo -debug "Can't require 'kak' module to declare highlighters for plug.kak."
    echo -debug "Check if kakrc.kak is available in your autoload."
}

# *plug* highlighters
try %{
    add-highlighter shared/plug_buffer group
    add-highlighter shared/plug_buffer/done          regex [^:]+:\h+(Up\h+to\h+date|Done|Installed)$                    1:string
    add-highlighter shared/plug_buffer/update        regex [^:]+:\h+(Update\h+available|Deleted)$                       1:keyword
    add-highlighter shared/plug_buffer/not_installed regex [^:]+:\h+(Not\h+(installed|loaded)|(\w+\h+)?Error([^\n]+)?)$ 1:red+b
    add-highlighter shared/plug_buffer/updating      regex [^:]+:\h+(Installing|Updating|Local\h+changes)$              1:type
    add-highlighter shared/plug_buffer/working       regex [^:]+:\h+(Running\h+post-update\h+hooks|Waiting[^\n]+)$      1:attribute
} catch %{
    echo -debug "plug.kak: Can't declare highlighters for *plug* buffer."
    echo -debug "          Detailed error: %val{error}"
}

hook -group plug-syntax global WinSetOption filetype=plug %{
    add-highlighter buffer/plug_buffer ref plug_buffer
    hook -always -once window WinSetOption filetype=.* %{
        remove-highlighter buffer/plug_buffer
    }
}

define-command -override -docstring \
"plug <plugin> [<switches>]: manage <plugin> from ""%opt{plug_install_dir}""
Switches:
    branch (tag, commit) <str>      checkout to <str> before loading plugin
    noload                          do not source plugin files
    subset <subset>                 source only <subset> of plugin files
    load-path <path>                path for loading plugin from foreign location
    defer <module> <configurations> load plugin <configurations> only when <module> is loaded
    config <configurations>         plugin <configurations>" \
plug -params 1.. -shell-script-candidates %{ ls -1 ${kak_opt_plug_install_dir} } %{ try %{
    evaluate-commands %sh{
        [ "${kak_opt_plug_profile}" = "true" ] && start=$(date +%s%N)
        plugin="${1%%.git}"
        shift
        plugin_name="${plugin##*/}"
        plugin_opt_name=$(printf "%s\n" "${plugin_name}" | sed 's/[^a-zA-Z0-9_]/_/g')
        load_files='*.kak'
        path_to_plugin="${kak_opt_plug_install_dir}/${plugin_name}"

        if [ $(expr "${kak_opt_plug_loaded_plugins}" : ".*${plugin}.*") -ne 0 ]; then
            printf "%s\n" "echo -markup %{{Information}${plugin_name} already loaded}"
            exit
        fi

        [ $(expr "${kak_opt_plug_plugins}" : ".*${plugin}.*") -eq 0 ] && printf "%s\n" "set-option -add global plug_plugins %{${plugin} }"
        [ "${kak_opt_plug_depth_sort}" = "true" ] && depth_sort="true" || depth_sort="false"

        while [ $# -gt 0 ]; do
            case $1 in
                (branch|tag|commit)
                    checkout_type=$1
                    shift
                    checkout="$1" ;;
                (noload)
                    noload=1 ;;
                (subset)
                    shift
                    subset=1
                    load_files="$1" ;;
                (load-path)
                    shift
                    path_to_plugin=$(printf "%s\n" "$1" | sed "s:^\s*~/:${HOME}/:") ;;
                (defer)
                    shift
                    module="$1"
                    shift
                    [ -z "${1##*@*}" ] && deferred_conf=$(printf "%s\n" "$1" | sed "s/@/@@/g") || deferred_conf="$1"
                    deferred_conf=$(printf "%s\n" "hook global ModuleLoaded ${module} %@ ${deferred_conf} @")
                    configurations="${configurations}
                    ${deferred_conf}" ;;
                (demand)
                    demand=1 ;;
                (do)
                    shift
                    hooks="${hooks} %{${plugin_name}} %{$1}" ;;
                (ensure)
                    ensure=1 ;;
                (theme)
                    noload=1
                    theme_hooks="mkdir -p ${kak_config}/colors
                           find . -type f -name '*.kak' -exec cp {} ${kak_config}/colors/ \;"
                    hooks="${hooks} %{${plugin_name}} %{${theme_hooks}}" ;;
                (depth-sort)
                    depth_sort="true" ;;
                (no-depth-sort)
                    depth_sort="false" ;;
                (domain)
                    shift
                    domains="${domains} %{${plugin_name}} %{$1}" ;;
                (config)
                    shift
                    configurations="${configurations} $1" ;;
                (*)
                    configurations="${configurations} $1" ;;
            esac
            shift
        done

        if [ -n "${demand}" ] && [ -n "${module}" ]; then
            configurations="${configurations}
                            require-module ${module}"
        fi

        # bake configuration options. We need this in case plugins are not installed, but
        # their configurations are known to `plug.kak', so it can load those after installation
        # automatically.
        [ -z "${configurations##*&*}" ] && configurations=$(printf "%s\n" "${configurations}" | sed "s/&/&&/g")
        printf "%s\n" "declare-option -hidden str plug_${plugin_opt_name}_conf %&${configurations}&"
        [ -n "${hooks}" ] &&   printf "%s\n" "set-option -add global plug_post_hooks ${hooks}"
        [ -n "${domains}" ] && printf "%s\n" "set-option -add global plug_domains ${domains}"

        if [ -n "${noload}" ] && [ -n "${subset}" ]; then
            printf "%s\n" "echo -debug %{Warning: plug.kak: ${plugin_name}: 'load' has higer priority than 'noload'}"
            noload=
        fi

        if [ -d "${path_to_plugin}" ]; then
            if [ -n "${checkout}" ]; then
                (
                    cd "${path_to_plugin}"
                    [ -z "${GIT_TERMINAL_PROMPT}" ] && export GIT_TERMINAL_PROMPT=0
                    if [ "${checkout_type}" = "branch" ]; then
                        current_branch=$(git branch | awk '/^\*/ { print $2 }')
                        [ "${current_branch}" != "${checkout}" ] && git fetch >/dev/null 2>&1
                    fi
                    git checkout ${checkout} >/dev/null 2>&1
                )
            fi
            if [ -z "${noload}" ]; then {
                IFS='
'
                set -f # set noglob
                for file in ${load_files}; do
                    # trim leading and trailing whitespaces. Looks ugly, but faster than `sed'
                    file="${file#"${file%%[![:space:]]*}"}"
                    file="${file%"${file##*[![:space:]]}"}"
                    if [ "${depth_sort}" = "true" ]; then
                        # performance hungry place.
                        find -L "${path_to_plugin}" -path '*/.git' -prune -o -type f -name "${file}" -print | perl -e '
                            print map  { $_->[0] }
                                  sort { $a->[1] <=> $b->[1] }
                                  map  { [$_, ($_ =~ s/^/source "/ && $_ =~ s/$/"/ && $_ =~ s/\//\//g)] }
                                           <>;'
                    else
                        # source files in order that `find' is returning
                        # may break some plugins
                        find -L "${path_to_plugin}" -path '*/.git' -prune -o -type f -name "${file}" -exec printf 'source "%s"\n' {} \;
                    fi
                done
            } fi
            printf "%s\n" "evaluate-commands %opt{plug_${plugin_opt_name}_conf}"
            printf "%s\n" "set-option -add global plug_loaded_plugins %{${plugin} }"
        else
            if [ -n "${ensure}" ] || [ "${kak_opt_plug_always_ensure}" = "true" ]; then
                printf "%s\n" "evaluate-commands plug-install ${plugin}"
            fi
        fi
        [ "${kak_opt_plug_profile}" = "true" ] && printf "%s\n" "echo -debug %{'$plugin' loaded in $(echo "($(date +%s%N)-${start})/1000000" | bc) ms}"
    }
} catch %{
    echo -debug "plug.kak: Error occured while loading '%arg{1}' plugin:"
    echo -debug "          %val{error}"
}}

define-command -override -docstring \
"plug-install [<plugin>]: install <plugin>.
If <plugin> omitted installs all plugins mentioned in configuration files" \
plug-install -params ..1 %{ nop %sh{ (
    plugin=$1
    plugin_name="${plugin##*/}"
    jobs=$(mktemp ${TMPDIR:-/tmp}/plug.kak.jobs.XXXXXX)

    [ -z "${GIT_TERMINAL_PROMPT}" ] && export GIT_TERMINAL_PROMPT=0

    if [ ! -d ${kak_opt_plug_install_dir} ]; then
        if ! mkdir -p ${kak_opt_plug_install_dir} >/dev/null 2>&1; then
            printf "%s\n" "evaluate-commands -client ${kak_client:-client0} echo -debug 'plug.kak: Error: unable to create directory for plugins'" | kak -p ${kak_session}
            exit
        fi
    fi

    printf "%s\n" "evaluate-commands -client ${kak_client:-client0} %{ try %{ buffer *plug* } catch %{ plug-list noupdate } }" | kak -p ${kak_session}
    sleep 0.3

    lockfile="${kak_opt_plug_install_dir}/.${plugin_name:-global}.plug.kak.lock"
    if [ -d "${lockfile}" ]; then
        printf "%s\n" "evaluate-commands -client ${kak_client:-client0} %{ plug-update-fifo %{${plugin_name}} %{Waiting for .plug.kak.lock} }" | kak -p ${kak_session}
    fi

    # this creates the lockfile for a plugin, if specified to prevent several processes of installation
    # of the same plugin, but will allow install different plugins without waiting for eachother.
    # Should be fine, since different plugins doesn't interfere with eachother.
    while ! mkdir "${lockfile}" 2>/dev/null; do sleep 1; done
    trap "rmdir '${lockfile}'" EXIT

    # if plugin specified as an argument add it to the *plug* buffer, if it isn't there already
    # otherwise update all plugins
    if [ -n "${plugin}" ]; then
        plugin_list=${plugin}
        printf "%s\n" "evaluate-commands -buffer *plug* %{
            try %{
                execute-keys /${plugin}<ret>
            } catch %{
                execute-keys gjO${plugin}:<space>Not<space>installed<esc>
            }
        }" | kak -p ${kak_session}
        sleep 0.2
    else
        plugin_list=${kak_opt_plug_plugins}
    fi

    for plugin in ${plugin_list}; do
        plugin_name="${plugin##*/}"
        git_domain=${kak_opt_plug_git_domain}

        eval "set -- ${kak_quoted_opt_plug_domains}"
        while [ $# -ne 0 ]; do
            if [ "$1" = "${plugin_name}" ]; then
                git_domain="https://$2"
                break
            fi
            shift
        done

        if [ ! -d "${kak_opt_plug_install_dir}/${plugin_name}" ]; then
            case ${plugin} in
                (http*|git*)
                    git="git clone ${plugin}" ;;
                (*)
                    git="git clone ${git_domain}/${plugin}" ;;
            esac

            (
                plugin_log="${TMPDIR:-/tmp}/${plugin_name}-log"
                printf "%s\n" "hook global -always KakEnd .* %{ nop %sh{rm -rf ${plugin_log}}}
                               evaluate-commands -client ${kak_client:-client0} %{ plug-update-fifo %{${plugin_name}} %{Installing} }" | kak -p ${kak_session}
                cd ${kak_opt_plug_install_dir} && ${git} >>${plugin_log} 2>&1
                status=$?
                if [ ${status} -ne 0 ]; then
                    printf "%s\n" "evaluate-commands -client ${kak_client:-client0} %{ plug-update-fifo %{${plugin_name}} %{Download Error (${status})} }" | kak -p ${kak_session}
                else
                    printf "%s\n" "evaluate-commands -client ${kak_client:-client0} plug-eval-hooks ${plugin_name}
                                   evaluate-commands -client ${kak_client:-client0} plug ${plugin}" | kak -p ${kak_session}
                fi
            ) > /dev/null 2>&1 < /dev/null &
        fi
        # this is a hacky way to measure amount of active processes. We need this
        # because dash shell has this long term bug: https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=482999
        jobs > ${jobs}; active=$(wc -l < ${jobs})
        while [ ${active} -ge ${kak_opt_plug_max_active_downloads} ]; do
            sleep 1
            jobs > ${jobs}; active=$(wc -l < ${jobs})
        done
    done
    wait
    rm -rf ${jobs}
) > /dev/null 2>&1 < /dev/null & }}

define-command -override -docstring \
"plug-update [<plugin>]: Update plugin.
If <plugin> omitted all installed plugins are updated" \
plug-update -params ..1 -shell-script-candidates %{ printf "%s\n" ${kak_opt_plug_plugins} | tr ' ' '\n' } %{
    evaluate-commands %sh{ (
        plugin=$1
        plugin_name="${plugin##*/}"
        jobs=$(mktemp ${TMPDIR:-/tmp}/jobs.XXXXXX)

        [ -z "${GIT_TERMINAL_PROMPT}" ] && export GIT_TERMINAL_PROMPT=0

        printf "%s\n" "evaluate-commands -client ${kak_client:-client0} %{ try %{ buffer *plug* } catch %{ plug-list noupdate } }" | kak -p ${kak_session}

        lockfile="${kak_opt_plug_install_dir}/.${plugin_name:-global}.plug.kak.lock"
        if [ -d "${lockfile}" ]; then
            printf "%s\n" "evaluate-commands -client ${kak_client:-client0} %{ plug-update-fifo %{${plugin##*/}} %{Waiting for .plug.kak.lock} }" | kak -p ${kak_session}
        fi

        while ! mkdir "${lockfile}" 2>/dev/null; do sleep 1; done
        trap "rmdir '${lockfile}'" EXIT

        [ -n "${plugin}" ] && plugin_list=${plugin} || plugin_list=${kak_opt_plug_plugins}
        for plugin in ${plugin_list}; do
            plugin_name="${plugin##*/}"
            if [ -d "${kak_opt_plug_install_dir}/${plugin_name}" ]; then
                (
                    plugin_log="${TMPDIR:-/tmp}/${plugin_name}-log"
                    printf "%s\n" "hook global -always KakEnd .* %{ nop %sh{rm -rf ${plugin_log}}}
                                   evaluate-commands -client ${kak_client:-client0} %{ plug-update-fifo %{${plugin_name}} %{Updating} }" | kak -p ${kak_session}
                    cd "${kak_opt_plug_install_dir}/${plugin_name}" && rev=$(git rev-parse HEAD) && git pull >> ${plugin_log} 2>&1
                    status=$?
                    if [ ${status} -ne 0 ]; then
                        printf "%s\n" "evaluate-commands -client ${kak_client:-client0} %{ plug-update-fifo %{${plugin_name}} %{Update Error (${status})} }" | kak -p ${kak_session}
                    else
                        if [ ${rev} != $(git rev-parse HEAD) ]; then
                            printf "%s\n" "evaluate-commands -client ${kak_client:-client0} plug-eval-hooks ${plugin_name}" | kak -p ${kak_session}
                        else
                            printf "%s\n" "evaluate-commands -client ${kak_client:-client0} %{ plug-update-fifo %{${plugin_name}} %{Done} }" | kak -p ${kak_session}
                        fi
                    fi
                ) > /dev/null 2>&1 < /dev/null &
            fi
            jobs > ${jobs}; active=$(wc -l < ${jobs})
            # TODO: re-check this
            # for some reason I need to multiply the amount of jobs by five here.
            while [ ${active} -ge $(expr ${kak_opt_plug_max_active_downloads} \* 5) ]; do
                sleep 1
                jobs > ${jobs}; active=$(wc -l < ${jobs})
            done
        done
        rm -rf ${jobs}
        wait
    ) > /dev/null 2>&1 < /dev/null & }
}

define-command -override -docstring \
"plug-clean [<plugin>]: delete <plugin>.
If <plugin> omitted deletes all plugins that are installed but not presented in configuration files" \
plug-clean -params ..1 -shell-script-candidates %{ ls -1 ${kak_opt_plug_install_dir} } %{ nop %sh{ (
    plugin=$1
    plugin_name="${plugin##*/}"

    printf "%s\n" "evaluate-commands -client ${kak_client:-client0} %{ try %{ buffer *plug* } catch %{ plug-list noupdate } }" | kak -p ${kak_session}

    lockfile="${kak_opt_plug_install_dir}/.${plugin_name:-global}.plug.kak.lock"
    if [ -d "${lockfile}" ]; then
        printf "%s\n" "evaluate-commands -client ${kak_client:-client0} %{ plug-update-fifo %{${plugin_name}} %{Waiting for .plug.kak.lock} }" | kak -p ${kak_session}
    fi

    while ! mkdir "${lockfile}" 2>/dev/null; do sleep 1; done
    trap "rmdir '${lockfile}'" EXIT

    if [ -n "${plugin}" ]; then
        if [ -d "${kak_opt_plug_install_dir}/${plugin_name}" ]; then
            (
                cd "${kak_opt_plug_install_dir}" && rm -rf "${plugin_name}"
                printf "%s\n" "evaluate-commands -client ${kak_client:-client0} %{ plug-update-fifo %{${plugin_name}} %{Deleted} }" | kak -p ${kak_session}
            )
        else
            printf "%s\n" "evaluate-commands -client ${kak_client:-client0} echo -markup %{{Error}No such plugin '${plugin}'}" | kak -p ${kak_session}
            exit
        fi
    else
        for installed_plugin in $(printf "%s\n" ${kak_opt_plug_install_dir}/*); do
            skip=
            for enabled_plugin in ${kak_opt_plug_plugins}; do
                [ "${installed_plugin##*/}" = "${enabled_plugin##*/}" ] && { skip=1; break; }
            done
            [ "${skip}" = "1" ] || plugins_to_remove=${plugins_to_remove}" ${installed_plugin}"
        done
        for plugin in ${plugins_to_remove}; do
            printf "%s\n" "evaluate-commands -client ${kak_client:-client0} %{ plug-update-fifo %{${plugin##*/}} %{Deleted} }" | kak -p ${kak_session}
            rm -rf ${plugin}
        done
    fi
) > /dev/null 2>&1 < /dev/null & }}

define-command -override -hidden \
-docstring "plug-eval-hooks: wrapper for post update/install hooks" \
plug-eval-hooks -params 1 %{ nop %sh{ (
    status=0
    plugin_name="$1"
    eval "set -- ${kak_quoted_opt_plug_post_hooks}"
    while [ $# -gt 0 ]; do
        if [ "$1" = "${plugin_name}" ]; then
            plugin_name="${1##*/}"
            plugin_log="${TMPDIR:-/tmp}/${plugin_name}-log"
            cd "${kak_opt_plug_install_dir}/${plugin_name}"
            printf "%s\n" "hook global -always KakEnd .* %{ nop %sh{rm -rf ${plugin_log}}}
                           evaluate-commands -client ${kak_client:-client0} %{ plug-update-fifo %{${plugin_name}} %{Running post-update hooks} }" | kak -p ${kak_session}
            IFS='
'
            for cmd in $2; do
                eval "${cmd}" >> ${plugin_log} 2>&1
                status=$?
                if [ ! ${status} -eq 0 ]; then
                    break
                fi
            done

            if [ ${status} -ne 0 ]; then
                printf "%s\n" "evaluate-commands -client ${kak_client:-client0} %{ echo -debug %{plug.kak: error occured while evaluation of post-update hooks for ${plugin_name}. Aborting with error code ${status}} }" | kak -p ${kak_session}
            fi
            break
        fi
        shift
    done

    if [ ${status} -ne 0 ]; then
        message="Error (${status})"
    else
        message="Done"
    fi
    printf "%s\n" "evaluate-commands -client ${kak_client:-client0} %{ plug-update-fifo %{${plugin_name}} %{${message}} }" | kak -p ${kak_session}
) > /dev/null 2>&1 < /dev/null & }}

define-command -override \
-docstring "plug-list [<noupdate>]: list all installed plugins in *plug* buffer. Checks updates by default unless <noupdate> is specified." \
plug-list -params ..1 %{ evaluate-commands -try-client %opt{toolsclient} %sh{
    noupdate=$1
    tmp=$(mktemp -d "${TMPDIR:-/tmp}/plug-kak.XXXXXXXX")
    fifo="${tmp}/fifo"
    plug_buffer="${tmp}/plug-buffer"
    mkfifo ${fifo}

    printf "%s\n" "edit! -fifo ${fifo} *plug*
                   set-option buffer filetype plug
                   plug-show-help
                   hook -always -once buffer BufCloseFifo .* %{ nop %sh{ rm -rf ${tmp} } }
                   map buffer normal '<ret>' ':<space>plug-fifo-operate install-update<ret>'
                   map buffer normal 'H' ':<space>plug-show-help<ret>'
                   map buffer normal 'U' ':<space>plug-fifo-operate update<ret>'
                   map buffer normal 'I' ':<space>plug-fifo-operate install<ret>'
                   map buffer normal 'L' ':<space>plug-fifo-operate log<ret>'
                   map buffer normal 'D' ':<space>plug-fifo-operate clean<ret>'
                   map buffer normal 'R' ':<space>plug-fifo-operate hooks<ret>'"

    # get those plugins which were loaded by plug.kak
    eval "set -- ${kak_opt_plug_plugins}"
    while [ $# -gt 0 ]; do
        if [ -d "${kak_opt_plug_install_dir}/${1##*/}" ]; then
            printf "%s: Installed\n" "$1" >> ${plug_buffer}
        else
            printf "%s: Not installed\n" "$1" >> ${plug_buffer}
        fi
        shift
    done

    # get those plugins which have a directory at installation path, but wasn't mentioned in any config file
    for exitsting_plugin in $(printf "%s\n" ${kak_opt_plug_install_dir}/*); do
        if [ $(expr "${kak_opt_plug_plugins}" : ".*${exitsting_plugin##*/}.*") -eq 0 ]; then
            printf "%s: Not loaded\n" "${exitsting_plugin##*/}" >> ${plug_buffer}
        fi
    done

    ( sort ${plug_buffer} > ${fifo} )  > /dev/null 2>&1 < /dev/null &

    if [ -z "${noupdate}" ]; then
        (
            [ -z "${GIT_TERMINAL_PROMPT}" ] && export GIT_TERMINAL_PROMPT=0
            eval "set -- ${kak_opt_plug_plugins}"
            while [ $# -gt 0 ]; do
                plugin_dir="${1##*/}"
                if [ -d "${kak_opt_plug_install_dir}/${plugin_dir}" ]; then (
                    cd ${kak_opt_plug_install_dir}/${plugin_dir}
                    git fetch > /dev/null 2>&1
                    status=$?
                    if [ ${status} -eq 0 ]; then
                        LOCAL=$(git rev-parse @{0})
                        REMOTE=$(git rev-parse @{u})
                        BASE=$(git merge-base @{0} @{u})

                        if [ ${LOCAL} = ${REMOTE} ]; then
                            message="Up to date"
                        elif [ ${LOCAL} = ${BASE} ]; then
                            message="Update available"
                        elif [ ${REMOTE} = ${BASE} ]; then
                            message="Local changes"
                        else
                            message="Installed"
                        fi
                    else
                        message="Fetch Error (${status})"
                    fi
                    printf "%s\n" "evaluate-commands -client ${kak_client:-client0} %{ plug-update-fifo %{$1} %{${message}} }" | kak -p ${kak_session}
                ) > /dev/null 2>&1 < /dev/null & fi
                shift
            done
        ) > /dev/null 2>&1 < /dev/null &
    fi

}}

define-command -hidden -override \
-docstring "operate on *plug* buffer contents based on current cursor position" \
plug-fifo-operate -params 1 %{ evaluate-commands -save-regs t %{
    execute-keys -save-regs '' "<a-h><a-l>"
    set-register t %val{selection}
    evaluate-commands %sh{
        plugin="${kak_reg_t%:*}"
        case $1 in
        (install-update)
            if [ -d "${kak_opt_plug_install_dir}/${plugin##*/}" ]; then
                printf "%s\n" "plug-update ${plugin}"
            else
                printf "%s\n" "plug-install ${plugin}"
            fi ;;
        (update)
            if [ -d "${kak_opt_plug_install_dir}/${plugin##*/}" ]; then
                printf "%s\n" "plug-update ${plugin}"
            else
                printf "%s\n" "echo -markup %{{Information}'${plugin}' is not installed}"
            fi ;;
        (install)
            if [ ! -d "${kak_opt_plug_install_dir}/${plugin##*/}" ]; then
                printf "%s\n" "plug-install ${plugin}"
            else
                printf "%s\n" "echo -markup %{{Information}'${plugin}' already installed}"
            fi ;;
        (clean)
            printf "%s\n" "plug-clean ${plugin}" ;;
        (log)
            printf "%s\n" "plug-display-log ${plugin}" ;;
        (hooks)
            printf "%s\n" "plug-eval-hooks ${plugin##*/}" ;;
        (*)
            ;;
        esac
    }
}}

define-command -hidden -override \
-docstring "plug-update-fifo <plugin> <message>" \
plug-update-fifo -params 2 %{ evaluate-commands -buffer *plug* -save-regs "/""" %{ try %{
    set-register / "%arg{1}: "
    set-register dquote %arg{2}
    execute-keys /<ret>lGlR
}}}

define-command -hidden -override \
plug-display-log -params 1 %{ evaluate-commands %sh{
    plugin_log="${TMPDIR:-/tmp}/${1##*/}-log"
    [ -s "${plugin_log}" ] && printf "%s\n" "edit! -existing -debug -readonly -scroll %{${plugin_log}}"
}}

define-command -override \
-docstring "displays help message" \
plug-show-help %{
    info -title "plug.kak Help" "h,j,k,l: Move
<ret>:   Update or Install plugin
I:       Install plugin
U:       Update plugin
D:       clean (Delete) plugin
L:       show Log, if any
R:       Run post-update hooks manually
H        show Help message"
}
