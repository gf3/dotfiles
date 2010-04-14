" Vim syntax file
" Language:     PHP 4/5
" Maintainer:   Peter Hodge <toomuchphp-vim@yahoo.com>
" Last Change:  May 7, 2008
"
" URL:      http://www.vim.org/scripts/script.php?script_id=1571
" Version:  0.9.7
"
" ================================================================
"
" Note: If you are having speed issues loading or editting PHP files, try
"       disabling some of the options.  In your .vimrc, try adding:
"         :let php_folding = 0
"         :let php_strict_blocks = 0
"
"       Also, there is a 'large file' mode which can disable certain options
"       if a PHP file is too big (see php_large_file below).
"
" ================================================================
"
" Note: If you are using a colour terminal with a dark background, you will
"       probably find the 'elflord' colorscheme will give you the most
"       distinctive colors.
"
" ================================================================
"
" OPTIONS:
"
"    Many of these default to 1 (On), so you would need to set
"	 them to 0 to turn them off. E.g., in your .vimrc file:
"      let php_special_vars = 0
"      let php_special_functions = 0
"      let php_alt_comparisons = 0
"      etc.
"    If the variables do not exist, the default value will be used.
"
"
"    All options can be set globally or on a per-file basis by using
"    global or buffer-local variables.  For example, you could turn on
"    automatic folding for all PHP scripts in your .vimrc:
"      let g:php_folding = 3
"
"    Then you could turn it off in only one file using this command:
"
"      :let b:php_folding = 0 | setfiletype php
"
"
"   -- PHP FEATURES --
"
"         let php_sql_query = 1/0  [default 0]
"             ... for SQL syntax highlighting inside strings
"
"         let php_htmlInStrings = 1/0  [default 0]
"             ... for HTML syntax highlighting inside strings
"
"         let php_baselib = 1/0  [default 0]
"             ... for highlighting baselib functions
"
"         let php_special_vars = 1/0  [default 1]
"             ... to highlight superglobals like $_GET, $_SERVER, etc.
"               * always on if using php_oldStyle
"
"         let php_special_functions = 1/0  [default 1]
"             ... to highlight functions with special behaviour
"                 e.g., unset(), extract()
"
"         let php_alt_comparisons = 1/0  [default 1]
"             ... to highlight comparison operators in an alternate colour
"               * always on if using php_oldStyle
"
"         let php_alt_assignByReference = 1/0  [default 1]
"             ... to highlight '=&' in '$foo =& $bar' in an alternate colour.
"                 This also applies to a function which returns by-reference,
"                 as well as a function argument which is by-reference.
"
"         let php_smart_members = 1/0  [default 0]
"             ... syntax works out whether -> indicates a property or method.
"                 Otherwise method colours may be used on properties, for
"                 example:
"                   $object->__get[] = true;
"                 '__get' would be highlighted as a special method, even
"                 thought it is clearly a property in this context.
"                 Note: turning this OFF can improve the highlighting speed on
"                 object-oriented code
"
"         let php_alt_properties = 1/0  [default 0]
"             ... use a different color for '->' based on whether it is used
"                 for property access, method access, or dynamic access (using
"                 '->{...}')
"               * requires php_smart_members
"
"         let php_highlight_quotes = 1/0  [default 0]
"             ... makes quote characters the same colour as the string
"                 contents, like in C syntax.
"
"         let php_show_semicolon = 1/0  [default 1]
"             ... to make the semicolon (;) more visible
"
"         let php_smart_semicolon = 1/0  [default 1]
"             ... semicolon adopts the color of a 'return' or 'break' keyword
"               * requires php_show_semicolon
"                 Note: this also includes the ':' or ';' which follows a
"                 'case' or 'default' inside a switch
"
"         let php_alt_blocks = 1/0  [default 1]
"             ... colorize { and } around class/function/try/catch bodies
"                 according to the type of code block.
"               * requires php_strict_blocks
"
"         let php_alt_arrays = 0/1/2  [default 1]
"             ... to colorize ( and ) around an array body, as well as '=>'
"               * requires php_strict_blocks
"                 Setting this to '2' will highlighting the commas as well.
"                 Commas are not highlighted by default because it's too much
"                 color.
"
"         let php_alt_construct_parents = 0/1  [default 0]
"             ... to colorize the ( and ) around an if, while, foreach, or switch
"                 body.
"               * requires ... what?
"                 TODO: work out dependencies, code them correctly
"
"         let php_show_spl = 1/0  [default 1]
"             .. to colorize methods which are defined by PHP default interfaces
"                TODO: work out what these interfaces are part of: SPL or just
"                PHP
"
"         let php_show_pcre = 1/0  [default 1]
"                 (was: 'php_show_preg')
"             ... to highlight regular expression patterns inside calls
"                 to preg_match(), preg_replace(), etc.
"
"
"   -- FINDING ERRORS --
"
"         let php_parent_error_close = 1/0  [default 1]
"             ... for highlighting parent error ] or ) or }
"               * requires php_strict_blocks
"
"         let php_parent_error_open = ?
"             ... for skipping a php end tag, if there exists an
"                 open ( or [ without a closing one
"                 Note: this option is now enabled permanently (unless
"                 php_strict_blocks is disabled).
"               * requires php_strict_blocks
"
"         let php_empty_construct_error = 0/1  [default 1]
"             ... highlights ';' as an error if it comes immediately after
"                 an if/else/while/for/foreach/switch statement (the
"                 logical constructs should have a body).
"               * requires php_strict_blocks

"         let php_show_semicolon_error = 1/0  [default 1]
"             ... highlights certain cases when ';' is followed by an
"                 operator such as '+'.
"               * requires php_show_semicolon
"
"         let php_nested_functions = 0/1  [default 0]
"             ... Whether or not to allow contiaining one function inside
"                 another.  This option is mostly for speeding up syntax
"                 highlighting - { } blocks can by inserted much faster while
"                 editting a large class.
"                 Note: this is the only option which might break valid PHP
"                 code, although only if you define one function inside
"                 another, which is usually discouraged.
"               * only relevant when using php_strict_blocks
"
"   -- OTHER OPTIONS --
"
"         let php_large_file = 0/?  [ default 3000 ]
"             ... If a PHP script has more lines than this limit (e.g., more
"                 than 3000 lines), some options are automatically turned off
"                 to help it load faster.  These options are:
"                   php_strict_blocks = 0
"                   php_smart_members = 0
"                   php_smart_semicolon = 0
"                   php_show_pcre = 0
"                   php_folding = 0
"                 Note: If you set this option to '0', then this feature will
"                 be disabled; thus you can use:
"                   :let b:php_large_file = 0 | setfiletype php
"                 to reload the current file with the disabled syntax options
"                 re-activated.
"
"         let php_strict_blocks = 1/0  [default 1]
"             ... to match together all {} () and [] blocks correctly. This is
"                 required by some of the other options for finding errors and
"                 applying correct colors to { } blocks, etc.  However, it may
"                 cause Vim to slow down on large files.
"
"         let php_asp_tags = 1/0  [default 0]
"             ... for highlighting ASP-style short tags: <% %>
"
"         let php_noShortTags = 1/0  [default 0]
"             ... don't sync <? ?> as php
"               * This has been deprecated in favour of php_short_tags (which
"                 has the opposite meaning)
"
"         let php_short_tags = 1/0  [default 1]
"             ... highlight <?...?> blocks as php. If this is OFF, you need to
"                 use the longer <?php...?>
"
"         let php_oldStyle = 1
"             ... for using old colorstyle
"
"         let php_folding = 1/2/3  [default 0]
"             ... 1: for folding classes and functions
"                 2: for folding all { } regions
"                 3: for folding only functions
"                 TODO: documentation for php_folding_manual
"
"         let php_fold_arrays = 0/1  [default 0]
"             ... To fold arrays as well.
"               * requires php_folding, php_strict_blocks, php_alt_arrays
"
"         let php_fold_heredoc = 0/1  [default 0]
"             ... Fold heredoc blocks ($string = <<<ENDOFSTRING)
"               * requires php_folding
"
"         let php_sync_method = x
"             ... x = -1 to sync by search (default)
"                 x > 0  to sync at least x lines backwards
"                 x = 0  to sync from start
"
"
" ================================================================
"
" TODO LIST:
"
" PRIORITY:
"   - document the phpControlParent feature, make sure it works fully, or not
"     at all
"   - what to do about 'iskeyword' containing '$'?
"   - test syntax against older Vims (6.4, 6.0, 5.7)
"   - concat rid of lines beginning with '\'
"   - allow turning off all semicolon errors
"   - fix bug in highlighting of pattern: /[\\\]/ and /[\\]]/
"   - fix bug in highlighting of pattern: "/\$/" should make the '$' match the
"     end-of-pattern
"   - case/default syntax items don't work inside a switch/endswitch
"     although it's still OK using {...}
"
" PHP Syntax:
"   - review support for PHP built-in superglobals:
"     - ensure all PHP 5.2 superglobals are recognized
"   - review support for PHP built-in constants, make sure I've got them all
"   - make support for PHP built-in class methods optional.
"   - review highlight links to ensure maximum readability on GUI
"     and color terminal using colorschemes 'elflord' and 'default'
"   - new group phpReservedFunction, because some parts of
"     phpSpecialFunction can be used as a method name.
"   - what is php_oldStyle? is it still needed?
"   - use 'nextgroup' to highlight errors when a ']' or ')' is followed by
"     a constant/variable or function call.  This will also help find when
"     a closing ')' is missing.
"   - Review syncing searches, see if there is anything we can add to help the
"     syncing process.
"   - make ';' on the end of a while() NOT a problem unless it is followed by
"     a '{'
"
" PCRE Syntax:
"   - fix problem: in regex "/(...)\1/", '\1' does NOT refer to a backreference!
"     this is caused by incorrect matching of escape sequences in
"     double-quoted strings where the double-quote escape sequences should
"     take precedence
"   - fix problem: this line breaks folding
"   		preg_match("#^require MODULES_PATH \. (['\"])main\.inc\.php\\1;$#m", '', $m))
"
"   - decide on terminology: 'PCRE' or 'PREG'
"   - Review effects of paired delimiters when combined with other
"     PCRE complex atoms using those symbols
"   - decide on better colors for the
"     (?(?=lookahead/lookbehind)conditional|sub-pattern)
"
"
" Future Plans:
"   - option to choose PHP4 or PHP5 compatibility
"   - differentiate between an interface block and a class block
"   - add ability to highlight user-defined functions and keywords
"     using any of the following means:
"       1) a comma-separated string of user functions
"       2) a file containing user functions
"       3) using names from the tags file(s)
"           (this may be better as a separate plugin)
"   - add support for phpDocumentor comment blocks and tags
"   - add support for POSIX Regex patterns
"   - allow easy embedding of the PHP syntax using 'contains=@phpClTop' or
"     'syn region phpRegion ...'
"   - automatically select best colors by examining the colorscheme
"   - check to see if 'baselib' needs to be highlighted still
"   - Add support 2nd parameter to preg_replace and highlight
"     "\1" and other confusing sequences as errors ("\1" is "\x01")
"   - Add support for class constants (foo::bar)
"
" Difficult To Implement:
"   - Highlight expressions without operators or commas, such as:
"       echo $a $b;
"     These expressions are *really* difficult to find efficiently.
"   - Folding around 'case' blocks.
"
"
"
" ================================================================
"
" Note:
" - syntax case is 'ignore' unless explicitly set to 'match'
"   for a single item.

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syn clear
elseif exists("b:current_syntax")
  finish
endif

if !exists("main_syntax")
  let main_syntax = 'php'
endif

" TODO: what's all this about?
if version < 600
  unlet! php_folding
  if exists("php_sync_method") && !php_sync_method
    let php_sync_method=-1
  endif
  so <sfile>:p:h/html.vim
else
  runtime! syntax/html.vim
  unlet b:current_syntax
endif

" accept old options
if !exists("php_sync_method")
  if exists("php_minlines")
    let php_sync_method=php_minlines
  else
    let php_sync_method=-1
  endif
endif

function! s:ChooseValue(name, default)
  if exists('b:' . a:name)
    return b:{a:name}
  elseif exists('g:' . a:name)
    return g:{a:name}
  else
    return a:default
  endif
endfunction

" set up variables based on global or buffer variables {{{1
  " Is it a large file? (s:large_file) {{{2
  let s:large_file_limit = s:ChooseValue('php_large_file', 3000)
  let s:large_file = (s:large_file_limit == 0) ? 0 : (line('$') >= s:large_file_limit)

  if s:large_file
    echohl WarningMsg
    echomsg printf('WARNING: Large PHP File (%d lines); some syntax options have been disabled.', line('$'))
    echohl None
  endif

  " Strict Blocks (s:strict_blocks) {{{2
  let s:strict_blocks = s:large_file ? 0 : s:ChooseValue('php_strict_blocks', 1)

  " Fold Level (s:folding) {{{2
  let s:folding = s:large_file ? 0 : s:ChooseValue('php_folding', 0)

  " Fold manually (s:fold_manual) {{{2
  let s:fold_manual = s:large_file ? 0 : s:ChooseValue('php_fold_manual', s:folding ? 1 : 0)

  " Fold arrays (s:fold_arrays) {{{2
  let s:fold_arrays = (s:folding && s:ChooseValue('php_fold_arrays', 0))

  " Fold heredoc strings (s:fold_heredoc) {{{2
  let s:fold_heredoc = (s:folding && s:ChooseValue('php_fold_heredoc', 0))

  " Allow nested functions (s:nested_functions) {{{2
  let s:nested_functions = s:ChooseValue('php_nested_functions', 1)

  " Allow ASP-style <% %> tags (s:asp_tags) {{{2
  let s:asp_tags = s:ChooseValue('php_asp_tags', 0)

  " Only allow long tags '<?php' (s:long_tags) {{{2
  let s:long_tags = !s:ChooseValue('php_short_tags', !s:ChooseValue('php_noShortTags', 0))

  " SQL in strings (s:show_sql) {{{2
  let s:show_sql = s:ChooseValue('php_sql_query', 0)

  " HTML in strings (s:show_html_in_strings) {{{2
  let s:show_html_in_strings = s:ChooseValue('php_htmlInStrings', 0)

  " Highlight the PHP baselib (s:show_baselib) {{{2
  let s:show_baselib = s:ChooseValue('php_baselib', 0)

  " Highlight superglobals (s:special_vars) {{{2
  let s:special_vars = s:ChooseValue('php_special_vars', s:ChooseValue('php_special_variables', s:ChooseValue('php_oldStyle', 1)))

  " Highlight special functions (s:special_functions) {{{2
  let s:special_functions = s:ChooseValue('php_special_functions', 1)

  " Highlight quotes around strings (s:show_quotes) {{{2
  let s:show_quotes = s:ChooseValue('php_highlight_quotes', 0)

  " Highlight PCRE patterns (s:show_pcre) {{{2
  let s:show_pcre = s:large_file ? 0 : s:ChooseValue('php_show_pcre', s:ChooseValue('php_show_preg', 1))

  " Highlight ';' (s:show_semicolon) {{{2
  let s:show_semicolon = s:ChooseValue('php_show_semicolon', 1)

  " Highlight ';' errors (s:show_semicolon_error) {{{2
  let s:show_semicolon_error = (s:show_semicolon && s:ChooseValue('php_show_semicolon_error', 1))

  " Highlight parent error ) ] or } (s:parent_error_close) {{{2
  let s:parent_error_close = (s:strict_blocks && s:ChooseValue('php_parent_error_close', 1))

  " Highlight invalid ';' after if/while/foreach (s:no_empty_construct) {{{2
  let s:no_empty_construct = (s:strict_blocks && s:ChooseValue('php_empty_construct_error', 1))

  " Alt colors for {} around class/func/try/catch blocks (s:alt_blocks) {{{2
  let s:alt_blocks = (s:strict_blocks && s:ChooseValue('php_alt_blocks', 1))

  " Alt color for by-reference operators (s:alt_refs) {{{2
  let s:alt_refs = s:ChooseValue('php_alt_assignByReference', 1)

  " Alt color for control structure parents (s:alt_control_parents) {{{2
  let s:alt_control_parents = s:ChooseValue('php_alt_construct_parents', 0)

  " Alt color for array syntax (s:alt_arrays) {{{2
  " * requires strict_blocks
  let s:alt_arrays = (s:strict_blocks ? s:ChooseValue('php_alt_arrays', 1) : 0)

  " Alt color for comparisons (s:alt_comparisons) {{{2
  let s:alt_comparisons = s:ChooseValue('php_alt_comparisons', s:ChooseValue('php_oldStyle', 1))

  " Alt colors for ';' after return/break (s:smart_semicolon) {{{2
  let s:smart_semicolon = s:large_file ? 0 : (s:show_semicolon && s:ChooseValue('php_smart_semicolon', 1))

  " Alt colors for '->' (s:smart_members / s:alt_properties) {{{2
  let s:smart_members = s:large_file ? 0 : s:ChooseValue('php_smart_members', 0)
  let s:alt_properties = (s:smart_members && s:ChooseValue('php_alt_properties', 0))

  " Syncing method (s:sync) {{{2
  let s:sync = s:ChooseValue('php_sync_method', -1)

" }}}1

delfunction s:ChooseValue

syn cluster htmlPreproc add=phpRegion,phpRegionAsp,phpRegionSc

" need to source the SQL syntax file in case we encounter
" heredoc containing SQL
if version < 600
  syn include @sqlTop <sfile>:p:h/sql.vim
else
  syn include @sqlTop syntax/sql.vim
endif

syn sync clear
unlet b:current_syntax
syn cluster sqlTop remove=sqlString,sqlComment
if s:show_sql
  syn cluster phpClShowInStrings contains=@sqlTop
endif

if s:show_html_in_strings
  syn cluster phpClShowInStrings add=@htmlTop
endif

" NOTE: syntax case should be 'ignore' for all items (in keeping
" with PHP's case insensitive behaviour).  If an item MUST be case
" sensitive, 'syntax case match' should precede that item and
" 'syntax case ignore' must follow IMMEDIATELY after so that there
" can be no confusion over the value of 'syntax case' for most items
" syntax matches and regions may use '\C' to utilize case sensitivity
syn case ignore

" PHP syntax: clusters {{{1

  " these represent a single value in PHP
  syn cluster phpClValues add=@phpClConstants

  syn cluster phpClValues add=phpIdentifier,phpIdentifierComplex,phpMethodsVar

  " TODO: add class constants (foo::BAR)

  " these can go anywhere an expression is valid inside PHP code
  syn cluster phpClExpressions add=@phpClValues
  syn cluster phpClExpressions add=phpRelation,phpList
  syn cluster phpClExpressions add=phpParent,phpParentError
  syn cluster phpClExpressions add=phpObjectOperator

  " these are the control statements - they shouldn't contain each other
  syn cluster phpClCode add=@phpClExpressions
  syn cluster phpClCode add=phpLabel
  syn cluster phpClCode add=phpFoldTry,phpFoldCatch

  " TODO: what is phpStorageClass class exactly? it needs a more descriptive
  " name so I know where to include it.  Maybe it should be broken up
  syn cluster phpClCode add=phpStorageClass

  " code which is inside a function/method/class or global scope
  syn cluster phpClInFunction add=@phpClCode
  syn cluster phpClInMethod add=@phpClCode
  syn cluster phpClInClass add=@phpClValues,phpAssign,phpSemicolon
  syn cluster phpClTop add=@phpClCode

  " This cluster contains matches to find where an expression follows another
  " expression without a comma or operator inbetween
  " TODO: is this even possible to do?
  syn cluster phpClExprNotSeparated add=NONE

  " Note: there is one cluster here each for:
  "  - constants
  "  - functions
  "  - classes
  "  - interfaces
  "  - structures (classes and interfaces combined)
  "  - methods
  "  - properties
  "  - members (things which are a method and a property)
  syn cluster phpClConstants add=NONE
  syn cluster phpClFunctions add=NONE

  syn cluster phpClClasses add=NONE
  syn cluster phpClInterfaces add=NONE
  syn cluster phpClStructures add=@phpClClasses,@phpClInterfaces

  syn cluster phpClMethods add=@phpClMembers
  syn cluster phpClProperties add=@phpClMembers
  syn cluster phpClMembers add=NONE

  " these are the things which can be matched inside an identifier
  syn cluster phpClIdentifier add=NONE

  " Note: the next clusters contains all the regions or matches that can
  " contain a class or interface name
  syn cluster phpClClassHere add=phpStructureHere
  syn cluster phpClInterfaceHere add=phpStructureHere
  syn cluster phpClStructureHere add=@phpClClassHere,@phpClStructureHere

  syn cluster phpClMethodHere add=phpMemberHere,phpMethodHere
  syn cluster phpClPropertyHere add=phpMemberHere,phpPropertyHere
  syn cluster phpClMemberHere add=@phpClMethodHere,@phpClPropertyHere

  " also, some very basic matches for these place-holders
  syn match phpStructureHere /\h\w*/ contained display
  syn match phpMemberHere /\h\w*/ contained display
  syn match phpMethodHere /\h\w*/ contained display
  syn match phpPropertyHere /\h\w*/ contained display

  " what to include at the top level?
  if ! s:strict_blocks
    " if not strict blocks, we must also allow matching of things from inside
    " functions/methods/classes
    syn cluster phpClTop add=@phpClInFunction,@phpClInMethod,@phpClInClass
  endif
  
  " Note: these are the old clusters ... they are deprecated now, but still
  " here just in case someone is using them elsewhere
  syn cluster phpClInside add=@phpClExpressions
  syn cluster phpClConst add=@phpClExpressions
  syn cluster phpClFunction add=@phpClCode

" PHP syntax: comments {{{1

  syn cluster phpClValues add=phpComment
  syn cluster phpClInClass add=phpComment

  syn region phpComment start="/\*" end="\*/" contained extend contains=@Spell
  if version >= 600
    syn match phpComment "#.\{-}\(?>\|$\)\@="  contained extend contains=@Spell
    syn match phpComment "//.\{-}\(?>\|$\)\@=" contained extend contains=@Spell
  else
    syn match phpComment "#.\{-}$"         contained
    syn match phpComment "#.\{-}?>"me=e-2  contained
    syn match phpComment "//.\{-}$"        contained
    syn match phpComment "//.\{-}?>"me=e-2 contained
  endif

" PHP syntax: Operators: + - * / && || (etc) {{{1

  " NOTE: these need to come before the numbers (so that floats work)
  syn cluster phpClExpressions add=phpAssign
  syn match phpAssign /==\@!/ contained display
  hi link phpAssign phpOperator

  syn cluster phpClExpressions add=phpOperator
  syn match   phpOperator contained display /[%^|*!.~]/
  " & can't be preceded by a '=' or ',' or '('
  syn match   phpOperator contained display /\%([=,(]\_s*\)\@<!&/
  " + can't be followed by a number or '.'
  " - can't be followed by a number or '.' or '>'
  syn match   phpOperator contained display /+[.0-9]\@!/
  syn match   phpOperator contained display /-[.>0-9]\@!/
  syn match   phpOperator contained display /[-+*/%^&|.]=/
  syn match   phpOperator contained display /\/[*/]\@!/
  syn match   phpOperator contained display /&&/
  syn match   phpOperator contained display /||/
  syn match   phpOperator contained display />>/
  " Note: how the shift-left operator can't be followed by another '<' (that
  " would make it a Heredoc syntax)
  syn match   phpOperator contained display /<<<\@!/
  syn keyword phpOperator	contained and or xor

  " allow a ':' on its own - this may be after an 'else:' or something like that
  syn match phpOperator contained display /::\@!/

  syn cluster phpClExpressions add=phpTernaryRegion
  syn region phpTernaryRegion matchgroup=phpOperator start=/?/ end=/::\@!/
        \ transparent keepend extend display
        \ contained matchgroup=Error end=/[;)\]}]/


" PHP syntax: null/true/false/numbers {{{1

  syn cluster phpClValues add=phpNull
  syn keyword phpNull contained null

  syn cluster phpClValues add=phpBoolean
  syn keyword phpBoolean contained true false

  syn cluster phpClValues add=phpNumber
  syn match phpNumber contained display /\<[1-9]\d*\>/
  syn match phpNumber contained display /-[1-9]\d*\>/
  syn match phpNumber contained display /+[1-9]\d*\>/
  syn match phpNumber contained display /\<0x\x\{1,8}\>/
  syn match phpNumber contained display /\<0\d*\>/ contains=phpOctalError
  syn match phpOctalError contained display /[89]/

  " Note: I've split float into 3 matches which each have a fixed starting
  " character
  syn cluster phpClValues add=phpFloat
  syn match phpFloat contained display /\.\d\+\>/
  syn match phpFloat contained display /\<\d\+\.\d\+\>/
  syn match phpFloat contained display /-\d*\.\d\+\>/
  syn match phpFloat contained display /+\d*\.\d\+\>/

" PHP syntax: dynamic strings {{{1

  " single-quoted strings are always the same
  " TODO: work out a good name for an option which will add 'display' option
  " to these strings
  syn cluster phpClValues add=phpStringSingle
  syn region phpStringSingle matchgroup=phpQuoteSingle   start=/'/ skip=/\\./ end=/'/
        \ contained keepend extend contains=@phpClShowInStrings

  " Note: this next version of the php double-quoted string can't contain
  " variables, because a few parts of PHP code require strings without
  " variables.  There is another string match later which takes precedence
  " over this one in most circumstances where a string containing variables IS
  " allowed
  syn cluster phpClValues add=phpStringDoubleConstant
  syn region phpStringDoubleConstant contained keepend extend
        \ matchgroup=phpQuoteSingle start=/"/ end=/"/ skip=/\\./
        \ contains=@phpClShowInStrings,phpSpecialChar

  " Note: this version of the double-quoted string also contains
  " @phpClStringIdentifiers, which means variables can go inside the string
  syn cluster phpClExpressions add=phpStringDouble
  syn region phpStringDouble matchgroup=phpQuoteDouble   start=/"/ skip=/\\./ end=/"/
        \ contained keepend extend
        \ contains=@phpClShowInStrings,phpSpecialChar,@phpClStringIdentifiers

  " backticks
  syn cluster phpClExpressions add=phpBacktick
  syn region phpBacktick matchgroup=phpQuoteBacktick start=/`/ skip=/\\./ end=/`/
        \ contained keepend extend
        \ contains=phpSpecialChar,@phpClStringIdentifiers

  " this cluster contains only strings which accept things like \n or \x32
  " inside them
  syn cluster phpClStringsWithSpecials add=phpStringDoubleConstant,@phpClStringsWithIdentifiers

  " this cluster contains strings which accept things like {$foo} inside them
  syn cluster phpClStringsWithIdentifiers add=phpStringDouble,phpBacktick,phpHereDoc

  " HereDoc {{{
  if version >= 600
    syn cluster phpClExpressions add=phpHereDoc
    syn case match
"    syn keyword phpHereDoc contained foobar
    if s:folding && s:fold_heredoc
      syn region phpHereDoc keepend extend contained
            \ matchgroup=phpHereDocDelimiter end=/^\z1\%(;\=$\)\@=/
            \ start=/<<<\s*\z(\h\w*\)$/
            \ contains=phpSpecialChar
            \ fold

      " always match special Heredocs which state what type of content they have
      syn region phpHereDoc keepend extend contained
            \ matchgroup=phpHereDocDelimiter end=/^\z1\%(;\=$\)\@=/
            \ start=/\c<<<\s*\z(\%(\h\w*\)\=html\)$/
            \ contains=@htmlTop,phpSpecialChar
            \ fold
      syn region phpHereDoc keepend extend contained
            \ matchgroup=phpHereDocDelimiter end=/^\z1\%(;\=$\)\@=/
            \ start=/\c<<<\s*\z(\%(\h\w*\)\=sql\)$/
            \ contains=@sqlTop,phpSpecialChar
            \ fold
      syn region phpHereDoc keepend extend contained
            \ matchgroup=phpHereDocDelimiter end=/^\z1\%(;\=$\)\@=/
            \ start=/\c<<<\s*\z(\%(\h\w*\)\=javascript\)$/
            \ contains=@htmlJavascript,phpSpecialChar
            \fold
    else
      syn region phpHereDoc keepend extend contained
            \ matchgroup=phpHereDocDelimiter end=/^\z1\%(;\=$\)\@=/
            \ start=/<<<\s*\z(\h\w*\)$/
            \ contains=phpSpecialChar

      " always match special Heredocs which state what type of content they have
      syn region phpHereDoc keepend extend contained
            \ matchgroup=phpHereDocDelimiter end=/^\z1\%(;\=$\)\@=/
            \ start=/\c<<<\s*\z(\%(\h\w*\)\=html\)$/
            \ contains=@htmlTop,phpSpecialChar
      syn region phpHereDoc keepend extend contained
            \ matchgroup=phpHereDocDelimiter end=/^\z1\%(;\=$\)\@=/
            \ start=/\c<<<\s*\z(\%(\h\w*\)\=sql\)$/
            \ contains=@sqlTop,phpSpecialChar
      syn region phpHereDoc keepend extend contained
            \ matchgroup=phpHereDocDelimiter end=/^\z1\%(;\=$\)\@=/
            \ start=/\c<<<\s*\z(\%(\h\w*\)\=javascript\)$/
            \ contains=@htmlJavascript,phpSpecialChar
    endif

    syn case ignore
  endif " }}}


" PHP syntax: strings: special sequences {{{1

  " match an escaped backslash inside any type of string
  syn match phpStringLiteral /\\\\/ contained display
        \ containedin=phpStringSingle,@phpClStringsWithSpecials

  " match an escaped quote in each type of string
  syn match phpStringLiteral /\\`/ contained display containedin=phpBacktick
  syn match phpStringLiteral /\\'/ contained display containedin=phpStringSingle
  syn match phpStringLiteral /\\"/ contained display
        \ containedin=phpStringDouble,phpStringDoubleConstant

  " highlighting for an escaped '\$' inside a double-quoted string
  syn match phpStringLiteral /\\\$/ contained display containedin=@phpClStringsWithSpecials

  " match \{ as regular string content so that it stops \{$var} from
  " highlighting the { } region
  syn match phpStringRegular /\\{/ contained display containedin=@phpClStringsWithSpecials
  hi link phpStringRegular phpStringDouble

  " match an \r\n\t or hex or octal sequence
  " TODO: these should also be available in PCRE pattern strings
  " TODO: what were all these extra escape sequences???
  syn match phpSpecialChar contained display /\\[nrt]/
  syn match phpSpecialChar contained display /\\\o\{1,3}/
  syn match phpSpecialChar contained display /\\x\x\x\=/

  " create an identifier match for inside strings:
  syn match phpIdentifierInString /\$\h\w*/ contained display
        \ nextgroup=phpPropertyInString,phpPropertyInStringError,@phpClIdentifierIndexInString
        \ contains=phpIdentifier
        \ containedin=@phpClStringsWithIdentifiers

  " match an index [bar] [0] [$key] after a variable inside a string {{{2

    " First: match the [ and ] which would appear in the index (they're
    " contained int the following items)
    syn match phpBracketInString /[[\]]/ contained display
    hi link phpBracketInString phpSpecialChar

    " Second: match a single '[' as an error
    syn cluster phpClIdentifierIndexInString add=phpBracketInStringFalseStart
    syn match phpBracketInStringFalseStart /\[\]\=/ contained display
    hi link phpBracketInStringFalseStart Error

    " Third: match any other [] region which is completed, but doesn't have a
    " valid key
    syn cluster phpClIdentifierIndexInString add=phpIdentifierIndexInString
    syn match phpIdentifierIndexInString /\[[^"]\{-1,}\]/
          \ contains=phpBracketInString,phpIdentifierIndexInStringError
          \ contained display

      " error on a bracket which is inside another bracket
      syn match phpIdentifierIndexInStringError /\[[$_a-z0-9]*\[/ contained display
      hi link phpIdentifierIndexInStringError Error

      " any character which isn't a valid index character
      syn match phpIdentifierIndexInStringError /[^$a-z0-9_[\]]\+/ contained display

      " a number *must not* be preceded by the '[' and followed by a \w
      syn match phpIdentifierIndexInStringError /\[\@<=\d\+\w\@=/ contained display

      " a dollar sign *must* be preceded by the '['
      syn match phpIdentifierIndexInStringError /\[\@<!\$/ contained display
            \ containedin=phpIdentifierIndexInStringError

    " Fourth: match a complete '[]' region properly

      " 1) an index containing a number
      syn match phpIdentifierIndexInString /\[\d\+\]/ contained display
            \ contains=phpBracketInString

      " 2) a word or variable
      syn match phpIdentifierIndexInString /\[\$\=\h\w*\]/ contained display
            \ contains=phpBracketInString,phpVarSelector
      hi link phpIdentifierIndexInString Identifier

  " }}}2

  " match an property after a variable inside a string
  syn cluster phpClPropertyHere add=phpPropertyInString
  syn match phpPropertyInString /->\h\w*/ contained display extend
        \ contains=phpPropertySelector,@phpClProperties

  " it's sometimes easy to get it wrong
  syn match phpPropertyInStringError contained display /->\%([^a-z0-9_"\\]\|\\.\)/
        \ contains=phpPropertySelector
  syn match phpPropertyInStringError contained display /->"\@=/
  hi! link phpPropertyInStringError Error

  syn region phpIdentifierInStringComplex matchgroup=phpSpecialChar
        \ start=/{\$\@=/ start=/\${\w\@!/ end=/}/
        \ contained extend keepend contains=@phpClExpressions
        \ containedin=@phpClStringsWithIdentifiers

  syn region phpIdentifierInStringErratic contained extend
        \ matchgroup=phpSpecialChar start=/\${\w\@=/ end=/}/
        \ containedin=@phpClStringsWithIdentifiers
        \ contains=phpIdentifierErratic

  syn match phpIdentifierErratic /{\@<=\h\w*/ contained
        \ nextgroup=phpErraticBracketRegion
        \ nextgroup=phpIdentifierInStringErraticError skipwhite skipempty
  syn region phpErraticBracketRegion contained keepend extend contains=@phpClExpressions
        \ matchgroup=phpParent start=/\[/ end=/\]/ display
        \ matchgroup=Error end=/;/
        \ nextgroup=phpIdentifierInStringErraticError skipwhite skipempty

  syn match phpIdentifierInStringErraticError contained /[^ \t\r\n}]\+/
        \ nextgroup=phpIdentifierInStringErraticError skipwhite skipempty
  hi link phpIdentifierInStringErraticError Error


" PHP syntax: variables/identifiers ($foo) {{{1

  " one $
  syn match phpVarSelector      contained display /\$/
  " two $$
  syn match phpVarSelectorDeref contained display /\$\$/
  syn match phpVarSelectorError contained display /\$\$\$\+/

  " match a regular variable
  syn cluster phpClExpressions add=phpIdentifier
  syn match phpIdentifier /\$\h\w*/ contained display
        \ contains=phpVarSelector,@phpClIdentifier

  " match a dereference-variable ($$variable)
  syn match phpIdentifier /\$\$\+\h\w*/ contained display
        \ contains=@phpClIdentifier,phpVarSelectorDeref,phpVarSelectorError,phpDerefInvalid

  " you can't dereference these variables:
  syn match phpDerefInvalid contained display
        \ /\C\$\$\ze\%(this\|arg[cv]\|GLOBALS\)\>/
  syn match phpDerefInvalid contained display
        \ /\C\$\$\ze_\%(GET\|POST\|REQUEST\|COOKIE\|SESSION\|SERVER\|ENV\)\>/
  hi link phpDerefInvalid Error

  if s:special_vars

    syn case match

    " Superglobals: {{{2
    syn cluster phpClIdentifier add=phpSuperglobal
    syn match phpSuperglobal contained display /\$\@<=GLOBALS\>/
    syn match phpSuperglobal contained display /\$\@<=_GET\>/
    syn match phpSuperglobal contained display /\$\@<=_POST\>/
    syn match phpSuperglobal contained display /\$\@<=_COOKIE\>/
    syn match phpSuperglobal contained display /\$\@<=_REQUEST\>/
    syn match phpSuperglobal contained display /\$\@<=_FILES\>/
    syn match phpSuperglobal contained display /\$\@<=_SESSION\>/
    syn match phpSuperglobal contained display /\$\@<=_SERVER\>/
    syn match phpSuperglobal contained display /\$\@<=_ENV\>/
    syn match phpSuperglobal contained display /\$\@<=this\>/

    " Built In Variables: {{{2
    syn cluster phpClIdentifier add=phpBuiltinVar
    syn match phpBuiltinVar	contained display /\$\@<=argc\>/
    syn match phpBuiltinVar	contained display /\$\@<=argv\>/
    syn match phpBuiltinVar	contained display /\$\@<=php_errormsg\>/

    " Long Arrays: {{{2
    syn cluster phpClIdentifier add=phpLongVar
    syn match phpLongVar contained display /\$\@<=HTTP_GET_VARS\>/
    syn match phpLongVar contained display /\$\@<=HTTP_POST_VARS\>/
    syn match phpLongVar contained display /\$\@<=HTTP_POST_FILES\>/
    syn match phpLongVar contained display /\$\@<=HTTP_COOKIE_VARS\>/
    syn match phpLongVar contained display /\$\@<=HTTP_SESSION_VARS\>/
    syn match phpLongVar contained display /\$\@<=HTTP_SERVER_VARS\>/
    syn match phpLongVar contained display /\$\@<=HTTP_ENV_VARS\>/

    " Server Variables: {{{2
    " TODO: check these against the latest PHP manual
    syn cluster phpClIdentifier add=phpEnvVar
    syn match phpEnvVar contained display /\C\$\@<=GATEWAY_INTERFACE\>/
    syn match phpEnvVar contained display /\C\$\@<=SERVER_NAME\>/
    syn match phpEnvVar contained display /\C\$\@<=SERVER_SOFTWARE\>/
    syn match phpEnvVar contained display /\C\$\@<=SERVER_PROTOCOL\>/
    syn match phpEnvVar contained display /\C\$\@<=REQUEST_METHOD\>/
    syn match phpEnvVar contained display /\C\$\@<=QUERY_STRING\>/
    syn match phpEnvVar contained display /\C\$\@<=DOCUMENT_ROOT\>/
    syn match phpEnvVar contained display /\C\$\@<=HTTP_ACCEPT\>/
    syn match phpEnvVar contained display /\C\$\@<=HTTP_ACCEPT_CHARSET\>/
    syn match phpEnvVar contained display /\C\$\@<=HTTP_ENCODING\>/
    syn match phpEnvVar contained display /\C\$\@<=HTTP_ACCEPT_LANGUAGE\>/
    syn match phpEnvVar contained display /\C\$\@<=HTTP_CONNECTION\>/
    syn match phpEnvVar contained display /\C\$\@<=HTTP_HOST\>/
    syn match phpEnvVar contained display /\C\$\@<=HTTP_REFERER\>/
    syn match phpEnvVar contained display /\C\$\@<=HTTP_USER_AGENT\>/
    syn match phpEnvVar contained display /\C\$\@<=REMOTE_ADDR\>/
    syn match phpEnvVar contained display /\C\$\@<=REMOTE_PORT\>/
    syn match phpEnvVar contained display /\C\$\@<=SCRIPT_FILENAME\>/
    syn match phpEnvVar contained display /\C\$\@<=SERVER_ADMIN\>/
    syn match phpEnvVar contained display /\C\$\@<=SERVER_PORT\>/
    syn match phpEnvVar contained display /\C\$\@<=SERVER_SIGNATURE\>/
    syn match phpEnvVar contained display /\C\$\@<=PATH_TRANSLATED\>/
    syn match phpEnvVar contained display /\C\$\@<=SCRIPT_NAME\>/
    syn match phpEnvVar contained display /\C\$\@<=REQUEST_URI\>/
  endif

  " }}}2

" PHP Syntax: type-casting: (string)$foo {{{1

  syn cluster phpClValues add=phpType
  syn keyword phpType contained null bool boolean int integer real double float string object
  " only match 'array' as a type when it *isn't* followed by '('
  syn match phpType contained /\<array\>\%(\_s*(\)\@!/

" PHP Syntax: function/static calls: {{{1

  " Note: I could have forced function calls to be matched only when a '('
  " follows, but I think users would want PHP functions highlighted in more
  " places, rather than less, so I have just added the :\@! to make sure it is
  " not part of a static method call
  " Note: this didn't work properly ... if the match didn't include the
  " '(...)', then functions in @phpClFunctions can't have a nextgroup on them
  syn cluster phpClExpressions add=@phpClFunctions
"  syn cluster phpClExpressions add=phpFunctionCall
"  syn match phpFunctionCall contained /\<\%(\h\w*\)(\_.*)/
"        \ contains=@phpClFunctions

  " Also, a match when a word is part of a :: reference:
  syn cluster phpClValues add=phpStaticUsage
  syn cluster phpClExpressions add=phpStaticUsage
  syn cluster phpClStructureHere add=phpStaticUsage
  syn match phpStaticUsage contained display /\<\%(\h\w*\)\@>\%(\_s*::\)\@=/
        \ transparent contains=@phpClClasses


  " a match for a static function call
  syn cluster phpClValues add=phpStaticAccess
  syn cluster phpClExpressions add=phpStaticVariable,phpStaticCall
  syn cluster phpClPropertyHere add=phpStaticAccess
  syn cluster phpClMethodHere add=phpStaticCall

  " also allowed in function prototypes
  syn cluster phpClDefineFuncProtoArgs add=phpStaticAccess

  syn match phpStaticAccess contained extend display /::\_s*\%(\h\w*\|\%#\)/ contains=phpDoubleColon
  syn match phpStaticCall contained extend display /::\_s*\h\w*\ze\_s*(/
        \ contains=phpDoubleColon,@phpClMethods

  " a match for a static variable usage
  syn match phpStaticVariable contained display /::\_s*\$\h\w*/ extend
        \ contains=phpDoubleColon,phpIdentifier

  " a match for a static constant usage

  syn match phpDoubleColon contained display /::/
  hi link phpDoubleColon phpDefine


" PHP Syntax: magic classes (self/parent): {{{1

  syn cluster phpClClasses add=phpMagicClass
  syn keyword phpMagicClass contained self parent

  " Note: 'self' and 'parent' are also matched in other places because they
  " could be confusing otherwise ...
  syn cluster phpClValues add=phpMagicClass

" PHP Syntax: control structures: {{{1

  syn cluster phpClCode add=phpConditional
  syn keyword phpConditional contained declare enddeclare if else elseif endif

  syn cluster phpClCode add=phpRepeat
  syn keyword phpRepeat contained foreach endforeach
        \ do while endwhile for endfor
        \ switch endswitch

  " override 'foreach' if we need use a special colour for the '(...)' region
  if s:alt_arrays || s:alt_control_parents
    syn keyword phpRepeat contained foreach nextgroup=phpForeachRegion skipwhite skipempty
    syn keyword phpAs contained as containedin=phpForeachRegion
    hi link phpAs phpRepeat
  else
    " otherwise, allow 'as' anywhere
    syn cluster phpClExpressions add=phpAs
    syn keyword phpAs contained as
  endif

  if s:strict_blocks
    " need an alternate parenthesis block for 'for' structure
    " when using strict blocks
    syn keyword phpRepeat contained for nextgroup=phpForRegion skipwhite skipempty

    " if looking for errors with semicolons, add the other 'nextgroups' as well
    if s:no_empty_construct
      syn keyword phpRepeat contained while nextgroup=phpConstructRegion skipwhite skipempty
      syn keyword phpRepeat contained switch nextgroup=phpSwitchConstructRegion skipwhite skipempty
      syn keyword phpConditional contained if elseif nextgroup=phpConstructRegion skipwhite skipempty
      syn keyword phpConditional contained else nextgroup=phpSemicolonNotAllowedHere skipwhite skipempty
      syn keyword phpRepeat contained do nextgroup=phpDoBlock,phpSemicolonNotAllowedHere skipwhite skipempty
      if s:folding == 2
        syn region phpDoBlock matchgroup=phpBrace start=/{/ end=/}/ keepend extend
              \ contained transparent
              \ nextgroup=phpDoWhile skipwhite skipempty
"              \ matchgroup=phpHTMLError end=/?>/
              \ fold
        syn region phpSwitchBlock matchgroup=phpBrace start=/{/ end=/}/ keepend extend
              \ contained contains=@phpClInSwitch,@phpClCode
              \ fold
      else
        syn region phpDoBlock matchgroup=phpBrace start=/{/ end=/}/ keepend extend
              \ contained transparent
              \ nextgroup=phpDoWhile skipwhite skipempty
"              \ matchgroup=phpHTMLError end=/?>/
        syn region phpSwitchBlock matchgroup=phpBrace start=/{/ end=/}/ keepend extend
              \ contained contains=@phpClInSwitch,@phpClCode

        " TODO: thoroughly test this feature ...
        if s:fold_manual
          syn region phpDoBlock matchgroup=phpBrace start='{\ze\s*//\s*fold\s*$\c' end='}' keepend extend
                \ contained transparent
                \ nextgroup=phpDoWhile skipwhite skipempty
"               \ matchgroup=phpHTMLError end=/?>/
                \ fold
          syn region phpSwitchBlock matchgroup=phpBrace start='{\ze\s*//\s*fold\s*$\c' end='}' keepend extend
                \ contained contains=@phpClInSwitch,@phpClCode
                \ fold
        endif
      endif
      syn keyword phpDoWhile contained while nextgroup=phpDoWhileConstructRegion skipwhite skipempty
      hi link phpDoWhile phpRepeat
    endif
  endif


" PHP Syntax: statements: {{{1

  " if we are not using smart semicolons, we can implement these using
  " keywords
  if ! s:smart_semicolon
    syn cluster phpClCode add=phpStatement
    syn keyword phpStatement contained return break continue exit die throw

    syn cluster phpClInSwitch add=phpCase
    syn keyword phpCase contained case default

  else
    " if we *are* using smart semicolons, we'll need to use regions instead
    syn cluster phpClCode add=phpStatementRegion
    syn cluster phpClInSwitch add=phpCaseRegion

    " with or without error on mis-matched /})]/ at end?
    " Note: by containing phpSemicolonError, the semicolon error is
    " automatically taken care of by the phpSemicolonError item

    " return/break/continue/exit/die: {{{2
    " Note: having an error ending at 'private/var/final' etc, makes things much
    " much faster for writing classes.
    if s:strict_blocks
      syn region phpStatementRegion extend keepend contained
            \ contains=@phpClExpressions,phpSemicolonError
            \ matchgroup=phpStatement end=/;/ end=/\ze?>/
            \ start=/\$\@<!\<return\>/
            \ start=/\$\@<!\<break\>/
            \ start=/\$\@<!\<continue\>/
            \ start=/\$\@<!\<exit\>/
            \ start=/\$\@<!\<die\>/
            \ start=/\$\@<!\<throw\>/
            \ matchgroup=Error
              \ end=/[$>]\@<!\<\%(protected\|public\|private\|var\|final\|abstract\|static\)\>/
              \ end=/}/ end=/)/ end=/\]/
              \ end=/,/
"              \ end=/;\_s*\%([.*\^|&,:!=<>]\|?>\@!\|\/[/*]\@!\|++\@!\|--\@!\)/
    else
      syn region phpStatementRegion extend keepend contained
            \ contains=@phpClExpressions,phpSemicolonError
            \ matchgroup=phpStatement end=/;/
            \ start=/\$\@<!\<return\>/
            \ start=/\$\@<!\<break\>/
            \ start=/\$\@<!\<continue\>/
            \ start=/\$\@<!\<exit\>/
            \ start=/\$\@<!\<die\>/
            \ start=/\$\@<!\<throw\>/
"              \ end=/;\_s*\%([.*\^|&,:!=<>]\|?>\@!\|\/[/*]\@!\|++\@!\|--\@!\)/
    endif " }}}2
    " case: {{{2
    if s:strict_blocks
      syn region phpCaseRegion extend keepend contained display
            \ contains=@phpClExpressions,phpSemicolonError,phpColonError
            \ matchgroup=phpCase start=/\$\@<!\<case\>/ end=/[;:]/ skip=/::/
            \ matchgroup=Error
              \ end=/}/ end=/)/ end=/\]/
"              \ end=/;\_s*\%([.*\^|&,:!=<>]\|?>\@!\|\/[/*]\@!\|++\@!\|--\@!\)/
    else
      syn region phpCaseRegion extend keepend contained display
            \ contains=@phpClExpressions,phpSemicolonError,phpColonError
            \ matchgroup=phpCase start=/\$\@<!\<case\>/ end=/[;:]/
    endif " }}}2
    " default: {{{2
    if s:strict_blocks
      syn region phpCaseRegion extend keepend contained display
            \ contains=phpComment,phpSemicolonError,phpColonError
            \ matchgroup=phpCase start=/\$\@<!\<default\>/ end=/[;:]/
            \ matchgroup=Error end=/}/ end=/)/ end=/\]/
"              \ end=/;\_s*\%([.*\^|&,:!=<>]\|?>\@!\|\/[/*]\@!\|++\@!\|--\@!\)/
    else
      syn region phpCaseRegion extend keepend contained display
            \ contains=phpComment,phpSemicolonError,phpColonError
            \ matchgroup=phpCase start=/\$\@<!\<default\>/ end=/[;:]/
    endif " }}}2

  endif

  " if we *aren't* using strict blocks, allow a 'case' or 'default'
  " anywhere in regular code
  if ! s:strict_blocks
    syn cluster phpClCode add=@phpClInSwitch
  endif


" PHP Syntax: semicolons: {{{1

  if s:show_semicolon
    " highlight the semicolon anywhere it appears in regular code
    syn cluster phpClExpressions add=phpSemicolon
    syn match phpSemicolon /;/ contained display contains=phpSemicolonError

    " match a semicolon or colon which is followed by one of:
    "   = ! . +-*/ &|^ < > , ? :
    if s:show_semicolon_error
      " match a semicolon or colon which is followed by one of:
      "   = ! . +-*/ &|^ < > , ? :
      syn match phpSemicolonError contained display extend
            \ ";\_s*\%(\%(\%(//.*$\|#.*$\|/\*\_.\{-}\*/\)\_s*\)*\)\@>\%([.*/\^|&,:!=<>]\|?>\@!\|++\@!\|--\@!\)"
      syn match phpColonError contained display extend
            \ "::\@!\_s*\%(\%(\%(//.*$\|#.*$\|/\*\_.\{-}\*/\)\_s*\)*\)\@>\%([.*/\^|&,:!=<>]\|?>\@!\|++\@!\|--\@!\)"
    endif
    hi link phpSemicolonError Error
    hi link phpColonError Error

    " need to sync back one or two linebreaks to capture the semicolon
    " error properly
    syn sync linebreaks=2
  endif

  " a special match for when a semicolon is not allowed here
  " Note: this is contained/nextgrouped by other items, not included
  " directly
  if s:no_empty_construct
    syn match phpSemicolonNotAllowedHere /;/ contained display
  endif


" PHP Syntax: constants: {{{1

  " Magic Constants {{{2
    syn cluster phpClConstants add=phpMagicConstant
    syn case match
    syn keyword	phpMagicConstant contained __LINE__ __FILE__ __FUNCTION__ __METHOD__ __CLASS__
    syn case ignore
  " }}}2

  " Built-in Constants {{{2
  " TODO: check these against the latest PHP manual
    syn cluster phpClConstants add=phpCoreConstant
    syn case match
    syn keyword phpCoreConstant contained ASSERT_ACTIVE ASSERT_BAIL ASSERT_CALLBACK ASSERT_QUIET_EVAL ASSERT_WARNING
    syn keyword phpCoreConstant contained CAL_DOW_DAYNO CAL_DOW_LONG CAL_DOW_SHORT CAL_EASTER_ALWAYS_GREGORIAN CAL_EASTER_ALWAYS_JULIAN CAL_EASTER_DEFAULT CAL_EASTER_ROMAN CAL_FRENCH CAL_GREGORIAN CAL_JULIAN CAL_NUM_CALS CAL_JEWISH CAL_JEWISH_ADD_ALAFIM CAL_JEWISH_ADD_ALAFIM_GERESH CAL_JEWISH_ADD_GERESHAYIM CAL_MONTH_FRENCH CAL_MONTH_GREGORIAN_LONG CAL_MONTH_GREGORIAN_SHORT CAL_MONTH_JEWISH CAL_MONTH_JULIAN_LONG CAL_MONTH_JULIAN_SHORT
    syn keyword phpCoreConstant contained CASE_LOWER CASE_UPPER CHAR_MAX
    syn keyword phpCoreConstant contained CLSCTX_ALL CLSCTX_INPROC_HANDLER CLSCTX_INPROC_SERVER CLSCTX_LOCAL_SERVER CLSCTX_REMOTE_SERVER CLSCTX_SERVER
    syn keyword phpCoreConstant contained CONNECTION_ABORTED CONNECTION_NORMAL CONNECTION_TIMEOUT
    syn keyword phpCoreConstant contained COUNT_NORMAL COUNT_RECURSIVE
    syn keyword phpCoreConstant contained CP_ACP CP_MACCP CP_OEMCP CP_SYMBOL CP_THREAD_ACP CP_UTF7 CP_UTF8
    syn keyword phpCoreConstant contained CREDITS_ALL CREDITS_DOCS CREDITS_FULLPAGE CREDITS_GENERAL CREDITS_GROUP CREDITS_MODULES CREDITS_QA CREDITS_SAPI
    syn keyword phpCoreConstant contained CRYPT_BLOWFISH CRYPT_EXT_DES CRYPT_MD5 CRYPT_SALT_LENGTH CRYPT_STD_DES
    syn keyword phpCoreConstant contained DATE_ATOM DATE_COOKIE DATE_ISO8601 DATE_RFC1036 DATE_RFC1123 DATE_RFC2822 DATE_RFC822 DATE_RFC850 DATE_RSS DATE_W3C
    syn keyword phpCoreConstant contained DEFAULT_INCLUDE_PATH DIRECTORY_SEPARATOR
    syn keyword phpCoreConstant contained DISP_E_BADINDEX DISP_E_DIVBYZERO DISP_E_OVERFLOW
    syn keyword phpCoreConstant contained DOMSTRING_SIZE_ERR
    syn keyword phpCoreConstant contained DOM_HIERARCHY_REQUEST_ERR DOM_INDEX_SIZE_ERR DOM_INUSE_ATTRIBUTE_ERR DOM_INVALID_ACCESS_ERR DOM_INVALID_CHARACTER_ERR DOM_INVALID_MODIFICATION_ERR
    syn keyword phpCoreConstant contained DOM_INVALID_STATE_ERR DOM_NAMESPACE_ERR DOM_NOT_FOUND_ERR DOM_NOT_SUPPORTED_ERR DOM_NO_DATA_ALLOWED_ERR DOM_NO_MODIFICATION_ALLOWED_ERR DOM_PHP_ERR
    syn keyword phpCoreConstant contained DOM_SYNTAX_ERR DOM_VALIDATION_ERR DOM_WRONG_DOCUMENT_ERR
    syn keyword phpCoreConstant contained ENT_COMPAT ENT_NOQUOTES ENT_QUOTES
    syn keyword phpCoreConstant contained EXTR_IF_EXISTS EXTR_OVERWRITE EXTR_PREFIX_ALL EXTR_PREFIX_IF_EXISTS EXTR_PREFIX_INVALID EXTR_PREFIX_SAME EXTR_REFS EXTR_SKIP
    syn keyword phpCoreConstant contained E_ERROR E_WARNING E_PARSE E_NOTICE E_STRICT E_CORE_ERROR E_CORE_WARNING E_COMPILE_ERROR E_COMPILE_WARNING E_USER_ERROR E_USER_WARNING E_USER_NOTICE E_ALL
    syn keyword phpCoreConstant contained FILE_APPEND FILE_IGNORE_NEW_LINES FILE_NO_DEFAULT_CONTEXT FILE_SKIP_EMPTY_LINES FILE_USE_INCLUDE_PATH
    syn keyword phpCoreConstant contained FORCE_DEFLATE FORCE_GZIP
    syn keyword phpCoreConstant contained FTP_ASCII FTP_AUTORESUME FTP_AUTOSEEK FTP_BINARY FTP_FAILED FTP_FINISHED FTP_IMAGE FTP_MOREDATA FTP_TEXT FTP_TIMEOUT_SEC
    syn keyword phpCoreConstant contained GLOB_BRACE GLOB_ERR GLOB_MARK GLOB_NOCHECK GLOB_NOESCAPE GLOB_NOSORT GLOB_ONLYDIR
    syn keyword phpCoreConstant contained HTML_ENTITIES HTML_SPECIALCHARS
    syn keyword phpCoreConstant contained ICONV_IMPL ICONV_MIME_DECODE_CONTINUE_ON_ERROR ICONV_MIME_DECODE_STRICT ICONV_VERSION
    syn keyword phpCoreConstant contained IMAGETYPE_BMP IMAGETYPE_GIF IMAGETYPE_IFF IMAGETYPE_JB2 IMAGETYPE_JP2 IMAGETYPE_JPC IMAGETYPE_JPEG IMAGETYPE_JPEG2000
    syn keyword phpCoreConstant contained IMAGETYPE_JPX IMAGETYPE_PNG IMAGETYPE_PSD IMAGETYPE_SWC IMAGETYPE_SWF IMAGETYPE_TIFF_II IMAGETYPE_TIFF_MM IMAGETYPE_WBMP IMAGETYPE_XBM
    syn keyword phpCoreConstant contained INF
    syn keyword phpCoreConstant contained INFO_ALL INFO_CONFIGURATION INFO_CREDITS INFO_ENVIRONMENT INFO_GENERAL INFO_LICENSE INFO_MODULES INFO_VARIABLES
    syn keyword phpCoreConstant contained INI_ALL INI_PERDIR INI_SYSTEM INI_USER
    syn keyword phpCoreConstant contained LC_ALL LC_COLLATE LC_CTYPE LC_MONETARY LC_NUMERIC LC_TIME
    syn keyword phpCoreConstant contained LIBXML_COMPACT LIBXML_DOTTED_VERSION LIBXML_DTDATTR LIBXML_DTDLOAD LIBXML_DTDVALID LIBXML_ERR_ERROR LIBXML_ERR_FATAL LIBXML_ERR_NONE
    syn keyword phpCoreConstant contained LIBXML_ERR_WARNING LIBXML_NOBLANKS LIBXML_NOCDATA LIBXML_NOEMPTYTAG LIBXML_NOENT LIBXML_NOERROR LIBXML_NONET LIBXML_NOWARNING
    syn keyword phpCoreConstant contained LIBXML_NOXMLDECL LIBXML_NSCLEAN LIBXML_VERSION LIBXML_XINCLUDE
    syn keyword phpCoreConstant contained LOCK_EX LOCK_NB LOCK_SH LOCK_UN
    syn keyword phpCoreConstant contained LOG_ALERT LOG_AUTH LOG_AUTHPRIV LOG_CONS LOG_CRIT LOG_CRON LOG_DAEMON LOG_DEBUG
    syn keyword phpCoreConstant contained LOG_EMERG LOG_ERR LOG_INFO LOG_KERN LOG_LPR LOG_MAIL LOG_NDELAY LOG_NEWS
    syn keyword phpCoreConstant contained LOG_NOTICE LOG_NOWAIT LOG_ODELAY LOG_PERROR LOG_PID LOG_SYSLOG LOG_USER LOG_UUCP LOG_WARNING
    syn keyword phpCoreConstant contained MK_E_UNAVAILABLE
    syn keyword phpCoreConstant contained MYSQL_ASSOC MYSQL_BOTH MYSQL_CLIENT_COMPRESS MYSQL_CLIENT_IGNORE_SPACE MYSQL_CLIENT_INTERACTIVE MYSQL_CLIENT_SSL MYSQL_NUM
    syn keyword phpCoreConstant contained M_1_PI M_2_PI M_2_SQRTPI M_E M_LN10 M_LN2 M_LOG10E M_LOG2E M_PI M_PI_2 M_PI_4 M_SQRT1_2 M_SQRT2
    syn keyword phpCoreConstant contained NAN
    syn keyword phpCoreConstant contained NORM_IGNORECASE NORM_IGNOREKANATYPE NORM_IGNORENONSPACE NORM_IGNORESYMBOLS NORM_IGNOREWIDTH
    syn keyword phpCoreConstant contained ODBC_BINMODE_CONVERT ODBC_BINMODE_PASSTHRU ODBC_BINMODE_RETURN ODBC_TYPE
    syn keyword phpCoreConstant contained PATHINFO_BASENAME PATHINFO_DIRNAME PATHINFO_EXTENSION
    syn keyword phpCoreConstant contained PATH_SEPARATOR
    syn keyword phpCoreConstant contained PEAR_INSTALL_DIR PEAR_EXTENSION_DIR
    syn keyword phpCoreConstant contained PHP_PREFIX PHP_BINDIR PHP_CONFIG_FILE_PATH PHP_CONFIG_FILE_SCAN_DIR PHP_DATADIR PHP_EXTENSION_DIR PHP_LIBDIR PHP_LOCALSTATEDIR PHP_SYSCONFDIR PHP_SHLIB_SUFFIX
    syn keyword phpCoreConstant contained PHP_OUTPUT_HANDLER_CONT PHP_OUTPUT_HANDLER_END PHP_OUTPUT_HANDLER_START
    syn keyword phpCoreConstant contained PHP_URL_FRAGMENT PHP_URL_HOST PHP_URL_PASS PHP_URL_PATH PHP_URL_PORT PHP_URL_QUERY PHP_URL_SCHEME PHP_URL_USER
    syn keyword phpCoreConstant contained PHP_VERSION PHP_OS PHP_SAPI PHP_EOL PHP_INT_MAX PHP_INT_SIZE
    syn keyword phpCoreConstant contained PREG_GREP_INVERT PREG_OFFSET_CAPTURE PREG_PATTERN_ORDER PREG_SET_ORDER PREG_SPLIT_DELIM_CAPTURE PREG_SPLIT_NO_EMPTY PREG_SPLIT_OFFSET_CAPTURE
    syn keyword phpCoreConstant contained PSFS_ERR_FATAL PSFS_FEED_ME PSFS_FLAG_FLUSH_CLOSE PSFS_FLAG_FLUSH_INC PSFS_FLAG_NORMAL PSFS_PASS_ON
    syn keyword phpCoreConstant contained SEEK_CUR SEEK_END SEEK_SET
    syn keyword phpCoreConstant contained SORT_ASC SORT_DESC SORT_LOCALE_STRING SORT_NUMERIC SORT_REGULAR SORT_STRING
    syn keyword phpCoreConstant contained SQL_BIGINT SQL_BINARY SQL_BIT SQL_CHAR SQL_CONCURRENCY SQL_CONCUR_LOCK SQL_CONCUR_READ_ONLY SQL_CONCUR_ROWVER SQL_CONCUR_VALUES
    syn keyword phpCoreConstant contained SQL_CURSOR_DYNAMIC SQL_CURSOR_FORWARD_ONLY SQL_CURSOR_KEYSET_DRIVEN SQL_CURSOR_STATIC SQL_CURSOR_TYPE SQL_CUR_USE_DRIVER SQL_CUR_USE_IF_NEEDED SQL_CUR_USE_ODBC
    syn keyword phpCoreConstant contained SQL_DATE SQL_DECIMAL SQL_DOUBLE SQL_FETCH_FIRST SQL_FETCH_NEXT SQL_FLOAT SQL_INTEGER SQL_KEYSET_SIZE
    syn keyword phpCoreConstant contained SQL_LONGVARBINARY SQL_LONGVARCHAR SQL_NUMERIC SQL_ODBC_CURSORS SQL_REAL SQL_SMALLINT SQL_TIME SQL_TIMESTAMP SQL_TINYINT SQL_VARBINARY SQL_VARCHAR
    syn keyword phpCoreConstant contained STDERR STDIN STDOUT
    syn keyword phpCoreConstant contained STREAM_CLIENT_ASYNC_CONNECT STREAM_CLIENT_CONNECT STREAM_CLIENT_PERSISTENT
    syn keyword phpCoreConstant contained STREAM_CRYPTO_METHOD_SSLv23_CLIENT STREAM_CRYPTO_METHOD_SSLv23_SERVER STREAM_CRYPTO_METHOD_SSLv2_CLIENT STREAM_CRYPTO_METHOD_SSLv2_SERVER STREAM_CRYPTO_METHOD_SSLv3_CLIENT STREAM_CRYPTO_METHOD_SSLv3_SERVER STREAM_CRYPTO_METHOD_TLS_CLIENT STREAM_CRYPTO_METHOD_TLS_SERVER
    syn keyword phpCoreConstant contained STREAM_ENFORCE_SAFE_MODE STREAM_FILTER_ALL STREAM_FILTER_READ STREAM_FILTER_WRITE STREAM_IGNORE_URL
    syn keyword phpCoreConstant contained STREAM_IPPROTO_ICMP STREAM_IPPROTO_IP STREAM_IPPROTO_RAW STREAM_IPPROTO_TCP STREAM_IPPROTO_UDP STREAM_MKDIR_RECURSIVE STREAM_MUST_SEEK
    syn keyword phpCoreConstant contained STREAM_NOTIFY_AUTH_REQUIRED STREAM_NOTIFY_AUTH_RESULT STREAM_NOTIFY_COMPLETED STREAM_NOTIFY_CONNECT STREAM_NOTIFY_FAILURE STREAM_NOTIFY_FILE_SIZE_IS STREAM_NOTIFY_MIME_TYPE_IS
    syn keyword phpCoreConstant contained STREAM_NOTIFY_PROGRESS STREAM_NOTIFY_REDIRECTED STREAM_NOTIFY_RESOLVE STREAM_NOTIFY_SEVERITY_ERR STREAM_NOTIFY_SEVERITY_INFO STREAM_NOTIFY_SEVERITY_WARN
    syn keyword phpCoreConstant contained STREAM_OOB STREAM_PEEK STREAM_PF_INET STREAM_PF_INET6 STREAM_PF_UNIX STREAM_REPORT_ERRORS STREAM_SERVER_BIND STREAM_SERVER_LISTEN
    syn keyword phpCoreConstant contained STREAM_SOCK_DGRAM STREAM_SOCK_RAW STREAM_SOCK_RDM STREAM_SOCK_SEQPACKET STREAM_SOCK_STREAM STREAM_URL_STAT_LINK STREAM_URL_STAT_QUIET STREAM_USE_PATH
    syn keyword phpCoreConstant contained STR_PAD_BOTH STR_PAD_LEFT STR_PAD_RIGHT
    syn keyword phpCoreConstant contained SUNFUNCS_RET_DOUBLE SUNFUNCS_RET_STRING SUNFUNCS_RET_TIMESTAMP
    syn keyword phpCoreConstant contained T_ABSTRACT T_AND_EQUAL T_ARRAY T_ARRAY_CAST T_AS T_BAD_CHARACTER T_BOOLEAN_AND T_BOOLEAN_OR T_BOOL_CAST T_BREAK T_CASE T_CATCH
    syn keyword phpCoreConstant contained T_CHARACTER T_CLASS T_CLASS_C T_CLONE T_CLOSE_TAG T_COMMENT T_CONCAT_EQUAL T_CONST T_CONSTANT_ENCAPSED_STRING T_CONTINUE
    syn keyword phpCoreConstant contained T_CURLY_OPEN T_DEC T_DECLARE T_DEFAULT T_DIV_EQUAL T_DNUMBER T_DO T_DOC_COMMENT T_DOLLAR_OPEN_CURLY_BRACES T_DOUBLE_ARROW
    syn keyword phpCoreConstant contained T_DOUBLE_CAST T_DOUBLE_COLON T_ECHO T_ELSE T_ELSEIF T_EMPTY T_ENCAPSED_AND_WHITESPACE T_ENDDECLARE T_ENDFOR T_ENDFOREACH
    syn keyword phpCoreConstant contained T_ENDIF T_ENDSWITCH T_ENDWHILE T_END_HEREDOC T_EVAL T_EXIT T_EXTENDS T_FILE T_FINAL T_FOR T_FOREACH T_FUNCTION T_FUNC_C
    syn keyword phpCoreConstant contained T_GLOBAL T_HALT_COMPILER T_IF T_IMPLEMENTS T_INC T_INCLUDE T_INCLUDE_ONCE T_INLINE_HTML T_INSTANCEOF T_INTERFACE T_INT_CAST
    syn keyword phpCoreConstant contained T_ISSET T_IS_EQUAL T_IS_GREATER_OR_EQUAL T_IS_IDENTICAL T_IS_NOT_EQUAL T_IS_NOT_IDENTICAL T_IS_SMALLER_OR_EQUAL T_LINE T_LIST
    syn keyword phpCoreConstant contained T_LNUMBER T_LOGICAL_AND T_LOGICAL_OR T_LOGICAL_XOR T_METHOD_C T_MINUS_EQUAL T_MOD_EQUAL T_MUL_EQUAL T_NEW T_NUM_STRING T_OBJECT_CAST
    syn keyword phpCoreConstant contained T_OBJECT_OPERATOR T_OPEN_TAG T_OPEN_TAG_WITH_ECHO T_OR_EQUAL T_PAAMAYIM_NEKUDOTAYIM T_PLUS_EQUAL T_PRINT T_PRIVATE T_PROTECTED T_PUBLIC
    syn keyword phpCoreConstant contained T_REQUIRE T_REQUIRE_ONCE T_RETURN T_SL T_SL_EQUAL T_SR T_SR_EQUAL T_START_HEREDOC T_STATIC T_STRING T_STRING_CAST T_STRING_VARNAME
    syn keyword phpCoreConstant contained T_SWITCH T_THROW T_TRY T_UNSET T_UNSET_CAST T_USE T_VAR T_VARIABLE T_WHILE T_WHITESPACE T_XOR_EQUAL
    syn keyword phpCoreConstant contained UPLOAD_ERR_CANT_WRITE UPLOAD_ERR_FORM_SIZE UPLOAD_ERR_INI_SIZE UPLOAD_ERR_NO_FILE UPLOAD_ERR_NO_TMP_DIR UPLOAD_ERR_OK UPLOAD_ERR_PARTIAL
    syn keyword phpCoreConstant contained VARCMP_EQ VARCMP_GT VARCMP_LT VARCMP_NULL
    syn keyword phpCoreConstant contained VT_ARRAY VT_BOOL VT_BSTR VT_BYREF VT_CY VT_DATE VT_DECIMAL VT_DISPATCH VT_EMPTY VT_ERROR VT_I1 VT_I2 VT_I4 VT_INT VT_NULL VT_R4 VT_R8 VT_UI1 VT_UI2 VT_UI4 VT_UINT VT_UNKNOWN VT_VARIANT
    syn keyword phpCoreConstant contained XML_ATTRIBUTE_CDATA XML_ATTRIBUTE_DECL_NODE XML_ATTRIBUTE_ENTITY XML_ATTRIBUTE_ENUMERATION XML_ATTRIBUTE_ID XML_ATTRIBUTE_IDREF
    syn keyword phpCoreConstant contained XML_ATTRIBUTE_IDREFS XML_ATTRIBUTE_NMTOKEN XML_ATTRIBUTE_NMTOKENS XML_ATTRIBUTE_NODE XML_ATTRIBUTE_NOTATION XML_CDATA_SECTION_NODE
    syn keyword phpCoreConstant contained XML_COMMENT_NODE XML_DOCUMENT_FRAG_NODE XML_DOCUMENT_NODE XML_DOCUMENT_TYPE_NODE XML_DTD_NODE XML_ELEMENT_DECL_NODE XML_ELEMENT_NODE
    syn keyword phpCoreConstant contained XML_ENTITY_DECL_NODE XML_ENTITY_NODE XML_ENTITY_REF_NODE XML_ERROR_ASYNC_ENTITY XML_ERROR_ATTRIBUTE_EXTERNAL_ENTITY_REF XML_ERROR_BAD_CHAR_REF
    syn keyword phpCoreConstant contained XML_ERROR_BINARY_ENTITY_REF XML_ERROR_DUPLICATE_ATTRIBUTE XML_ERROR_EXTERNAL_ENTITY_HANDLING XML_ERROR_INCORRECT_ENCODING XML_ERROR_INVALID_TOKEN
    syn keyword phpCoreConstant contained XML_ERROR_JUNK_AFTER_DOC_ELEMENT XML_ERROR_MISPLACED_XML_PI XML_ERROR_NONE XML_ERROR_NO_ELEMENTS XML_ERROR_NO_MEMORY XML_ERROR_PARAM_ENTITY_REF
    syn keyword phpCoreConstant contained XML_ERROR_PARTIAL_CHAR XML_ERROR_RECURSIVE_ENTITY_REF XML_ERROR_SYNTAX XML_ERROR_TAG_MISMATCH XML_ERROR_UNCLOSED_CDATA_SECTION
    syn keyword phpCoreConstant contained XML_ERROR_UNCLOSED_TOKEN XML_ERROR_UNDEFINED_ENTITY XML_ERROR_UNKNOWN_ENCODING XML_HTML_DOCUMENT_NODE XML_LOCAL_NAMESPACE
    syn keyword phpCoreConstant contained XML_NAMESPACE_DECL_NODE XML_NOTATION_NODE XML_OPTION_CASE_FOLDING XML_OPTION_SKIP_TAGSTART XML_OPTION_SKIP_WHITE XML_OPTION_TARGET_ENCODING
    syn keyword phpCoreConstant contained XML_PI_NODE XML_SAX_IMPL XML_TEXT_NODE
    syn keyword phpCoreConstant contained ZEND_THREAD_SAFE
    syn case ignore
  " }}}2

" PHP Syntax: functions {{{1

  " TODO: move constants out of here - they need to be case-sensitive

  " Function and Methods ripped from php_manual_de.tar.gz Jan 2003 {{{2
  " TODO: check these against the latest PHP manual
  syn cluster phpClFunctions add=phpFunctions

  " Apache
  syn keyword phpFunctions contained apache_child_terminate apache_get_modules apache_get_version apache_getenv apache_lookup_uri apache_note apache_request_headers apache_reset_timeout apache_response_headers apache_setenv ascii2ebcdic ebcdic2ascii getallheaders virtual

  " APC Alternative PHP Cache
  syn keyword phpFunctions contained apc_add apc_cache_info apc_clear_cache apc_compile_file apc_define_constants apc_delete apc_fetch apc_load_constants apc_sma_info apc_store

  " APD Advanced PHP Debugger
  syn keyword phpCoreConstant contained FUNCTION_TRACE ARGS_TRACE ASSIGNMENT_TRACE STATEMENT_TRACE MEMORY_TRACE TIMING_TRACE SUMMARY_TRACE ERROR_TRACE PROF_TRACE APD_VERSION
  syn keyword phpFunctions contained apd_breakpoint apd_callstack apd_clunk apd_continue apd_croak apd_dump_function_table apd_dump_persistent_resources apd_dump_regular_resources apd_echo apd_get_active_symbols apd_set_pprof_trace apd_set_session_trace apd_set_session apd_set_socket_session_trace override_function rename_function

  " array functions
  syn keyword phpCoreConstant contained CASE_LOWER CASE_UPPER SORT_ASC SORT_DESC SORT_REGULAR SORT_NUMERIC SORT_STRING SORT_LOCALE_STRING COUNT_NORMAL COUNT_RECURSIVE EXTR_OVERWRITE EXTR_SKIP EXTR_PREFIX_SAME EXTR_PREFIX_ALL EXTR_PREFIX_INVALID EXTR_PREFIX_IF_EXISTS EXTR_IF_EXISTS EXTR_REFS
  syn keyword phpFunctions contained array_change_key_case array_chunk array_combine array_count_values array_diff_assoc array_diff_key array_diff_uassoc array_diff_ukey array_diff array_fill_keys array_fill array_filter array_flip array_intersect_assoc array_intersect_key array_intersect_uassoc array_intersect_ukey array_intersect array_key_exists array_keys array_map array_merge_recursive array_merge array_multisort array_pad array_pop array_product array_push array_rand array_reduce array_reverse array_search array_shift array_slice array_splice array_sum array_udiff_assoc array_udiff_uassoc array_udiff array_uintersect_assoc array_uintersect_uassoc array_uintersect array_unique array_unshift array_values array_walk_recursive array_walk arsort asort compact count current each end extract in_array key krsort ksort natcasesort natsort next pos prev range reset rsort shuffle sizeof sort uasort uksort usort

  " NOTE: aspell is deprecated as of PHP 4.3.0
  " syn keyword	phpFunctions	aspell_check aspell_new aspell_suggest	contained

  " BBCode
  syn keyword phpCoreConstant contained BBCODE_TYPE_NOARG BBCODE_TYPE_SINGLE BBCODE_TYPE_ARG BBCODE_TYPE_OPTARG BBCODE_TYPE_ROOT BBCODE_FLAGS_ARG_PARSING BBCODE_FLAGS_CDATA_NOT_ALLOWED BBCODE_FLAGS_SMILEYS_ON BBCODE_FLAGS_SMILEYS_OFF BBCODE_FLAGS_ONE_OPEN_PER_LEVEL BBCODE_FLAGS_REMOVE_IF_EMPTY BBCODE_FLAGS_DENY_REOPEN_CHILD BBCODE_ARG_DOUBLE_QUOTE BBCODE_ARG_SINGLE_QUOTE BBCODE_ARG_HTML_QUOTE BBCODE_AUTO_CORRECT BBCODE_CORRECT_REOPEN_TAGS BBCODE_DISABLE_TREE_BUILD BBCODE_DEFAULT_SMILEYS_ON BBCODE_DEFAULT_SMILEYS_OFF BBCODE_FORCE_SMILEYS_OFF BBCODE_SMILEYS_CASE_INSENSITIVE BBCODE_SET_FLAGS_SET BBCODE_SET_FLAGS_ADD BBCODE_SET_FLAGS_REMOVE
  syn keyword phpFunctions contained bbcode_add_element bbcode_add_smiley bbcode_create bbcode_destroy bbcode_parse bbcode_set_arg_parser bbcode_set_flags

  " BC Math
  syn keyword phpFunctions contained bcadd bccomp bcdiv bcmod bcmul bcpow bcpowmod bcscale bcsqrt bcsub

  " BZip2
  syn keyword phpFunctions contained bzclose bzcompress bzdecompress bzerrno bzerror bzerrstr bzflush bzopen bzread bzwrite

  " Calendar functions
  syn keyword phpCoreConstant contained CAL_GREGORIAN CAL_JULIAN CAL_JEWISH CAL_FRENCH CAL_NUM_CALS CAL_DOW_DAYNO CAL_DOW_SHORT CAL_DOW_LONG CAL_MONTH_GREGORIAN_SHORT CAL_MONTH_GREGORIAN_LONG CAL_MONTH_JULIAN_SHORT CAL_MONTH_JULIAN_LONG CAL_MONTH_JEWISH CAL_MONTH_FRENCH CAL_EASTER_DEFAULT CAL_EASTER_ROMAN CAL_EASTER_ALWAYS_GREGORIAN CAL_EASTER_ALWAYS_JULIAN CAL_JEWISH_ADD_ALAFIM_GERESH CAL_JEWISH_ADD_ALAFIM CAL_JEWISH_ADD_GERESHAYIM
  syn keyword phpFunctions contained cal_days_in_month cal_from_jd cal_info cal_to_jd easter_date easter_days FrenchToJD GregorianToJD JDDayOfWeek JDMonthName JDToFrench JDToGregorian jdtojewish JDToJulian jdtounix JewishToJD JulianToJD unixtojd

  " NOTE: CCVS has been deprecated as of PHP 4.3.0
  " syn keyword	phpFunctions	ccvs_add ccvs_auth ccvs_command ccvs_count ccvs_delete ccvs_done ccvs_init ccvs_lookup ccvs_new ccvs_report ccvs_return ccvs_reverse ccvs_sale ccvs_status ccvs_textvalue ccvs_void	contained

  " Classes / Objects
  " NOTE: call_user_method_array() and call_user_method() are both deprecated ...
  syn keyword phpFunctions contained class_exists get_class_methods get_class_vars get_class get_declared_classes get_declared_interfaces get_object_vars get_parent_class interface_exists is_a is_subclass_of method_exists property_exists

  " COM
  syn keyword phpCoreConstant contained CLSCTX_INPROC_SERVER CLSCTX_INPROC_HANDLER CLSCTX_LOCAL_SERVER CLSCTX_REMOTE_SERVER CLSCTX_SERVER CLSCTX_ALL VT_NULL VT_EMPTY VT_UI1 VT_I2 VT_I4 VT_R4 VT_R8 VT_BOOL VT_ERROR VT_CY VT_DATE VT_BSTR VT_DECIMAL VT_UNKNOWN VT_DISPATCH VT_VARIANT VT_I1 VT_UI2 VT_UI4 VT_INT VT_UINT VT_ARRAY VT_BYREF CP_ACP CP_MACCP CP_OEMCP CP_UTF7 CP_UTF8 CP_SYMBOL CP_THREAD_ACP VARCMP_LT VARCMP_EQ VARCMP_GT VARCMP_NULL NORM_IGNORECASE NORM_IGNORENONSPACE NORM_IGNORESYMBOLS NORM_IGNOREWIDTH NORM_IGNOREKANATYPE NORM_IGNOREKASHIDA DISP_E_DIVBYZERO DISP_E_OVERFLOW MK_E_UNAVAILABLE
  syn keyword phpClasses contained COM DOTNET VARIANT
  syn keyword phpFunctions contained com_addref com_create_guid com_event_sink com_get_active_object com_get com_invoke com_isenum com_load_typelib com_load com_message_pump com_print_typeinfo com_propget com_propput com_propset com_release com_set variant_abs variant_add variant_and variant_cast variant_cat variant_cmp variant_date_from_timestamp variant_date_to_timestamp variant_div variant_eqv variant_fix variant_get_type variant_idiv variant_imp variant_int variant_mod variant_mul variant_neg variant_not variant_or variant_pow variant_round variant_set_type variant_set variant_sub variant_xor

  " Crack functions
  syn keyword phpFunctions contained crack_check crack_closedict crack_getlastmessage crack_opendict

  " Character type functions
  syn keyword phpFunctions contained ctype_alnum ctype_alpha ctype_cntrl ctype_digit ctype_graph ctype_lower ctype_print ctype_punct ctype_space ctype_upper ctype_xdigit

  " cURL
  syn keyword phpCoreConstant contained CURLOPT_AUTOREFERER CURLOPT_COOKIESESSION CURLOPT_DNS_USE_GLOBAL_CACHE CURLOPT_DNS_CACHE_TIMEOUT CURLOPT_FTP_SSL CURLFTPSSL_TRY CURLFTPSSL_ALL CURLFTPSSL_CONTROL CURLFTPSSL_NONE CURLOPT_PRIVATE CURLOPT_FTPSSLAUTH CURLOPT_PORT CURLOPT_FILE CURLOPT_INFILE CURLOPT_INFILESIZE CURLOPT_URL CURLOPT_PROXY CURLOPT_VERBOSE CURLOPT_HEADER CURLOPT_HTTPHEADER CURLOPT_NOPROGRESS CURLOPT_NOBODY CURLOPT_FAILONERROR CURLOPT_UPLOAD CURLOPT_POST CURLOPT_FTPLISTONLY CURLOPT_FTPAPPEND CURLOPT_FTP_CREATE_MISSING_DIRS CURLOPT_NETRC CURLOPT_FOLLOWLOCATION CURLOPT_FTPASCII CURLOPT_PUT CURLOPT_MUTE CURLOPT_USERPWD CURLOPT_PROXYUSERPWD CURLOPT_RANGE CURLOPT_TIMEOUT CURLOPT_TIMEOUT_MS CURLOPT_TCP_NODELAY CURLOPT_POSTFIELDS CURLOPT_REFERER CURLOPT_USERAGENT CURLOPT_FTPPORT CURLOPT_FTP_USE_EPSV CURLOPT_LOW_SPEED_LIMIT CURLOPT_LOW_SPEED_TIME CURLOPT_RESUME_FROM CURLOPT_COOKIE CURLOPT_SSLCERT CURLOPT_SSLCERTPASSWD CURLOPT_WRITEHEADER CURLOPT_SSL_VERIFYHOST CURLOPT_COOKIEFILE CURLOPT_SSLVERSION CURLOPT_TIMECONDITION CURLOPT_TIMEVALUE CURLOPT_CUSTOMREQUEST CURLOPT_STDERR CURLOPT_TRANSFERTEXT CURLOPT_RETURNTRANSFER CURLOPT_QUOTE CURLOPT_POSTQUOTE CURLOPT_INTERFACE CURLOPT_KRB4LEVEL CURLOPT_HTTPPROXYTUNNEL CURLOPT_FILETIME CURLOPT_WRITEFUNCTION CURLOPT_READFUNCTION CURLOPT_PASSWDFUNCTION CURLOPT_HEADERFUNCTION CURLOPT_MAXREDIRS CURLOPT_MAXCONNECTS CURLOPT_CLOSEPOLICY CURLOPT_FRESH_CONNECT CURLOPT_FORBID_REUSE CURLOPT_RANDOM_FILE CURLOPT_EGDSOCKET CURLOPT_CONNECTTIMEOUT CURLOPT_CONNECTTIMEOUT_MS CURLOPT_SSL_VERIFYPEER CURLOPT_CAINFO CURLOPT_CAPATH CURLOPT_COOKIEJAR CURLOPT_SSL_CIPHER_LIST CURLOPT_BINARYTRANSFER CURLOPT_NOSIGNAL CURLOPT_PROXYTYPE CURLOPT_BUFFERSIZE CURLOPT_HTTPGET CURLOPT_HTTP_VERSION CURLOPT_SSLKEY CURLOPT_SSLKEYTYPE CURLOPT_SSLKEYPASSWD CURLOPT_SSLENGINE CURLOPT_SSLENGINE_DEFAULT CURLOPT_SSLCERTTYPE CURLOPT_CRLF CURLOPT_ENCODING CURLOPT_PROXYPORT CURLOPT_UNRESTRICTED_AUTH CURLOPT_FTP_USE_EPRT CURLOPT_HTTP200ALIASES CURLOPT_HTTPAUTH CURLAUTH_BASIC
  syn keyword phpCoreConstant contained CURLAUTH_DIGEST CURLAUTH_GSSNEGOTIATE CURLAUTH_NTLM CURLAUTH_ANY CURLAUTH_ANYSAFE CURLOPT_PROXYAUTH CURLCLOSEPOLICY_LEAST_RECENTLY_USED CURLCLOSEPOLICY_LEAST_TRAFFIC CURLCLOSEPOLICY_SLOWEST CURLCLOSEPOLICY_CALLBACK CURLCLOSEPOLICY_OLDEST CURLINFO_PRIVATE CURLINFO_EFFECTIVE_URL CURLINFO_HTTP_CODE CURLINFO_HEADER_OUT CURLINFO_HEADER_SIZE CURLINFO_REQUEST_SIZE CURLINFO_TOTAL_TIME CURLINFO_NAMELOOKUP_TIME CURLINFO_CONNECT_TIME CURLINFO_PRETRANSFER_TIME CURLINFO_SIZE_UPLOAD CURLINFO_SIZE_DOWNLOAD CURLINFO_SPEED_DOWNLOAD CURLINFO_SPEED_UPLOAD CURLINFO_FILETIME CURLINFO_SSL_VERIFYRESULT CURLINFO_CONTENT_LENGTH_DOWNLOAD CURLINFO_CONTENT_LENGTH_UPLOAD CURLINFO_STARTTRANSFER_TIME CURLINFO_CONTENT_TYPE CURLINFO_REDIRECT_TIME CURLINFO_REDIRECT_COUNT CURL_TIMECOND_IFMODSINCE CURL_TIMECOND_IFUNMODSINCE CURL_TIMECOND_LASTMOD CURL_VERSION_IPV6 CURL_VERSION_KERBEROS4 CURL_VERSION_SSL CURL_VERSION_LIBZ CURLVERSION_NOW CURLE_OK CURLE_UNSUPPORTED_PROTOCOL CURLE_FAILED_INIT CURLE_URL_MALFORMAT CURLE_URL_MALFORMAT_USER CURLE_COULDNT_RESOLVE_PROXY CURLE_COULDNT_RESOLVE_HOST CURLE_COULDNT_CONNECT CURLE_FTP_WEIRD_SERVER_REPLY CURLE_FTP_ACCESS_DENIED CURLE_FTP_USER_PASSWORD_INCORRECT CURLE_FTP_WEIRD_PASS_REPLY CURLE_FTP_WEIRD_USER_REPLY CURLE_FTP_WEIRD_PASV_REPLY CURLE_FTP_WEIRD_227_FORMAT CURLE_FTP_CANT_GET_HOST CURLE_FTP_CANT_RECONNECT CURLE_FTP_COULDNT_SET_BINARY CURLE_PARTIAL_FILE CURLE_FTP_COULDNT_RETR_FILE CURLE_FTP_WRITE_ERROR CURLE_FTP_QUOTE_ERROR CURLE_HTTP_NOT_FOUND CURLE_WRITE_ERROR CURLE_MALFORMAT_USER CURLE_FTP_COULDNT_STOR_FILE CURLE_READ_ERROR CURLE_OUT_OF_MEMORY CURLE_OPERATION_TIMEOUTED CURLE_FTP_COULDNT_SET_ASCII CURLE_FTP_PORT_FAILED CURLE_FTP_COULDNT_USE_REST CURLE_FTP_COULDNT_GET_SIZE CURLE_HTTP_RANGE_ERROR CURLE_HTTP_POST_ERROR CURLE_SSL_CONNECT_ERROR CURLE_FTP_BAD_DOWNLOAD_RESUME CURLE_FILE_COULDNT_READ_FILE CURLE_LDAP_CANNOT_BIND CURLE_LDAP_SEARCH_FAILED CURLE_LIBRARY_NOT_FOUND CURLE_FUNCTION_NOT_FOUND CURLE_ABORTED_BY_CALLBACK CURLE_BAD_FUNCTION_ARGUMENT CURLE_BAD_CALLING_ORDER CURLE_HTTP_PORT_FAILED CURLE_BAD_PASSWORD_ENTERED CURLE_TOO_MANY_REDIRECTS CURLE_UNKNOWN_TELNET_OPTION CURLE_TELNET_OPTION_SYNTAX CURLE_OBSOLETE CURLE_SSL_PEER_CERTIFICATE CURLE_GOT_NOTHING CURLE_SSL_ENGINE_NOTFOUND CURLE_SSL_ENGINE_SETFAILED CURLE_SEND_ERROR CURLE_RECV_ERROR CURLE_SHARE_IN_USE CURLE_SSL_CERTPROBLEM CURLE_SSL_CIPHER CURLE_SSL_CACERT CURLE_BAD_CONTENT_ENCODING CURLE_LDAP_INVALID_URL CURLE_FILESIZE_EXCEEDED CURLE_FTP_SSL_FAILED CURLFTPAUTH_DEFAULT CURLFTPAUTH_SSL CURLFTPAUTH_TLS CURLPROXY_HTTP CURLPROXY_SOCKS5 CURL_NETRC_OPTIONAL CURL_NETRC_IGNORED CURL_NETRC_REQUIRED CURL_HTTP_VERSION_NONE CURL_HTTP_VERSION_1_0 CURL_HTTP_VERSION_1_1 CURLM_CALL_MULTI_PERFORM CURLM_OK CURLM_BAD_HANDLE CURLM_BAD_EASY_HANDLE CURLM_OUT_OF_MEMORY CURLM_INTERNAL_ERROR CURLMSG_DONE
  syn keyword phpFunctions contained curl_close curl_copy_handle curl_errno curl_error curl_exec curl_getinfo curl_init curl_multi_add_handle curl_multi_close curl_multi_exec curl_multi_getcontent curl_multi_info_read curl_multi_init curl_multi_remove_handle curl_multi_select curl_setopt_array curl_setopt curl_version

  " Cybercash
  syn keyword phpFunctions contained cybercash_base64_decode cybercash_base64_encode cybercash_decr cybercash_encr

  " Cybermut
  syn keyword phpFunctions contained cybermut_creerformulairecm cybermut_creerreponsecm cybermut_testmac

  " Cyrus IMAP administration
  syn keyword phpCoreConstant contained CYRUS_CONN_NONSYNCLITERAL CYRUS_CONN_INITIALRESPONSE CYRUS_CALLBACK_NUMBERED CYRUS_CALLBACK_NOLITERAL
  syn keyword phpFunctions contained cyrus_authenticate cyrus_bind cyrus_close cyrus_connect cyrus_query cyrus_unbind

  " Date/Time functions
  syn keyword phpCoreConstant contained DATE_ATOM DATE_COOKIE DATE_ISO8601 DATE_RFC822 DATE_RFC850 DATE_RFC1036 DATE_RFC1123 DATE_RFC2822 DATE_RFC3339 DATE_RSS DATE_W3C SUNFUNCS_RET_TIMESTAMP SUNFUNCS_RET_STRING SUNFUNCS_RET_DOUBLE
  syn keyword phpFunctions contained checkdate date_create date_date_set date_default_timezone_get date_default_timezone_set date_format date_isodate_set date_modify date_offset_get date_parse date_sun_info date_sunrise date_sunset date_time_set date_timezone_get date_timezone_set date getdate gettimeofday gmdate gmmktime gmstrftime idate localtime microtime mktime strftime strptime strtotime time timezone_abbreviations_list timezone_identifiers_list timezone_name_from_abbr timezone_name_get timezone_offset_get timezone_open timezone_transitions_get
  syn keyword phpClasses contained DateTime DateTimeZone

  " Database (dbm-style) Abstraction Layer Functions 
  syn keyword phpFunctions contained dba_close dba_delete dba_exists dba_fetch dba_firstkey dba_handlers dba_insert dba_key_split dba_list dba_nextkey dba_open dba_optimize dba_popen dba_replace dba_sync

  " dBase functions
  syn keyword phpFunctions contained dbase_add_record dbase_close dbase_create dbase_delete_record dbase_get_header_info dbase_get_record_with_names dbase_get_record dbase_numfields dbase_numrecords dbase_open dbase_pack dbase_replace_record

  " NOTE: DBM functions are deprecated as of PHP 5.0
  " syn keyword phpFunctions contained dblist dbmclose dbmdelete dbmexists dbmfetch dbmfirstkey dbminsert dbmnextkey dbmopen dbmreplace

  " DBX Functions
  syn keyword phpCoreConstant contained DBX_MYSQL DBX_ODBC DBX_PGSQL DBX_MSSQL DBX_FBSQL DBX_OCI8 DBX_SYBASECT DBX_SQLITE DBX_PERSISTENT DBX_RESULT_INFO DBX_RESULT_INDEX DBX_RESULT_ASSOC DBX_RESULT_UNBUFFERED DBX_COLNAMES_UNCHANGED DBX_COLNAMES_UPPERCASE DBX_COLNAMES_LOWERCASE DBX_CMP_NATIVE DBX_CMP_TEXT DBX_CMP_NUMBER DBX_CMP_ASC DBX_CMP_DESC
  syn keyword phpFunctions contained dbx_close dbx_compare dbx_connect dbx_error dbx_escape_string dbx_fetch_row dbx_query dbx_sort

  " Direct I/O functions
  " NOTE: this extension also defines the constant 'c', but I am not declaring
  " it here because most people will never use it ...
  syn keyword phpCoreConstant contained F_DUPFD F_GETFD F_GETFL F_GETLK F_GETOWN F_RDLCK F_SETFL F_SETLK F_SETLKW F_SETOWN F_UNLCK F_WRLCK O_APPEND O_ASYNC O_CREAT O_EXCL O_NDELAY O_NOCTTY O_NONBLOCK O_RDONLY O_RDWR O_SYNC O_TRUNC O_WRONLY S_IRGRP S_IROTH S_IRUSR S_IRWXG S_IRWXO S_IRWXU S_IWGRP S_IWOTH S_IWUSR S_IXGRP S_IXOTH S_IXUSR
  syn keyword phpFunctions contained dio_close dio_fcntl dio_open dio_read dio_seek dio_stat dio_tcsetattr dio_truncate dio_write

  " Directory functions
  syn keyword phpCoreConstant contained DIRECTORY_SEPARATOR PATH_SEPARATOR
  syn keyword phpFunctions contained chdir chroot dir closedir getcwd opendir readdir rewinddir scandir
  syn keyword phpClasses contained Directory

  " DOM functions
  syn keyword phpClasses contained DOMAttr DOMCharacterData DOMComment DOMDocument DOMDocumentFragment DOMDocumentType DOMElement DOMEntity DOMEntityReference DOMException DOMImplementation DOMNamedNodeMap DOMNode DOMNodeList DOMNotation DOMProcessingInstruction DOMText DOMXPath
  syn keyword phpCoreConstant contained XML_ELEMENT_NODE XML_ATTRIBUTE_NODE XML_TEXT_NODE XML_CDATA_SECTION_NODE XML_ENTITY_REF_NODE XML_ENTITY_NODE XML_PI_NODE XML_COMMENT_NODE XML_DOCUMENT_NODE XML_DOCUMENT_TYPE_NODE XML_DOCUMENT_FRAG_NODE XML_NOTATION_NODE XML_HTML_DOCUMENT_NODE XML_DTD_NODE XML_ELEMENT_DECL_NODE XML_ATTRIBUTE_DECL_NODE XML_ENTITY_DECL_NODE XML_NAMESPACE_DECL_NODE XML_ATTRIBUTE_CDATA XML_ATTRIBUTE_ID XML_ATTRIBUTE_IDREF XML_ATTRIBUTE_IDREFS XML_ATTRIBUTE_ENTITY XML_ATTRIBUTE_NMTOKEN XML_ATTRIBUTE_NMTOKENS XML_ATTRIBUTE_ENUMERATION XML_ATTRIBUTE_NOTATION DOM_INDEX_SIZE_ERR DOMSTRING_SIZE_ERR DOM_HIERARCHY_REQUEST_ERR DOM_WRONG_DOCUMENT_ERR DOM_INVALID_CHARACTER_ERR DOM_NO_DATA_ALLOWED_ERR DOM_NO_MODIFICATION_ALLOWED_ERR DOM_NOT_FOUND_ERR DOM_NOT_SUPPORTED_ERR DOM_INUSE_ATTRIBUTE_ERR DOM_INVALID_STATE_ERR DOM_SYNTAX_ERR DOM_INVALID_MODIFICATION_ERR DOM_NAMESPACE_ERR DOM_INVALID_ACCESS_ERR DOM_VALIDATION_ERR
  syn keyword phpFunctions contained dom_import_simplexml

  " DOM XML functions
  "  NOTE: DOM XML is deprecated in favour of the new 'DOM' extension (they
  "  appear to contain overlapping functionality)
  " syn keyword phpCoreConstant contained XML_ELEMENT_NODE XML_ATTRIBUTE_NODE XML_TEXT_NODE XML_CDATA_SECTION_NODE XML_ENTITY_REF_NODE XML_ENTITY_NODE XML_PI_NODE XML_COMMENT_NODE XML_DOCUMENT_NODE XML_DOCUMENT_TYPE_NODE XML_DOCUMENT_FRAG_NODE XML_NOTATION_NODE XML_GLOBAL_NAMESPACE XML_LOCAL_NAMESPACE XML_HTML_DOCUMENT_NODE XML_DTD_NODE XML_ELEMENT_DECL_NODE XML_ATTRIBUTE_DECL_NODE XML_ENTITY_DECL_NODE XML_NAMESPACE_DECL_NODE XML_ATTRIBUTE_CDATA XML_ATTRIBUTE_ID XML_ATTRIBUTE_IDREF XML_ATTRIBUTE_IDREFS XML_ATTRIBUTE_ENTITY XML_ATTRIBUTE_NMTOKEN XML_ATTRIBUTE_NMTOKENS XML_ATTRIBUTE_ENUMERATION XML_ATTRIBUTE_NOTATION XPATH_UNDEFINED XPATH_NODESET XPATH_BOOLEAN XPATH_NUMBER XPATH_STRING XPATH_POINT XPATH_RANGE XPATH_LOCATIONSET XPATH_USERS XPATH_NUMBER
  " syn keyword phpClasses contained DomAttribute DomCData DomComment DomDocument DomDocumentType DomElement DomEntity DomEntityReference DomProcessingInstruction DomText Parser XPathContext
  " syn keyword phpFunctions contained domxml_new_doc domxml_open_file domxml_open_mem domxml_version domxml_xmltree domxml_xslt_stylesheet_doc domxml_xslt_stylesheet_file domxml_xslt_stylesheet domxml_xslt_version xpath_eval_expression xpath_eval xpath_new_context xpath_register_ns_auto xpath_register_ns xptr_eval xptr_new_context

  " Enchant functions
  syn keyword phpFunctions contained enchant_broker_describe enchant_broker_dict_exists enchant_broker_free_dict enchant_broker_free enchant_broker_get_error enchant_broker_init enchant_broker_list_dicts enchant_broker_request_dict enchant_broker_request_pwl_dict enchant_broker_set_ordering enchant_dict_add_to_personal enchant_dict_add_to_session enchant_dict_check enchant_dict_describe enchant_dict_get_error enchant_dict_is_in_session enchant_dict_quick_check enchant_dict_store_replacement enchant_dict_suggest

  " error-handling
  syn keyword phpCoreConstant contained E_ERROR E_WARNING E_PARSE E_NOTICE E_CORE_ERROR E_CORE_WARNING E_COMPILE_ERROR E_COMPILE_WARNING E_USER_ERROR E_USER_WARNING E_USER_NOTICE E_STRICT E_RECOVERABLE_ERROR E_ALL
  syn keyword phpFunctions contained debug_backtrace debug_print_backtrace error_get_last error_log error_reporting restore_error_handler restore_exception_handler set_error_handler set_exception_handler trigger_error user_error

  " exif functions
  syn keyword phpCoreConstant contained EXIF_USE_MBSTRING
  syn keyword phpCoreConstant contained IMAGETYPE_GIF IMAGETYPE_JPEG IMAGETYPE_PNG IMAGETYPE_SWF IMAGETYPE_PSD IMAGETYPE_BMP IMAGETYPE_TIFF_II IMAGETYPE_TIFF_MM IMAGETYPE_JPC IMAGETYPE_JP2 IMAGETYPE_JPX IMAGETYPE_JB2 IMAGETYPE_SWC IMAGETYPE_IFF IMAGETYPE_WBMP IMAGETYPE_XBM
  syn keyword phpFunctions contained exif_imagetype exif_read_data exif_tagname exif_thumbnail read_exif_data

  " expect functions
  syn keyword phpCoreConstant contained EXP_GLOB EXP_EXACT EXP_REGEXP EXP_EOF EXP_TIMEOUT EXP_FULLBUFFER
  syn keyword phpFunctions contained expect_expectl expect_popen

  " FAM functions
  syn keyword phpCoreConstant contained FAMChanged FAMDeleted FAMStartExecuting FAMStopExecuting FAMCreated FAMMoved FAMAcknowledge FAMExists FAMEndExist
  syn keyword phpFunctions contained fam_cancel_monitor fam_close fam_monitor_collection fam_monitor_directory fam_monitor_file fam_next_event fam_open fam_pending fam_resume_monitor fam_suspend_monitor

  " FDF functions
  syn keyword phpCoreConstant contained FDFValue FDFStatus FDFFile FDFID FDFFf FDFSetFf FDFClearFf FDFFlags FDFSetF FDFClrF FDFAP FDFAS FDFAction FDFAA FDFAPRef FDFIF FDFEnter FDFExit FDFDown FDFUp FDFFormat FDFValidate FDFKeystroke FDFCalculate FDFNormalAP FDFRolloverAP FDFDownAP
  syn keyword phpFunctions contained fdf_add_doc_javascript fdf_add_template fdf_close fdf_create fdf_enum_values fdf_errno fdf_error fdf_get_ap fdf_get_attachment fdf_get_encoding fdf_get_file fdf_get_flags fdf_get_opt fdf_get_status fdf_get_value fdf_get_version fdf_header fdf_next_field_name fdf_open_string fdf_open fdf_remove_item fdf_save_string fdf_save fdf_set_ap fdf_set_encoding fdf_set_file fdf_set_flags fdf_set_javascript_action fdf_set_on_import_javascript fdf_set_opt fdf_set_status fdf_set_submit_form_action fdf_set_target_frame fdf_set_value fdf_set_version

  " Fileinfo functions
  syn keyword phpCoreConstant contained FILEINFO_NONE FILEINFO_SYMLINK FILEINFO_MIME FILEINFO_COMPRESS FILEINFO_DEVICES FILEINFO_CONTINUE FILEINFO_PRESERVE_ATIME FILEINFO_RAW
  syn keyword phpFunctions contained finfo_buffer finfo_close finfo_file finfo_open finfo_set_flags

  " Filepro functions
  syn keyword phpFunctions contained filepro_fieldcount filepro_fieldname filepro_fieldtype filepro_fieldwidth filepro_retrieve filepro_rowcount filepro

  " Filesystem functions
  syn keyword phpCoreConstant contained GLOB_BRACE GLOB_ONLYDIR GLOB_MARK GLOB_NOSORT GLOB_NOCHECK GLOB_NOESCAPE PATHINFO_DIRNAME PATHINFO_BASENAME PATHINFO_EXTENSION PATHINFO_FILENAME FILE_USE_INCLUDE_PATH FILE_APPEND FILE_IGNORE_NEW_LINES FILE_SKIP_EMPTY_LINES FILE_BINARY FILE_TEXT
  syn keyword phpFunctions contained basename chgrp chmod chown clearstatcache copy delete dirname disk_free_space disk_total_space diskfreespace fclose feof fflush fgetc fgetcsv fgets fgetss file_exists file_get_contents file_put_contents file fileatime filectime filegroup fileinode filemtime fileowner fileperms filesize filetype flock fnmatch fopen fpassthru fputcsv fputs fread fscanf fseek fstat ftell ftruncate fwrite glob is_dir is_executable is_file is_link is_readable is_uploaded_file is_writable is_writeable lchgrp lchown link linkinfo lstat mkdir move_uploaded_file parse_ini_file pathinfo pclose popen readfile readlink realpath rename rewind rmdir set_file_buffer stat symlink tempnam tmpfile touch umask unlink

  " Filter extension
  syn keyword phpCoreConstant contained INPUT_POST INPUT_GET INPUT_COOKIE INPUT_ENV INPUT_SERVER INPUT_SESSION INPUT_REQUEST FILTER_FLAG_NONE FILTER_REQUIRE_SCALAR FILTER_REQUIRE_ARRAY FILTER_FORCE_ARRAY FILTER_NULL_ON_FAILURE FILTER_VALIDATE_INT FILTER_VALIDATE_BOOLEAN FILTER_VALIDATE_FLOAT FILTER_VALIDATE_REGEXP FILTER_VALIDATE_URL FILTER_VALIDATE_EMAIL FILTER_VALIDATE_IP FILTER_DEFAULT FILTER_UNSAFE_RAW FILTER_SANITIZE_STRING FILTER_SANITIZE_STRIPPED FILTER_SANITIZE_ENCODED FILTER_SANITIZE_SPECIAL_CHARS FILTER_SANITIZE_EMAIL FILTER_SANITIZE_URL FILTER_SANITIZE_NUMBER_INT FILTER_SANITIZE_NUMBER_FLOAT FILTER_SANITIZE_MAGIC_QUOTES FILTER_CALLBACK FILTER_FLAG_ALLOW_OCTAL FILTER_FLAG_ALLOW_HEX FILTER_FLAG_STRIP_LOW FILTER_FLAG_STRIP_HIGH FILTER_FLAG_ENCODE_LOW FILTER_FLAG_ENCODE_HIGH FILTER_FLAG_ENCODE_AMP FILTER_FLAG_NO_ENCODE_QUOTES FILTER_FLAG_EMPTY_STRING_NULL FILTER_FLAG_ALLOW_FRACTION FILTER_FLAG_ALLOW_THOUSAND FILTER_FLAG_ALLOW_SCIENTIFIC FILTER_FLAG_SCHEME_REQUIRED FILTER_FLAG_HOST_REQUIRED FILTER_FLAG_PATH_REQUIRED FILTER_FLAG_QUERY_REQUIRED FILTER_FLAG_IPV4 FILTER_FLAG_IPV6 FILTER_FLAG_NO_RES_RANGE FILTER_FLAG_NO_PRIV_RANGE 
  syn keyword phpFunctions contained filter_has_var filter_id filter_input_array filter_input filter_list filter_var_array filter_var

  " Firebird / interbase functions
  syn keyword phpCoreConstant contained IBASE_DEFAULT IBASE_READ IBASE_WRITE IBASE_CONSISTENCY IBASE_CONCURRENCY IBASE_COMMITTED IBASE_WAIT IBASE_NOWAIT IBASE_FETCH_BLOBS IBASE_FETCH_ARRAYS IBASE_UNIXTIME IBASE_BKP_IGNORE_CHECKSUMS IBASE_BKP_IGNORE_LIMBO IBASE_BKP_METADATA_ONLY IBASE_BKP_NO_GARBAGE_COLLECT IBASE_BKP_OLD_DESCRIPTIONS IBASE_BKP_NON_TRANSPORTABLE IBASE_BKP_CONVERT IBASE_RES_DEACTIVATE_IDX IBASE_RES_NO_SHADOW IBASE_RES_NO_VALIDITY IBASE_RES_ONE_AT_A_TIME IBASE_RES_REPLACE IBASE_RES_CREATE IBASE_RES_USE_ALL_SPACE IBASE_PRP_PAGE_BUFFERS IBASE_PRP_SWEEP_INTERVAL IBASE_PRP_SHUTDOWN_DB IBASE_PRP_DENY_NEW_TRANSACTIONS IBASE_PRP_DENY_NEW_ATTACHMENTS IBASE_PRP_RESERVE_SPACE IBASE_PRP_RES_USE_FULL IBASE_PRP_RES IBASE_PRP_WRITE_MODE IBASE_PRP_WM_ASYNC IBASE_PRP_WM_SYNC IBASE_PRP_ACCESS_MODE IBASE_PRP_AM_READONLY IBASE_PRP_AM_READWRITE IBASE_PRP_SET_SQL_DIALECT IBASE_PRP_ACTIVATE IBASE_PRP_DB_ONLINE IBASE_RPR_CHECK_DB IBASE_RPR_IGNORE_CHECKSUM IBASE_RPR_KILL_SHADOWS IBASE_RPR_MEND_DB IBASE_RPR_VALIDATE_DB IBASE_RPR_FULL IBASE_RPR_SWEEP_DB IBASE_STS_DATA_PAGES IBASE_STS_DB_LOG IBASE_STS_HDR_PAGES IBASE_STS_IDX_PAGES IBASE_STS_SYS_RELATIONS IBASE_SVC_SERVER_VERSION IBASE_SVC_IMPLEMENTATION IBASE_SVC_GET_ENV IBASE_SVC_GET_ENV_LOCK IBASE_SVC_GET_ENV_MSG IBASE_SVC_USER_DBPATH IBASE_SVC_SVR_DB_INFO IBASE_SVC_GET_USERS
  syn keyword phpFunctions contained ibase_add_user ibase_affected_rows ibase_backup ibase_blob_add ibase_blob_cancel ibase_blob_close ibase_blob_create ibase_blob_echo ibase_blob_get ibase_blob_import ibase_blob_info ibase_blob_open ibase_close ibase_commit_ret ibase_commit ibase_connect ibase_db_info ibase_delete_user ibase_drop_db ibase_errcode ibase_errmsg ibase_execute ibase_fetch_assoc ibase_fetch_object ibase_fetch_row ibase_field_info ibase_free_event_handler ibase_free_query ibase_free_result ibase_gen_id ibase_maintain_db ibase_modify_user ibase_name_result ibase_num_fields ibase_num_params ibase_param_info ibase_pconnect ibase_prepare ibase_query ibase_restore ibase_rollback_ret ibase_rollback ibase_server_info ibase_service_attach ibase_service_detach ibase_set_event_handler ibase_timefmt ibase_trans ibase_wait_event

  " FriDiBi functions
  syn keyword phpCoreConstant contained FRIBIDI_CHARSET_UTF8 FRIBIDI_CHARSET_8859_6 FRIBIDI_CHARSET_8859_8 FRIBIDI_CHARSET_CP1255 FRIBIDI_CHARSET_CP1256 FRIBIDI_CHARSET_ISIRI_3342 FRIBIDI_CHARSET_CAP_RTL FRIBIDI_RTL FRIBIDI_LTR FRIBIDI_AUTO
  syn keyword phpFunctions contained fribidi_log2vis

  " FrontBase functions
  syn keyword phpCoreConstant contained FBSQL_ASSOC FBSQL_NUM FBSQL_BOTH FBSQL_LOCK_DEFERRED FBSQL_LOCK_OPTIMISTIC FBSQL_LOCK_PESSIMISTIC FBSQL_ISO_READ_UNCOMMITTED FBSQL_ISO_READ_COMMITTED FBSQL_ISO_REPEATABLE_READ FBSQL_ISO_SERIALIZABLE FBSQL_ISO_VERSIONED FBSQL_UNKNOWN FBSQL_STOPPED FBSQL_STARTING FBSQL_RUNNING FBSQL_STOPPING FBSQL_NOEXEC FBSQL_LOB_DIRECT FBSQL_LOB_HANDLE
  syn keyword phpFunctions contained fbsql_affected_rows fbsql_autocommit fbsql_blob_size fbsql_change_user fbsql_clob_size fbsql_close fbsql_commit fbsql_connect fbsql_create_blob fbsql_create_clob fbsql_create_db fbsql_data_seek fbsql_database_password fbsql_database fbsql_db_query fbsql_db_status fbsql_drop_db fbsql_errno fbsql_error fbsql_fetch_array fbsql_fetch_assoc fbsql_fetch_field fbsql_fetch_lengths fbsql_fetch_object fbsql_fetch_row fbsql_field_flags fbsql_field_len fbsql_field_name fbsql_field_seek fbsql_field_table fbsql_field_type fbsql_free_result fbsql_get_autostart_info fbsql_hostname fbsql_insert_id fbsql_list_dbs fbsql_list_fields fbsql_list_tables fbsql_next_result fbsql_num_fields fbsql_num_rows fbsql_password fbsql_pconnect fbsql_query fbsql_read_blob fbsql_read_clob fbsql_result fbsql_rollback fbsql_rows_fetched fbsql_select_db fbsql_set_characterset fbsql_set_lob_mode fbsql_set_password fbsql_set_transaction fbsql_start_db fbsql_stop_db fbsql_table_name fbsql_tablename fbsql_username fbsql_warnings

  " FTP functions
  syn keyword phpCoreConstant contained FTP_ASCII FTP_TEXT FTP_BINARY FTP_IMAGE FTP_TIMEOUT_SEC FTP_AUTOSEEK FTP_AUTORESUME FTP_FAILED FTP_FINISHED FTP_MOREDATA
  syn keyword phpFunctions contained ftp_alloc ftp_cdup ftp_chdir ftp_chmod ftp_close ftp_connect ftp_delete ftp_exec ftp_fget ftp_fput ftp_get_option ftp_get ftp_login ftp_mdtm ftp_mkdir ftp_nb_continue ftp_nb_fget ftp_nb_fput ftp_nb_get ftp_nb_put ftp_nlist ftp_pasv ftp_put ftp_pwd ftp_quit ftp_raw ftp_rawlist ftp_rename ftp_rmdir ftp_set_option ftp_site ftp_size ftp_ssl_connect ftp_systype

  " Function Handling Functions
  syn keyword phpFunctions contained call_user_func_array call_user_func create_function func_get_arg func_get_args func_num_args function_exists get_defined_functions register_shutdown_function register_tick_function unregister_tick_function

  " GeoIP Functions
  syn keyword phpCoreConstant contained GEOIP_COUNTRY_EDITION GEOIP_REGION_EDITION_REV0 GEOIP_CITY_EDITION_REV0 GEOIP_ORG_EDITION GEOIP_ISP_EDITION GEOIP_CITY_EDITION_REV1 GEOIP_REGION_EDITION_REV1 GEOIP_PROXY_EDITION GEOIP_ASNUM_EDITION GEOIP_NETSPEED_EDITION GEOIP_DOMAIN_EDITION GEOIP_UNKNOWN_SPEED GEOIP_DIALUP_SPEED GEOIP_CABLEDSL_SPEED GEOIP_CORPORATE_SPEED
  syn keyword phpFunctions contained geoip_country_code_by_name geoip_country_code3_by_name geoip_country_name_by_name geoip_database_info geoip_db_avail geoip_db_filename geoip_db_get_all_info geoip_id_by_name geoip_org_by_name geoip_record_by_name geoip_region_by_name

  " Gettext functions
  syn keyword phpFunctions contained bind_textdomain_codeset bindtextdomain dcgettext dcngettext dgettext dngettext gettext ngettext textdomain

  " GMP Function
  syn keyword phpCoreConstant contained GMP_ROUND_ZERO GMP_ROUND_PLUSINF GMP_ROUND_MINUSINF GMP_VERSION
  syn keyword phpFunctions contained gmp_abs gmp_add gmp_and gmp_clrbit gmp_cmp gmp_com gmp_div_q gmp_div_qr gmp_div_r gmp_div gmp_divexact gmp_fact gmp_gcd gmp_gcdext gmp_hamdist gmp_init gmp_intval gmp_invert gmp_jacobi gmp_legendre gmp_mod gmp_mul gmp_neg gmp_nextprime gmp_or gmp_perfect_square gmp_popcount gmp_pow gmp_powm gmp_prob_prime gmp_random gmp_scan0 gmp_scan1 gmp_setbit gmp_sign gmp_sqrt gmp_sqrtrem gmp_strval gmp_sub gmp_testbit gmp_xor

  " gnupg Functions
  syn keyword phpCoreConstant contained GNUPG_SIG_MODE_NORMAL GNUPG_SIG_MODE_DETACH GNUPG_SIG_MODE_CLEAR GNUPG_VALIDITY_UNKNOWN GNUPG_VALIDITY_UNDEFINED GNUPG_VALIDITY_NEVER GNUPG_VALIDITY_MARGINAL GNUPG_VALIDITY_FULL GNUPG_VALIDITY_ULTIMATE GNUPG_PROTOCOL_OpenPGP GNUPG_PROTOCOL_CMS GNUPG_SIGSUM_VALID GNUPG_SIGSUM_GREEN GNUPG_SIGSUM_RED GNUPG_SIGSUM_KEY_REVOKED GNUPG_SIGSUM_KEY_EXPIRED GNUPG_SIGSUM_KEY_MISSING GNUPG_SIGSUM_SIG_EXPIRED GNUPG_SIGSUM_CRL_MISSING GNUPG_SIGSUM_CRL_TOO_OLD GNUPG_SIGSUM_BAD_POLICY GNUPG_SIGSUM_SYS_ERROR GNUPG_ERROR_WARNING GNUPG_ERROR_EXCEPTION GNUPG_ERROR_SILENT
  syn keyword phpFunctions contained gnupg_adddecryptkey gnupg_addencryptkey gnupg_addsignkey gnupg_cleardecryptkeys gnupg_clearencryptkeys gnupg_clearsignkeys gnupg_decrypt gnupg_decryptverify gnupg_encrypt gnupg_encryptsign gnupg_export gnupg_geterror gnupg_getprotocol gnupg_import gnupg_keyinfo gnupg_setarmor gnupg_seterrormode gnupg_setsignmode gnupg_sign gnupg_verify

  " Net_Gopher
  syn keyword phpCoreConstant contained GOPHER_DOCUMENT GOPHER_DIRECTORY GOPHER_BINHEX GOPHER_DOSBINARY GOPHER_UUENCODED GOPHER_BINARY GOPHER_INFO GOPHER_HTTP GOPHER_UNKNOWN
  syn keyword phpFunctions contained gopher_parsedir

  " hash functions
  syn keyword phpCoreConstant contained HASH_HMAC
  syn keyword phpFunctions contained hash_algos hash_file hash_final hash_hmac_file hash_hmac hash_init hash_update_file hash_update_stream hash_update hash

  " HTTP functions
  " TODO: I've never seen these classes before ... make sure they work / are available
  syn keyword phpCoreConstant contained HTTP_SUPPORT HTTP_SUPPORT_REQUESTS HTTP_SUPPORT_MAGICMIME HTTP_SUPPORT_ENCODINGS HTTP_SUPPORT_SSLREQUESTS HTTP_PARAMS_ALLOW_COMMA HTTP_PARAMS_ALLOW_FAILURE HTTP_PARAMS_RAISE_ERROR HTTP_PARAMS_DEFAULT HTTP_COOKIE_PARSE_RAW HTTP_COOKIE_SECURE HTTP_COOKIE_HTTPONLY HTTP_DEFLATE_LEVEL_DEF HTTP_DEFLATE_LEVEL_MIN HTTP_DEFLATE_LEVEL_MAX HTTP_DEFLATE_TYPE_ZLIB HTTP_DEFLATE_TYPE_GZIP HTTP_DEFLATE_TYPE_RAW HTTP_DEFLATE_STRATEGY_DEF HTTP_DEFLATE_STRATEGY_FILT HTTP_DEFLATE_STRATEGY_HUFF HTTP_DEFLATE_STRATEGY_RLE HTTP_DEFLATE_STRATEGY_FIXED HTTP_ENCODING_STREAM_FLUSH_NONE HTTP_ENCODING_STREAM_FLUSH_SYNC HTTP_ENCODING_STREAM_FLUSH_FULL HTTP_E_RUNTIME HTTP_E_INVALID_PARAM HTTP_E_HEADER HTTP_E_MALFORMED_HEADERS HTTP_E_REQUEST_METHOD HTTP_E_MESSAGE_TYPE HTTP_E_ENCODING HTTP_E_REQUEST HTTP_E_REQUEST_POOL HTTP_E_SOCKET HTTP_E_RESPONSE HTTP_E_URL HTTP_E_QUERYSTRING HTTP_MSG_NONE HTTP_MSG_REQUEST HTTP_MSG_RESPONSE HTTP_QUERYSTRING_TYPE_BOOL HTTP_QUERYSTRING_TYPE_INT HTTP_QUERYSTRING_TYPE_FLOAT HTTP_QUERYSTRING_TYPE_STRING
  syn keyword phpCoreConstant contained HTTP_QUERYSTRING_TYPE_ARRAY HTTP_QUERYSTRING_TYPE_OBJECT HTTP_AUTH_BASIC HTTP_AUTH_DIGEST HTTP_AUTH_NTLM HTTP_AUTH_GSSNEG HTTP_AUTH_ANY HTTP_VERSION_ANY HTTP_VERSION_1_0 HTTP_VERSION_1_1 HTTP_SSL_VERSION_ANY HTTP_SSL_VERSION_TLSv1 HTTP_SSL_VERSION_SSLv3 HTTP_SSL_VERSION_SSLv2 HTTP_PROXY_SOCKS4 HTTP_PROXY_SOCKS5 HTTP_PROXY_HTTP HTTP_IPRESOLVE_V4 HTTP_IPRESOLVE_V6 HTTP_IPRESOLVE_ANY HTTP_METH_GET HTTP_METH_HEAD HTTP_METH_POST HTTP_METH_PUT HTTP_METH_DELETE HTTP_METH_OPTIONS HTTP_METH_TRACE HTTP_METH_CONNECT HTTP_METH_PROPFIND HTTP_METH_PROPPATCH HTTP_METH_MKCOL HTTP_METH_COPY HTTP_METH_MOVE HTTP_METH_LOCK HTTP_METH_UNLOCK HTTP_METH_VERSION_CONTROL HTTP_METH_REPORT HTTP_METH_CHECKOUT HTTP_METH_CHECKIN HTTP_METH_UNCHECKOUT HTTP_METH_MKWORKSPACE HTTP_METH_UPDATE HTTP_METH_LABEL HTTP_METH_MERGE HTTP_METH_BASELINE_CONTROL HTTP_METH_MKACTIVITY HTTP_METH_ACL HTTP_REDIRECT HTTP_REDIRECT_PERM HTTP_REDIRECT_FOUND HTTP_REDIRECT_POST HTTP_REDIRECT_PROXY HTTP_REDIRECT_TEMP HTTP_URL_REPLACE HTTP_URL_JOIN_PATH
  syn keyword phpCoreConstant contained HTTP_URL_JOIN_QUERY HTTP_URL_STRIP_USER HTTP_URL_STRIP_PASS HTTP_URL_STRIP_AUTH HTTP_URL_STRIP_PORT HTTP_URL_STRIP_PATH HTTP_URL_STRIP_QUERY HTTP_URL_STRIP_FRAGMENT HTTP_URL_STRIP_ALL
  syn keyword phpClasses contained HttpMessage HttpQueryString HttpDeflateStream HttpInflateStream HttpRequest HttpRequestPool HttpResponse
  syn keyword phpFunctions contained http_cache_etag http_cache_last_modified http_chunked_decode http_deflate http_inflate http_get_request_body_stream http_get_request_body http_get_request_headers http_date http_support http_match_etag http_match_modified http_match_request_header http_build_cookie http_negotiate_charset http_negotiate_content_type http_negotiate_language ob_deflatehandler ob_etaghandler ob_inflatehandler http_parse_cookie http_parse_headers http_parse_message http_parse_params http_persistent_handles_count http_persistent_handles_ident http_persistent_handles_clean http_get http_head http_post_data http_post_fields http_put_data http_put_file http_put_stream http_request_method_exists http_request_method_name http_request_method_register http_request_method_unregister http_request http_request_body_encode http_redirect http_send_content_disposition http_send_content_type http_send_data http_send_file http_send_last_modified http_send_status http_send_stream http_throttle http_build_str http_build_url

  " Hyperwave functions
  syn keyword phpCoreConstant contained HW_ATTR_LANG HW_ATTR_NR HW_ATTR_NONE
  syn keyword phpFunctions contained hw_Array2Objrec hw_changeobject hw_Children hw_ChildrenObj hw_Close hw_Connect hw_connection_info hw_cp hw_Deleteobject hw_DocByAnchor hw_DocByAnchorObj hw_Document_Attributes hw_Document_BodyTag hw_Document_Content hw_Document_SetContent hw_Document_Size hw_dummy hw_EditText hw_Error hw_ErrorMsg hw_Free_Document hw_GetAnchors hw_GetAnchorsObj hw_GetAndLock hw_GetChildColl hw_GetChildCollObj hw_GetChildDocColl hw_GetChildDocCollObj hw_GetObject hw_GetObjectByQuery hw_GetObjectByQueryColl hw_GetObjectByQueryCollObj hw_GetObjectByQueryObj hw_GetParents hw_GetParentsObj hw_getrellink hw_GetRemote hw_getremotechildren hw_GetSrcByDestObj hw_GetText hw_getusername hw_Identify hw_InCollections hw_Info hw_InsColl hw_InsDoc hw_insertanchors hw_InsertDocument hw_InsertObject hw_mapid hw_Modifyobject hw_mv hw_New_Document hw_objrec2array hw_Output_Document hw_pConnect hw_PipeDocument hw_Root hw_setlinkroot hw_stat hw_Unlock hw_Who

  " Hyperwave API
  syn keyword phpClasses contained HW_API HW_API_Object HW_API_Attribute HW_API_Error HW_API_Content HW_API_Reason 
  syn keyword phpFunctions contained hw_api_object hw_api_content hwapi_hgcsp hw_api_attribute

  " NOTE: i18n functions are not yet available

  " IBM DB2
  syn keyword phpCoreConstant contained DB2_BINARY DB2_CONVERT DB2_PASSTHRU DB2_SCROLLABLE DB2_FORWARD_ONLY DB2_PARAM_IN DB2_PARAM_OUT DB2_PARAM_INOUT DB2_PARAM_FILE DB2_AUTOCOMMIT_ON DB2_AUTOCOMMIT_OFF DB2_DOUBLE DB2_LONG DB2_CHAR DB2_CASE_NATURAL DB2_CASE_LOWER DB2_CASE_UPPER DB2_DEFERRED_PREPARE_ON DB2_DEFERRED_PREPARE_OFF
  syn keyword phpFunctions contained db2_autocommit db2_bind_param db2_client_info db2_close db2_column_privileges db2_columns db2_commit db2_conn_error db2_conn_errormsg db2_connect db2_cursor_type db2_escape_string db2_exec db2_execute db2_fetch_array db2_fetch_assoc db2_fetch_both db2_fetch_object db2_fetch_row db2_field_display_size db2_field_name db2_field_num db2_field_precision db2_field_scale db2_field_type db2_field_width db2_foreign_keys db2_free_result db2_free_stmt db2_get_option db2_lob_read db2_next_result db2_num_fields db2_num_rows db2_pconnect db2_prepare db2_primary_keys db2_procedure_columns db2_procedures db2_result db2_rollback db2_server_info db2_set_option db2_special_columns db2_statistics db2_stmt_error db2_stmt_errormsg db2_table_privileges db2_tables

  " ICONV functions
  syn keyword phpCoreConstant contained ICONV_IMPL ICONV_VERSION ICONV_MIME_DECODE_STRICT ICONV_MIME_DECODE_CONTINUE_ON_ERROR
  syn keyword phpFunctions contained iconv_get_encoding iconv_mime_decode_headers iconv_mime_decode iconv_mime_encode iconv_set_encoding iconv_strlen iconv_strpos iconv_strrpos iconv_substr iconv ob_iconv_handler

  " ID3 functions
  syn keyword phpCoreConstant contained ID3_V1_0 ID3_V1_1 ID3_V2_1 ID3_V2_2 ID3_V2_3 ID3_V2_4 ID3_BEST
  syn keyword phpFunctions contained id3_get_frame_long_name id3_get_frame_short_name id3_get_genre_id id3_get_genre_list id3_get_genre_name id3_get_tag id3_get_version id3_remove_tag id3_set_tag

  " IIS functions
  syn keyword phpCoreConstant contained IIS_READ IIS_WRITE IIS_EXECUTE IIS_SCRIPT IIS_ANONYMOUS IIS_BASIC IIS_NTLM IIS_STARTING IIS_STOPPED IIS_PAUSED IIS_RUNNING
  syn keyword phpFunctions contained iis_add_server iis_get_dir_security iis_get_script_map iis_get_server_by_comment iis_get_server_by_path iis_get_server_rights iis_get_service_state iis_remove_server iis_set_app_settings iis_set_dir_security iis_set_script_map iis_set_server_rights iis_start_server iis_start_service iis_stop_server iis_stop_service

  " Image functions
  syn keyword phpCoreConstant contained GD_VERSION GD_MAJOR_VERSION GD_MINOR_VERSION GD_RELEASE_VERSION GD_EXTRA_VERSION IMG_GIF IMG_JPG IMG_JPEG IMG_PNG IMG_WBMP IMG_XPM IMG_COLOR_TILED IMG_COLOR_STYLED IMG_COLOR_BRUSHED IMG_COLOR_STYLEDBRUSHED IMG_COLOR_TRANSPARENT IMG_ARC_ROUNDED IMG_ARC_PIE IMG_ARC_CHORD IMG_ARC_NOFILL IMG_ARC_EDGED IMG_GD2_RAW IMG_GD2_COMPRESSED IMG_EFFECT_REPLACE IMG_EFFECT_ALPHABLEND IMG_EFFECT_NORMAL IMG_EFFECT_OVERLAY IMG_FILTER_NEGATE IMG_FILTER_GRAYSCALE IMG_FILTER_BRIGHTNESS IMG_FILTER_CONTRAST IMG_FILTER_COLORIZE IMG_FILTER_EDGEDETECT IMG_FILTER_GAUSSIAN_BLUR IMG_FILTER_SELECTIVE_BLUR IMG_FILTER_EMBOSS IMG_FILTER_MEAN_REMOVAL IMG_FILTER_SMOOTH IMAGETYPE_GIF IMAGETYPE_JPEG IMAGETYPE_PNG IMAGETYPE_SWF IMAGETYPE_PSD IMAGETYPE_BMP IMAGETYPE_WBMP IMAGETYPE_XBM IMAGETYPE_TIFF_II IMAGETYPE_TIFF_MM IMAGETYPE_IFF IMAGETYPE_JB2 IMAGETYPE_JPC IMAGETYPE_JP2 IMAGETYPE_JPX IMAGETYPE_SWC IMAGETYPE_ICO PNG_NO_FILTER PNG_FILTER_NONE PNG_FILTER_SUB PNG_FILTER_UP PNG_FILTER_AVG PNG_FILTER_PAETH PNG_ALL_FILTERS
  syn keyword phpFunctions contained gd_info getimagesize image_type_to_extension image_type_to_mime_type image2wbmp imagealphablending imageantialias imagearc imagechar imagecharup imagecolorallocate imagecolorallocatealpha imagecolorat imagecolorclosest imagecolorclosestalpha imagecolorclosesthwb imagecolordeallocate imagecolorexact imagecolorexactalpha imagecolormatch imagecolorresolve imagecolorresolvealpha imagecolorset imagecolorsforindex imagecolorstotal imagecolortransparent imageconvolution imagecopy imagecopymerge imagecopymergegray imagecopyresampled imagecopyresized imagecreate imagecreatefromgd2 imagecreatefromgd2part imagecreatefromgd imagecreatefromgif imagecreatefromjpeg imagecreatefrompng imagecreatefromstring imagecreatefromwbmp imagecreatefromxbm imagecreatefromxpm imagecreatetruecolor imagedashedline imagedestroy imageellipse imagefill imagefilledarc imagefilledellipse imagefilledpolygon imagefilledrectangle imagefilltoborder imagefilter imagefontheight
  syn keyword phpFunctions contained imagefontwidth imageftbbox imagefttext imagegammacorrect imagegd2 imagegd imagegif imagegrabscreen imagegrabwindow imageinterlace imageistruecolor imagejpeg imagelayereffect imageline imageloadfont imagepalettecopy imagepng imagepolygon imagepsbbox imagepsencodefont imagepsextendfont imagepsfreefont imagepsloadfont imagepsslantfont imagepstext imagerectangle imagerotate imagesavealpha imagesetbrush imagesetpixel imagesetstyle imagesetthickness imagesettile imagestring imagestringup imagesx imagesy imagetruecolortopalette imagettfbbox imagettftext imagetypes imagewbmp imagexbm iptcembed iptcparse jpeg2wbmp png2wbmp

  " Imagick Image Library
  " NOTE: this extension is experimental
  " syn keyword phpClasses contained Imagick ImagickDraw ImagickPixel ImagickPixelIterator

  " IMAP POP3 and NNTP functions
  syn keyword phpCoreConstant contained NIL OP_DEBUG OP_READONLY OP_ANONYMOUS OP_SHORTCACHE OP_SILENT OP_PROTOTYPE OP_HALFOPEN OP_EXPUNGE OP_SECURE CL_EXPUNGE FT_UID FT_PEEK FT_NOT FT_INTERNAL FT_PREFETCHTEXT ST_UID ST_SILENT ST_SET CP_UID CP_MOVE SE_UID SE_FREE SE_NOPREFETCH SO_FREE SO_NOSERVER SA_MESSAGES SA_RECENT SA_UNSEEN SA_UIDNEXT SA_UIDVALIDITY SA_ALL LATT_NOINFERIORS LATT_NOSELECT LATT_MARKED LATT_UNMARKED SORTDATE SORTARRIVAL SORTFROM SORTSUBJECT SORTTO SORTCC SORTSIZE TYPETEXT TYPEMULTIPART TYPEMESSAGE TYPEAPPLICATION TYPEAUDIO TYPEIMAGE TYPEVIDEO TYPEOTHER ENC7BIT ENC8BIT ENCBINARY ENCBASE64 ENCQUOTEDPRINTABLE ENCOTHER IMAP_OPENTIMEOUT IMAP_READTIMEOUT IMAP_WRITETIMEOUT IMAP_CLOSETIMEOUT LATT_REFERRAL LATT_HASCHILDREN LATT_HASNOCHILDREN TYPEMODEL
  syn keyword phpFunctions contained imap_8bit imap_alerts imap_append imap_base64 imap_binary imap_body imap_bodystruct imap_check imap_clearflag_full imap_close imap_createmailbox imap_delete imap_deletemailbox imap_errors imap_expunge imap_fetch_overview imap_fetchbody imap_fetchheader imap_fetchstructure imap_get_quota imap_get_quotaroot imap_getacl imap_getmailboxes imap_getsubscribed imap_header imap_headerinfo imap_headers imap_last_error imap_list imap_listmailbox imap_listscan imap_listsubscribed imap_lsub imap_mail_compose imap_mail_copy imap_mail_move imap_mail imap_mailboxmsginfo imap_mime_header_decode imap_msgno imap_num_msg imap_num_recent imap_open imap_ping imap_qprint imap_renamemailbox imap_reopen imap_rfc822_parse_adrlist imap_rfc822_parse_headers imap_rfc822_write_address imap_savebody imap_scanmailbox imap_search imap_set_quota imap_setacl imap_setflag_full imap_sort imap_status imap_subscribe imap_thread imap_timeout imap_uid imap_undelete imap_unsubscribe imap_utf7_decode imap_utf7_encode imap_utf8

  " Informix functions
  syn keyword phpCoreConstant contained IFX_SCROLL IFX_HOLD IFX_LO_RDONLY IFX_LO_WRONLY IFX_LO_APPEND IFX_LO_RDWR IFX_LO_BUFFER IFX_LO_NOBUFFER
  syn keyword phpFunctions contained ifx_affected_rows ifx_blobinfile_mode ifx_byteasvarchar ifx_close ifx_connect ifx_copy_blob ifx_create_blob ifx_create_char ifx_do ifx_error ifx_errormsg ifx_fetch_row ifx_fieldproperties ifx_fieldtypes ifx_free_blob ifx_free_char ifx_free_result ifx_get_blob ifx_get_char ifx_getsqlca ifx_htmltbl_result ifx_nullformat ifx_num_fields ifx_num_rows ifx_pconnect ifx_prepare ifx_query ifx_textasvarchar ifx_update_blob ifx_update_char ifxus_close_slob ifxus_create_slob ifxus_free_slob ifxus_open_slob ifxus_read_slob ifxus_seek_slob ifxus_tell_slob ifxus_write_slob

  " Ingres II Functions
  syn keyword phpCoreConstant contained INGRES_ASSOC INGRES_NUM INGRES_BOTH INGRES_EXT_VERSION INGRES_API_VERSION INGRES_CURSOR_READONLY INGRES_CURSOR_UPDATE INGRES_DATE_MULTINATIONAL INGRES_DATE_MULTINATIONAL4 INGRES_DATE_FINNISH INGRES_DATE_ISO INGRES_DATE_ISO4 INGRES_DATE_GERMAN INGRES_DATE_MDY INGRES_DATE_DMY INGRES_DATE_YMD INGRES_MONEY_LEADING INGRES_MONEY_TRAILING INGRES_STRUCTURE_BTREE INGRES_STRUCTURE_CBTREE INGRES_STRUCTURE_HASH INGRES_STRUCTURE_CHASH INGRES_STRUCTURE_HEAP INGRES_STRUCTURE_CHEAP INGRES_STRUCTURE_ISAM INGRES_STRUCTURE_CISAM
  syn keyword phpFunctions contained ingres_autocommit ingres_close ingres_commit ingres_connect ingres_cursor ingres_errno ingres_error ingres_errsqlstate ingres_fetch_array ingres_fetch_object ingres_fetch_row ingres_field_length ingres_field_name ingres_field_nullable ingres_field_precision ingres_field_scale ingres_field_type ingres_num_fields ingres_num_rows ingres_pconnect ingres_query ingres_rollback

  " IRC Gateway functions
  syn keyword phpFunctions contained ircg_channel_mode ircg_disconnect ircg_eval_ecmascript_params ircg_fetch_error_msg ircg_get_username ircg_html_encode ircg_ignore_add ircg_ignore_del ircg_invite ircg_is_conn_alive ircg_join ircg_kick ircg_list ircg_lookup_format_messages ircg_lusers ircg_msg ircg_names ircg_nick ircg_nickname_escape ircg_nickname_unescape ircg_notice ircg_oper ircg_part ircg_pconnect ircg_register_format_messages ircg_set_current ircg_set_file ircg_set_on_die ircg_topic ircg_who ircg_whois

  " PHP/Java Integration
  " NOTE: this extension is experimental

  " JSON functions
  syn keyword phpFunctions contained json_decode json_encode

  " KADM5
  syn keyword phpCoreConstant contained KRB5_KDB_DISALLOW_POSTDATED KRB5_KDB_DISALLOW_FORWARDABLE KRB5_KDB_DISALLOW_TGT_BASED KRB5_KDB_DISALLOW_RENEWABLE KRB5_KDB_DISALLOW_PROXIABLE KRB5_KDB_DISALLOW_DUP_SKEY KRB5_KDB_DISALLOW_ALL_TIX KRB5_KDB_REQUIRES_PRE_AUTH KRB5_KDB_REQUIRES_HW_AUTH KRB5_KDB_REQUIRES_PWCHANGE KRB5_KDB_DISALLOW_SVR KRB5_KDB_PWCHANGE_SERVER KRB5_KDB_SUPPORT_DESMD5 KRB5_KDB_NEW_PRINC KADM5_PRINCIPAL KADM5_PRINC_EXPIRE_TIME KADM5_LAST_PW_CHANGE KADM5_PW_EXPIRATION KADM5_MAX_LIFE KADM5_MAX_RLIFE KADM5_MOD_NAME KADM5_MOD_TIME KADM5_KVNO KADM5_POLICY KADM5_CLEARPOLICY KADM5_LAST_SUCCESS KADM5_LAST_FAILED KADM5_FAIL_AUTH_COUNT KADM5_RANDKEY KADM5_ATTRIBUTES
  syn keyword phpFunctions contained kadm5_chpass_principal kadm5_create_principal kadm5_delete_principal kadm5_destroy kadm5_flush kadm5_get_policies kadm5_get_principal kadm5_get_principals kadm5_init_with_password kadm5_modify_principal

  " LDAP functions
  syn keyword phpCoreConstant contained LDAP_DEREF_NEVER LDAP_DEREF_SEARCHING LDAP_DEREF_FINDING LDAP_DEREF_ALWAYS LDAP_OPT_DEREF LDAP_OPT_SIZELIMIT LDAP_OPT_TIMELIMIT LDAP_OPT_NETWORK_TIMEOUT LDAP_OPT_PROTOCOL_VERSION LDAP_OPT_ERROR_NUMBER LDAP_OPT_REFERRALS LDAP_OPT_RESTART LDAP_OPT_HOST_NAME LDAP_OPT_ERROR_STRING LDAP_OPT_MATCHED_DN LDAP_OPT_SERVER_CONTROLS LDAP_OPT_CLIENT_CONTROLS LDAP_OPT_DEBUG_LEVEL GSLC_SSL_NO_AUTH GSLC_SSL_ONEWAY_AUTH GSLC_SSL_TWOWAY_AUTH
  syn keyword phpFunctions contained ldap_8859_to_t61 ldap_add ldap_bind ldap_close ldap_compare ldap_connect ldap_count_entries ldap_delete ldap_dn2ufn ldap_err2str ldap_errno ldap_error ldap_explode_dn ldap_first_attribute ldap_first_entry ldap_first_reference ldap_free_result ldap_get_attributes ldap_get_dn ldap_get_entries ldap_get_option ldap_get_values_len ldap_get_values ldap_list ldap_mod_add ldap_mod_del ldap_mod_replace ldap_modify ldap_next_attribute ldap_next_entry ldap_next_reference ldap_parse_reference ldap_parse_result ldap_read ldap_rename ldap_sasl_bind ldap_search ldap_set_option ldap_set_rebind_proc ldap_sort ldap_start_tls ldap_t61_to_8859 ldap_unbind

  " libxml functions
  syn keyword phpClasses contained LibXMLError
  syn keyword phpCoreConstant contained LIBXML_COMPACT LIBXML_DTDATTR LIBXML_DTDLOAD LIBXML_DTDVALID LIBXML_NOBLANKS LIBXML_NOCDATA LIBXML_NOEMPTYTAG LIBXML_NOENT LIBXML_NOERROR LIBXML_NONET LIBXML_NOWARNING LIBXML_NOXMLDECL LIBXML_NSCLEAN LIBXML_XINCLUDE LIBXML_ERR_ERROR LIBXML_ERR_FATAL LIBXML_ERR_NONE LIBXML_ERR_WARNING LIBXML_VERSION LIBXML_DOTTED_VERSION
  syn keyword phpFunctions contained libxml_clear_errors libxml_get_errors libxml_get_last_error libxml_set_streams_context libxml_use_internal_errors

  " Lotus Notes functions
  " NOTE: experimental, no maintainer
  " syn keyword phpFunctions contained notes_body notes_copy_db notes_create_db notes_create_note notes_drop_db notes_find_note notes_header_info notes_list_msgs notes_mark_read notes_mark_unread notes_nav_create notes_search notes_unread notes_version

  " LZF functions
  syn keyword phpFunctions contained lzf_compress lzf_decompress lzf_optimized_for

  " Mail functions
  syn keyword phpFunctions contained ezmlm_hash mail

  " Mailparse functions
  syn keyword phpCoreConstant contained MAILPARSE_EXTRACT_OUTPUT MAILPARSE_EXTRACT_STREAM MAILPARSE_EXTRACT_RETURN
  syn keyword phpFunctions contained mailparse_determine_best_xfer_encoding mailparse_msg_create mailparse_msg_extract_part_file mailparse_msg_extract_part mailparse_msg_extract_whole_part_file mailparse_msg_free mailparse_msg_get_part_data mailparse_msg_get_part mailparse_msg_get_structure mailparse_msg_parse_file mailparse_msg_parse mailparse_rfc822_parse_addresses mailparse_stream_encode mailparse_uudecode_all

  " Mathematical functions
  syn keyword phpCoreConstant contained M_PI M_E M_LOG2E M_LOG10E M_LN2 M_LN10 M_PI_2 M_PI_4 M_1_PI M_2_PI M_SQRTPI M_2_SQRTPI M_SQRT2 M_SQRT3 M_SQRT1_2 M_LNPI M_EULER
  syn keyword phpFunctions contained abs acos acosh asin asinh atan2 atan atanh base_convert bindec ceil cos cosh decbin dechex decoct deg2rad exp expm1 floor fmod getrandmax hexdec hypot is_finite is_infinite is_nan lcg_value log10 log1p log max min mt_getrandmax mt_rand mt_srand octdec pi pow rad2deg rand round sin sinh sqrt srand tan tanh

  " MaxDB functions
  syn keyword phpClasses contained maxdb maxdb_stmt maxdb_result
  syn keyword phpCoreConstant contained MAXDB_COMPNAME MAXDB_APPLICATION MAXDB_APPVERSION MAXDB_SQLMODE MAXDB_UNICODE MAXDB_TIMEOUT MAXDB_ISOLATIONLEVEL MAXDB_PACKETCOUNT MAXDB_STATEMENTCACHESIZE MAXDB_CURSORPREFIX MAXDB_ASSOC MAXDB_ASSOC_UPPER MAXDB_ASSOC_LOWER MAXDB_BOTH MAXDB_NUM
  syn keyword phpFunctions contained maxdb_affected_rows maxdb_autocommit maxdb_bind_param maxdb_bind_result maxdb_change_user maxdb_character_set_name maxdb_client_encoding maxdb_close_long_data maxdb_close maxdb_commit maxdb_connect_errno maxdb_connect_error maxdb_connect maxdb_data_seek maxdb_debug maxdb_disable_reads_from_master maxdb_disable_rpl_parse maxdb_dump_debug_info maxdb_embedded_connect maxdb_enable_reads_from_master maxdb_enable_rpl_parse maxdb_errno maxdb_error maxdb_escape_string maxdb_execute maxdb_fetch_array maxdb_fetch_assoc maxdb_fetch_field_direct maxdb_fetch_field maxdb_fetch_fields maxdb_fetch_lengths maxdb_fetch_object maxdb_fetch_row maxdb_fetch maxdb_field_count maxdb_field_seek maxdb_field_tell maxdb_free_result maxdb_get_client_info maxdb_get_client_version maxdb_get_host_info maxdb_get_metadata maxdb_get_proto_info maxdb_get_server_info maxdb_get_server_version maxdb_info maxdb_init maxdb_insert_id maxdb_kill maxdb_master_query maxdb_more_results
  syn keyword phpFunctions contained maxdb_multi_query maxdb_next_result maxdb_num_fields maxdb_num_rows maxdb_options maxdb_param_count maxdb_ping maxdb_prepare maxdb_query maxdb_real_connect maxdb_real_escape_string maxdb_real_query maxdb_report maxdb_rollback maxdb_rpl_parse_enabled maxdb_rpl_probe maxdb_rpl_query_type maxdb_select_db maxdb_send_long_data maxdb_send_query maxdb_server_end maxdb_server_init maxdb_set_opt maxdb_sqlstate maxdb_ssl_set maxdb_stat maxdb_stmt_affected_rows maxdb_stmt_bind_param maxdb_stmt_bind_result maxdb_stmt_close_long_data maxdb_stmt_close maxdb_stmt_data_seek maxdb_stmt_errno maxdb_stmt_error maxdb_stmt_execute maxdb_stmt_fetch maxdb_stmt_free_result maxdb_stmt_init maxdb_stmt_num_rows maxdb_stmt_param_count maxdb_stmt_prepare maxdb_stmt_reset maxdb_stmt_result_metadata maxdb_stmt_send_long_data maxdb_stmt_sqlstate maxdb_stmt_store_result maxdb_store_result maxdb_thread_id maxdb_thread_safe maxdb_use_result maxdb_warning_count

  " MCAL functions
  syn keyword phpCoreConstant contained MCAL_SUNDAY MCAL_MONDAY MCAL_TUESDAY MCAL_WEDNESDAY MCAL_THURSDAY MCAL_FRIDAY MCAL_SATURDAY MCAL_JANUARY MCAL_FEBRUARY MCAL_MARCH MCAL_APRIL MCAL_MAY MCAL_JUNE MCAL_JULY MCAL_AUGUST MCAL_SEPTEMBER MCAL_OCTOBER MCAL_NOVEMBER MCAL_DECEMBER MCAL_RECUR_NONE MCAL_RECUR_DAILY MCAL_RECUR_WEEKLY MCAL_RECUR_MONTHLY_MDAY MCAL_RECUR_MONTHLY_WDAY MCAL_RECUR_YEARLY MCAL_M_SUNDAY MCAL_M_MONDAY MCAL_M_TUESDAY MCAL_M_WEDNESDAY MCAL_M_THURSDAY MCAL_M_FRIDAY MCAL_M_SATURDAY MCAL_M_WEEKDAYS MCAL_M_WEEKEND MCAL_M_ALLDAYS
  syn keyword phpFunctions contained mcal_append_event mcal_close mcal_create_calendar mcal_date_compare mcal_date_valid mcal_day_of_week mcal_day_of_year mcal_days_in_month mcal_delete_calendar mcal_delete_event mcal_event_add_attribute mcal_event_init mcal_event_set_alarm mcal_event_set_category mcal_event_set_class mcal_event_set_description mcal_event_set_end mcal_event_set_recur_daily mcal_event_set_recur_monthly_mday mcal_event_set_recur_monthly_wday mcal_event_set_recur_none mcal_event_set_recur_weekly mcal_event_set_recur_yearly mcal_event_set_start mcal_event_set_title mcal_expunge mcal_fetch_current_stream_event mcal_fetch_event mcal_is_leap_year mcal_list_alarms mcal_list_events mcal_next_recurrence mcal_open mcal_popen mcal_rename_calendar mcal_reopen mcal_snooze mcal_store_event mcal_time_valid mcal_week_of_year

  " Mcrypt Encryption functions
  syn keyword phpCoreConstant contained MCRYPT_MODE_ECB MCRYPT_MODE_CBC MCRYPT_MODE_CFB MCRYPT_MODE_OFB MCRYPT_MODE_NOFB MCRYPT_MODE_STREAM MCRYPT_ENCRYPT MCRYPT_DECRYPT MCRYPT_DEV_RANDOM MCRYPT_DEV_URANDOM MCRYPT_RAND MCRYPT_3DES MCRYPT_ARCFOUR_IV MCRYPT_ARCFOUR MCRYPT_BLOWFISH MCRYPT_CAST_128 MCRYPT_CAST_256 MCRYPT_CRYPT MCRYPT_DES MCRYPT_DES_COMPAT MCRYPT_ENIGMA MCRYPT_GOST MCRYPT_IDEA MCRYPT_LOKI97 MCRYPT_MARS MCRYPT_PANAMA MCRYPT_RIJNDAEL_128 MCRYPT_RIJNDAEL_192 MCRYPT_RIJNDAEL_256 MCRYPT_RC2 MCRYPT_RC4 MCRYPT_RC6 MCRYPT_RC6_128 MCRYPT_RC6_192 MCRYPT_RC6_256 MCRYPT_SAFER64 MCRYPT_SAFER128 MCRYPT_SAFERPLUS MCRYPT_SERPENT(libmcrypt MCRYPT_SERPENT_128 MCRYPT_SERPENT_192 MCRYPT_SERPENT_256 MCRYPT_SKIPJACK MCRYPT_TEAN MCRYPT_THREEWAY MCRYPT_TRIPLEDES MCRYPT_TWOFISH MCRYPT_TWOFISH128 MCRYPT_TWOFISH192 MCRYPT_TWOFISH256 MCRYPT_WAKE MCRYPT_XTEA
  syn keyword phpFunctions contained mcrypt_cbc mcrypt_cfb mcrypt_create_iv mcrypt_decrypt mcrypt_ecb mcrypt_enc_get_algorithms_name mcrypt_enc_get_block_size mcrypt_enc_get_iv_size mcrypt_enc_get_key_size mcrypt_enc_get_modes_name mcrypt_enc_get_supported_key_sizes mcrypt_enc_is_block_algorithm_mode mcrypt_enc_is_block_algorithm mcrypt_enc_is_block_mode mcrypt_enc_self_test mcrypt_encrypt mcrypt_generic_deinit mcrypt_generic_end mcrypt_generic_init mcrypt_generic mcrypt_get_block_size mcrypt_get_cipher_name mcrypt_get_iv_size mcrypt_get_key_size mcrypt_list_algorithms mcrypt_list_modes mcrypt_module_close mcrypt_module_get_algo_block_size mcrypt_module_get_algo_key_size mcrypt_module_get_supported_key_sizes mcrypt_module_is_block_algorithm_mode mcrypt_module_is_block_algorithm mcrypt_module_is_block_mode mcrypt_module_open mcrypt_module_self_test mcrypt_ofb mdecrypt_generic

  " MCVE (Monetra) Payment functions
  syn keyword phpCoreConstant contained M_PENDING M_DONE M_ERROR M_FAIL M_SUCCESS
  syn keyword phpFunctions contained m_checkstatus m_completeauthorizations m_connect m_connectionerror m_deletetrans m_destroyconn m_destroyengine m_getcell m_getcellbynum m_getcommadelimited m_getheader m_initconn m_initengine m_iscommadelimited m_maxconntimeout m_monitor m_numcolumns m_numrows m_parsecommadelimited m_responsekeys m_responseparam m_returnstatus m_setblocking m_setdropfile m_setip m_setssl_cafile m_setssl_files m_setssl m_settimeout m_sslcert_gen_hash m_transactionssent m_transinqueue m_transkeyval m_transnew m_transsend m_uwait m_validateidentifier m_verifyconnection m_verifysslcert

  " Memcache functions
  syn keyword phpClasses contained Memcache
  syn keyword phpCoreConstant contained MEMCACHE_COMPRESSED MEMCACHE_HAVE_SESSION
  syn keyword phpFunctions contained memcache_add memcache_add_server memcache_close memcache_connect memcache_debug memcache_decrement memcache_delete memcache_flush memcache_get memcache_get_extended_stats memcache_get_server_status memcache_get_stats memcache_get_version memcache_increment memcache_pconnect memcache_replace memcache_set memcache_set_compress_threshold memcache_set_server_params

  " MHash functions
  syn keyword phpCoreConstant contained MHASH_ADLER32 MHASH_CRC32 MHASH_CRC32B MHASH_GOST MHASH_HAVAL128 MHASH_HAVAL160 MHASH_HAVAL192 MHASH_HAVAL256 MHASH_MD4 MHASH_MD5 MHASH_RIPEMD160 MHASH_SHA1 MHASH_SHA256 MHASH_TIGER MHASH_TIGER128 MHASH_TIGER160 
  syn keyword phpFunctions contained mhash_count mhash_get_block_size mhash_get_hash_name mhash_keygen_s2k mhash

  " Mimetype functions
  " NOTE: has been deprecated in favour of the Fileinfo extension
  " syn keyword phpFunctions contained mime_content_type

  " Ming functions for flash
  " NOTE: this extension is experimental
  " syn keyword phpCoreConstant contained MING_NEW MING_ZLIB SWFBUTTON_HIT SWFBUTTON_DOWN SWFBUTTON_OVER SWFBUTTON_UP SWFBUTTON_MOUSEUPOUTSIDE SWFBUTTON_DRAGOVER SWFBUTTON_DRAGOUT SWFBUTTON_MOUSEUP SWFBUTTON_MOUSEDOWN SWFBUTTON_MOUSEOUT SWFBUTTON_MOUSEOVER SWFFILL_RADIAL_GRADIENT SWFFILL_LINEAR_GRADIENT SWFFILL_TILED_BITMAP SWFFILL_CLIPPED_BITMAP SWFTEXTFIELD_HASLENGTH SWFTEXTFIELD_NOEDIT SWFTEXTFIELD_PASSWORD SWFTEXTFIELD_MULTILINE SWFTEXTFIELD_WORDWRAP SWFTEXTFIELD_DRAWBOX SWFTEXTFIELD_NOSELECT SWFTEXTFIELD_HTML SWFTEXTFIELD_ALIGN_LEFT SWFTEXTFIELD_ALIGN_RIGHT SWFTEXTFIELD_ALIGN_CENTER SWFTEXTFIELD_ALIGN_JUSTIFY SWFACTION_ONLOAD SWFACTION_ENTERFRAME SWFACTION_UNLOAD SWFACTION_MOUSEMOVE SWFACTION_MOUSEDOWN SWFACTION_MOUSEUP SWFACTION_KEYDOWN SWFACTION_KEYUP SWFACTION_DATA
  " syn keyword phpClasses contained SWFAction SWFBitmap SWFButton SWFDisplayItem SWFFill SWFFont SWFFontChar SWFGradient SWFMorph SWFMovie SWFPrebuiltClip SWFShape SWFSound SWFSoundInstance SWFSprite SWFText SWFTextField SWFVideoStream
  " syn keyword phpFunctions contained ming_keypress ming_setcubicthreshold ming_setscale ming_setswfcompression ming_useconstants ming_useswfversion

  " Miscellaneous functions
  " NOTE: php_check_syntax was removed after PHP 5.0.4
  " NOTE: some of the functions like exit() and die() are defined elsewhere
  syn keyword phpCoreConstant contained CONNECTION_ABORTED CONNECTION_NORMAL CONNECTION_TIMEOUT __COMPILER_HALT_OFFSET__
  syn keyword phpFunctions contained connection_aborted connection_status connection_timeout constant define defined get_browser highlight_file highlight_string ignore_user_abort pack php_strip_whitespace show_source sleep sys_getloadavg time_nanosleep time_sleep_until uniqid unpack usleep

  " mnoGoSearch functions
  syn keyword phpCoreConstant contained UDM_FIELD_URLID UDM_FIELD_URL UDM_FIELD_CONTENT UDM_FIELD_TITLE UDM_FIELD_KEYWORDS UDM_FIELD_DESC UDM_FIELD_DESCRIPTION UDM_FIELD_TEXT UDM_FIELD_SIZE UDM_FIELD_RATING UDM_FIELD_SCORE UDM_FIELD_MODIFIED UDM_FIELD_ORDER UDM_FIELD_CRC UDM_FIELD_CATEGORY UDM_FIELD_LANG UDM_FIELD_CHARSET UDM_PARAM_PAGE_SIZE UDM_PARAM_PAGE_NUM UDM_PARAM_SEARCH_MODE UDM_PARAM_CACHE_MODE UDM_PARAM_TRACK_MODE UDM_PARAM_PHRASE_MODE UDM_PARAM_CHARSET UDM_PARAM_LOCAL_CHARSET UDM_PARAM_BROWSER_CHARSET UDM_PARAM_STOPTABLE UDM_PARAM_STOP_TABLE UDM_PARAM_STOPFILE UDM_PARAM_STOP_FILE UDM_PARAM_WEIGHT_FACTOR UDM_PARAM_WORD_MATCH UDM_PARAM_MAX_WORD_LEN UDM_PARAM_MAX_WORDLEN UDM_PARAM_MIN_WORD_LEN UDM_PARAM_MIN_WORDLEN UDM_PARAM_ISPELL_PREFIXES UDM_PARAM_ISPELL_PREFIX UDM_PARAM_PREFIXES UDM_PARAM_PREFIX UDM_PARAM_CROSS_WORDS UDM_PARAM_CROSSWORDS UDM_PARAM_VARDIR UDM_PARAM_DATADIR UDM_PARAM_HLBEG UDM_PARAM_HLEND UDM_PARAM_SYNONYM UDM_PARAM_SEARCHD UDM_PARAM_QSTRING UDM_PARAM_REMOTE_ADDR UDM_LIMIT_CAT UDM_LIMIT_URL UDM_LIMIT_TAG UDM_LIMIT_LANG UDM_LIMIT_DATE UDM_PARAM_FOUND UDM_PARAM_NUM_ROWS UDM_PARAM_WORDINFO UDM_PARAM_WORD_INFO UDM_PARAM_SEARCHTIME UDM_PARAM_SEARCH_TIME UDM_PARAM_FIRST_DOC UDM_PARAM_LAST_DOC UDM_MODE_ALL UDM_MODE_ANY UDM_MODE_BOOL UDM_MODE_PHRASE UDM_CACHE_ENABLED UDM_CACHE_DISABLED UDM_TRACK_ENABLED UDM_TRACK_DISABLED UDM_PHRASE_ENABLED UDM_PHRASE_DISABLED UDM_CROSS_WORDS_ENABLED UDM_CROSSWORDS_ENABLED UDM_CROSS_WORDS_DISABLED UDM_CROSSWORDS_DISABLED UDM_PREFIXES_ENABLED UDM_PREFIX_ENABLED UDM_ISPELL_PREFIXES_ENABLED UDM_ISPELL_PREFIX_ENABLED UDM_PREFIXES_DISABLED UDM_PREFIX_DISABLED UDM_ISPELL_PREFIXES_DISABLED UDM_ISPELL_PREFIX_DISABLED UDM_ISPELL_TYPE_AFFIX UDM_ISPELL_TYPE_SPELL UDM_ISPELL_TYPE_DB UDM_ISPELL_TYPE_SERVER UDM_MATCH_WORD UDM_MATCH_BEGIN UDM_MATCH_SUBSTR UDM_MATCH_END
  syn keyword phpFunctions contained udm_add_search_limit udm_alloc_agent_array udm_alloc_agent udm_api_version udm_cat_list udm_cat_path udm_check_charset udm_check_stored udm_clear_search_limits udm_close_stored udm_crc32 udm_errno udm_error udm_find udm_free_agent udm_free_ispell_data udm_free_res udm_get_doc_count udm_get_res_field udm_get_res_param udm_hash32 udm_load_ispell_data udm_open_stored udm_set_agent_param

  " Microsoft SQL server functions
  syn keyword phpCoreConstant contained MSSQL_ASSOC MSSQL_NUM MSSQL_BOTH SQLTEXT SQLVARCHAR SQLCHAR SQLINT1 SQLINT2 SQLINT4 SQLBIT SQLFLT8
  syn keyword phpFunctions contained mssql_bind mssql_close mssql_connect mssql_data_seek mssql_execute mssql_fetch_array mssql_fetch_assoc mssql_fetch_batch mssql_fetch_field mssql_fetch_object mssql_fetch_row mssql_field_length mssql_field_name mssql_field_seek mssql_field_type mssql_free_result mssql_free_statement mssql_get_last_message mssql_guid_string mssql_init mssql_min_error_severity mssql_min_message_severity mssql_next_result mssql_num_fields mssql_num_rows mssql_pconnect mssql_query mssql_result mssql_rows_affected mssql_select_db

  " Mohawk Software Session Handler Functions
  syn keyword phpFunctions contained msession_connect msession_count msession_create msession_destroy msession_disconnect msession_find msession_get_array msession_get_data msession_get msession_inc msession_list msession_listvar msession_lock msession_plugin msession_randstr msession_set_array msession_set_data msession_set msession_timeout msession_uniq msession_unlock

  " mSQL functions
  syn keyword phpCoreConstant contained MSQL_ASSOC MSQL_NUM MSQL_BOTH
  syn keyword phpFunctions contained msql_affected_rows msql_close msql_connect msql_create_db msql_createdb msql_data_seek msql_db_query msql_dbname msql_drop_db msql_error msql_fetch_array msql_fetch_field msql_fetch_object msql_fetch_row msql_field_flags msql_field_len msql_field_name msql_field_seek msql_field_table msql_field_type msql_fieldflags msql_fieldlen msql_fieldname msql_fieldtable msql_fieldtype msql_free_result msql_list_dbs msql_list_fields msql_list_tables msql_num_fields msql_num_rows msql_numfields msql_numrows msql_pconnect msql_query msql_regcase msql_result msql_select_db msql_tablename msql

  " Multibyte string functions
  syn keyword phpCoreConstant contained MB_OVERLOAD_MAIL MB_OVERLOAD_STRING MB_OVERLOAD_REGEX MB_CASE_UPPER MB_CASE_LOWER MB_CASE_TITLE
  syn keyword phpFunctions contained mb_check_encoding mb_convert_case mb_convert_encoding mb_convert_kana mb_convert_variables mb_decode_mimeheader mb_decode_numericentity mb_detect_encoding mb_detect_order mb_encode_mimeheader mb_encode_numericentity mb_ereg_match mb_ereg_replace mb_ereg_search_getpos mb_ereg_search_getregs mb_ereg_search_init mb_ereg_search_pos mb_ereg_search_regs mb_ereg_search_setpos mb_ereg_search mb_ereg mb_eregi_replace mb_eregi mb_get_info mb_http_input mb_http_output mb_internal_encoding mb_language mb_output_handler mb_parse_str mb_preferred_mime_name mb_regex_encoding mb_regex_set_options mb_send_mail mb_split mb_strcut mb_strimwidth mb_stripos mb_stristr mb_strlen mb_strpos mb_strrchr mb_strrichr mb_strripos mb_strrpos mb_strstr mb_strtolower mb_strtoupper mb_strwidth mb_substitute_character mb_substr_count mb_substr

  " muscat functions
  " NOTE: Experimental, doesn't seem to be necessary any more

  " MySQL functions
  syn keyword phpCoreConstant contained MYSQL_CLIENT_COMPRESS MYSQL_CLIENT_IGNORE_SPACE MYSQL_CLIENT_INTERACTIVE MYSQL_CLIENT_SSL MYSQL_ASSOC MYSQL_BOTH MYSQL_NUM
  syn keyword phpFunctions contained mysql_affected_rows mysql_change_user mysql_client_encoding mysql_close mysql_connect mysql_create_db mysql_data_seek mysql_db_name mysql_db_query mysql_drop_db mysql_errno mysql_error mysql_escape_string mysql_fetch_array mysql_fetch_assoc mysql_fetch_field mysql_fetch_lengths mysql_fetch_object mysql_fetch_row mysql_field_flags mysql_field_len mysql_field_name mysql_field_seek mysql_field_table mysql_field_type mysql_free_result mysql_get_client_info mysql_get_host_info mysql_get_proto_info mysql_get_server_info mysql_info mysql_insert_id mysql_list_dbs mysql_list_fields mysql_list_processes mysql_list_tables mysql_num_fields mysql_num_rows mysql_pconnect mysql_ping mysql_query mysql_real_escape_string mysql_result mysql_select_db mysql_set_charset mysql_stat mysql_tablename mysql_thread_id mysql_unbuffered_query

  " MySQL Improved extension
  syn keyword phpClasses contained mysqli mysqli_stmt mysqli_result
  syn keyword phpCoreConstant contained MYSQLI_READ_DEFAULT_GROUP MYSQLI_READ_DEFAULT_FILE MYSQLI_OPT_CONNECT_TIMEOUT MYSQLI_OPT_LOCAL_INFILE MYSQLI_INIT_COMMAND MYSQLI_CLIENT_SSL MYSQLI_CLIENT_COMPRESS MYSQLI_CLIENT_INTERACTIVE MYSQLI_CLIENT_IGNORE_SPACE MYSQLI_CLIENT_NO_SCHEMA MYSQLI_CLIENT_MULTI_QUERIES MYSQLI_STORE_RESULT MYSQLI_USE_RESULT MYSQLI_ASSOC MYSQLI_NUM MYSQLI_BOTH MYSQLI_NOT_NULL_FLAG MYSQLI_PRI_KEY_FLAG MYSQLI_UNIQUE_KEY_FLAG MYSQLI_MULTIPLE_KEY_FLAG MYSQLI_BLOB_FLAG MYSQLI_UNSIGNED_FLAG MYSQLI_ZEROFILL_FLAG MYSQLI_AUTO_INCREMENT_FLAG MYSQLI_TIMESTAMP_FLAG MYSQLI_SET_FLAG MYSQLI_NUM_FLAG MYSQLI_PART_KEY_FLAG MYSQLI_GROUP_FLAG MYSQLI_TYPE_DECIMAL MYSQLI_TYPE_NEWDECIMAL MYSQLI_TYPE_BIT MYSQLI_TYPE_TINY MYSQLI_TYPE_SHORT MYSQLI_TYPE_LONG MYSQLI_TYPE_FLOAT MYSQLI_TYPE_DOUBLE MYSQLI_TYPE_NULL MYSQLI_TYPE_TIMESTAMP MYSQLI_TYPE_LONGLONG MYSQLI_TYPE_INT24 MYSQLI_TYPE_DATE MYSQLI_TYPE_TIME MYSQLI_TYPE_DATETIME MYSQLI_TYPE_YEAR MYSQLI_TYPE_NEWDATE MYSQLI_TYPE_ENUM MYSQLI_TYPE_SET MYSQLI_TYPE_TINY_BLOB MYSQLI_TYPE_MEDIUM_BLOB MYSQLI_TYPE_LONG_BLOB MYSQLI_TYPE_BLOB MYSQLI_TYPE_VAR_STRING MYSQLI_TYPE_STRING MYSQLI_TYPE_GEOMETRY MYSQLI_NEED_DATA MYSQLI_NO_DATA MYSQLI_DATA_TRUNCATED
  syn keyword phpFunctions contained mysqli_affected_rows mysqli_autocommit mysqli_bind_param mysqli_bind_result mysqli_change_user mysqli_character_set_name mysqli_client_encoding mysqli_close mysqli_commit mysqli_connect_errno mysqli_connect_error mysqli_connect mysqli_data_seek mysqli_debug mysqli_disable_reads_from_master mysqli_disable_rpl_parse mysqli_dump_debug_info mysqli_embedded_server_end mysqli_embedded_server_start mysqli_enable_reads_from_master mysqli_enable_rpl_parse mysqli_errno mysqli_error mysqli_escape_string mysqli_execute mysqli_fetch_array mysqli_fetch_assoc mysqli_fetch_field_direct mysqli_fetch_field mysqli_fetch_fields mysqli_fetch_lengths mysqli_fetch_object mysqli_fetch_row mysqli_fetch mysqli_field_count mysqli_field_seek mysqli_field_tell mysqli_free_result mysqli_get_charset mysqli_get_client_info mysqli_get_client_version mysqli_get_host_info mysqli_get_metadata mysqli_get_proto_info mysqli_get_server_info mysqli_get_server_version mysqli_get_warnings
  syn keyword phpFunctions contained mysqli_info mysqli_init mysqli_insert_id mysqli_kill mysqli_master_query mysqli_more_results mysqli_multi_query mysqli_next_result mysqli_num_fields mysqli_num_rows mysqli_options mysqli_param_count mysqli_ping mysqli_prepare mysqli_query mysqli_real_connect mysqli_real_escape_string mysqli_real_query mysqli_report mysqli_rollback mysqli_rpl_parse_enabled mysqli_rpl_probe mysqli_rpl_query_type mysqli_select_db mysqli_send_long_data mysqli_send_query mysqli_server_end mysqli_server_init mysqli_set_charset mysqli_set_local_infile_default mysqli_set_local_infile_handler mysqli_set_opt mysqli_slave_query mysqli_sqlstate mysqli_ssl_set mysqli_stat mysqli_stmt_affected_rows mysqli_stmt_attr_get mysqli_stmt_attr_set mysqli_stmt_bind_param mysqli_stmt_bind_result mysqli_stmt_close mysqli_stmt_data_seek mysqli_stmt_errno mysqli_stmt_error mysqli_stmt_execute mysqli_stmt_fetch mysqli_stmt_field_count mysqli_stmt_free_result mysqli_stmt_get_warnings
  syn keyword phpFunctions contained mysqli_stmt_init mysqli_stmt_insert_id mysqli_stmt_num_rows mysqli_stmt_param_count mysqli_stmt_prepare mysqli_stmt_reset mysqli_stmt_result_metadata mysqli_stmt_send_long_data mysqli_stmt_sqlstate mysqli_stmt_store_result mysqli_store_result mysqli_thread_id mysqli_thread_safe mysqli_use_result mysqli_warning_count

  " ncurses extension
  " NOTE: this extension is experimental
  syn keyword phpCoreConstant contained NCURSES_ERR NCURSES_COLOR_BLACK NCURSES_COLOR_WHITE NCURSES_COLOR_RED NCURSES_COLOR_GREEN NCURSES_COLOR_YELLOW NCURSES_COLOR_BLUE NCURSES_COLOR_CYAN NCURSES_COLOR_MAGENTA
  " Keyboard
  syn match phpCoreConstant contained /\<NCURSES_KEY_F\%(6[0-4]\=\|[1-5][0-9]\|[0-9]\)\>/
  syn keyword phpCoreConstant contained NCURSES_KEY_DOWN NCURSES_KEY_UP NCURSES_KEY_LEFT NCURSES_KEY_RIGHT NCURSES_KEY_HOME NCURSES_KEY_BACKSPACE NCURSES_KEY_DL NCURSES_KEY_IL NCURSES_KEY_DC NCURSES_KEY_IC NCURSES_KEY_EIC NCURSES_KEY_CLEAR NCURSES_KEY_EOS NCURSES_KEY_EOL NCURSES_KEY_SF NCURSES_KEY_SR NCURSES_KEY_NPAGE NCURSES_KEY_PPAGE NCURSES_KEY_STAB NCURSES_KEY_CTAB NCURSES_KEY_CATAB NCURSES_KEY_SRESET NCURSES_KEY_RESET NCURSES_KEY_PRINT NCURSES_KEY_LL NCURSES_KEY_A1 NCURSES_KEY_A3 NCURSES_KEY_B2 NCURSES_KEY_C1 NCURSES_KEY_C3 NCURSES_KEY_BTAB NCURSES_KEY_BEG NCURSES_KEY_CANCEL NCURSES_KEY_CLOSE NCURSES_KEY_COMMAND NCURSES_KEY_COPY NCURSES_KEY_CREATE NCURSES_KEY_END NCURSES_KEY_EXIT NCURSES_KEY_FIND NCURSES_KEY_HELP NCURSES_KEY_MARK NCURSES_KEY_MESSAGE NCURSES_KEY_MOVE NCURSES_KEY_NEXT NCURSES_KEY_OPEN NCURSES_KEY_OPTIONS NCURSES_KEY_PREVIOUS NCURSES_KEY_REDO NCURSES_KEY_REFERENCE NCURSES_KEY_REFRESH NCURSES_KEY_REPLACE NCURSES_KEY_RESTART NCURSES_KEY_RESUME
  syn keyword phpCoreConstant contained NCURSES_KEY_SAVE NCURSES_KEY_SBEG NCURSES_KEY_SCANCEL NCURSES_KEY_SCOMMAND NCURSES_KEY_SCOPY NCURSES_KEY_SCREATE NCURSES_KEY_SDC NCURSES_KEY_SDL NCURSES_KEY_SELECT NCURSES_KEY_SEND NCURSES_KEY_SEOL NCURSES_KEY_SEXIT NCURSES_KEY_SFIND NCURSES_KEY_SHELP NCURSES_KEY_SHOME NCURSES_KEY_SIC NCURSES_KEY_SLEFT NCURSES_KEY_SMESSAGE NCURSES_KEY_SMOVE NCURSES_KEY_SNEXT NCURSES_KEY_SOPTIONS NCURSES_KEY_SPREVIOUS NCURSES_KEY_SPRINT NCURSES_KEY_SREDO NCURSES_KEY_SREPLACE NCURSES_KEY_SRIGHT NCURSES_KEY_SRSUME NCURSES_KEY_SSAVE NCURSES_KEY_SSUSPEND NCURSES_KEY_UNDO NCURSES_KEY_MOUSE NCURSES_KEY_MAX
  " Mouse
  syn keyword phpCoreConstant contained NCURSES_BUTTON1_RELEASED NCURSES_BUTTON2_RELEASED NCURSES_BUTTON3_RELEASED NCURSES_BUTTON4_RELEASED NCURSES_BUTTON1_PRESSED NCURSES_BUTTON2_PRESSED NCURSES_BUTTON3_PRESSED NCURSES_BUTTON4_PRESSED NCURSES_BUTTON1_CLICKED NCURSES_BUTTON2_CLICKED NCURSES_BUTTON3_CLICKED NCURSES_BUTTON4_CLICKED NCURSES_BUTTON1_DOUBLE_CLICKED NCURSES_BUTTON2_DOUBLE_CLICKED NCURSES_BUTTON3_DOUBLE_CLICKED NCURSES_BUTTON4_DOUBLE_CLICKED NCURSES_BUTTON1_TRIPLE_CLICKED NCURSES_BUTTON2_TRIPLE_CLICKED NCURSES_BUTTON3_TRIPLE_CLICKED NCURSES_BUTTON4_TRIPLE_CLICKED NCURSES_BUTTON_CTRL NCURSES_BUTTON_SHIFT NCURSES_BUTTON_ALT NCURSES_ALL_MOUSE_EVENTS NCURSES_REPORT_MOUSE_POSITION
  " Functions
  syn keyword phpFunctions contained ncurses_addch ncurses_addchnstr ncurses_addchstr ncurses_addnstr ncurses_addstr ncurses_assume_default_colors ncurses_attroff ncurses_attron ncurses_attrset ncurses_baudrate ncurses_beep ncurses_bkgd ncurses_bkgdset ncurses_border ncurses_bottom_panel ncurses_can_change_color ncurses_cbreak ncurses_clear ncurses_clrtobot ncurses_clrtoeol ncurses_color_content ncurses_color_set ncurses_curs_set ncurses_def_prog_mode ncurses_def_shell_mode ncurses_define_key ncurses_del_panel ncurses_delay_output ncurses_delch ncurses_deleteln ncurses_delwin ncurses_doupdate ncurses_echo ncurses_echochar ncurses_end ncurses_erase ncurses_erasechar ncurses_filter ncurses_flash ncurses_flushinp ncurses_getch ncurses_getmaxyx ncurses_getmouse ncurses_getyx ncurses_halfdelay ncurses_has_colors ncurses_has_ic ncurses_has_il ncurses_has_key ncurses_hide_panel ncurses_hline ncurses_inch ncurses_init_color ncurses_init_pair
  syn keyword phpFunctions contained ncurses_init ncurses_insch ncurses_insdelln ncurses_insertln ncurses_insstr ncurses_instr ncurses_isendwin ncurses_keyok ncurses_keypad ncurses_killchar ncurses_longname ncurses_meta ncurses_mouse_trafo ncurses_mouseinterval ncurses_mousemask ncurses_move_panel ncurses_move ncurses_mvaddch ncurses_mvaddchnstr ncurses_mvaddchstr ncurses_mvaddnstr ncurses_mvaddstr ncurses_mvcur ncurses_mvdelch ncurses_mvgetch ncurses_mvhline ncurses_mvinch ncurses_mvvline ncurses_mvwaddstr ncurses_napms ncurses_new_panel ncurses_newpad ncurses_newwin ncurses_nl ncurses_nocbreak ncurses_noecho ncurses_nonl ncurses_noqiflush ncurses_noraw ncurses_pair_content ncurses_panel_above ncurses_panel_below ncurses_panel_window ncurses_pnoutrefresh ncurses_prefresh ncurses_putp ncurses_qiflush ncurses_raw ncurses_refresh ncurses_replace_panel ncurses_reset_prog_mode ncurses_reset_shell_mode ncurses_resetty ncurses_savetty
  syn keyword phpFunctions contained ncurses_scr_dump ncurses_scr_init ncurses_scr_restore ncurses_scr_set ncurses_scrl ncurses_show_panel ncurses_slk_attr ncurses_slk_attroff ncurses_slk_attron ncurses_slk_attrset ncurses_slk_clear ncurses_slk_color ncurses_slk_init ncurses_slk_noutrefresh ncurses_slk_refresh ncurses_slk_restore ncurses_slk_set ncurses_slk_touch ncurses_standend ncurses_standout ncurses_start_color ncurses_termattrs ncurses_termname ncurses_timeout ncurses_top_panel ncurses_typeahead ncurses_ungetch ncurses_ungetmouse ncurses_update_panels ncurses_use_default_colors ncurses_use_env ncurses_use_extended_names ncurses_vidattr ncurses_vline ncurses_waddch ncurses_waddstr ncurses_wattroff ncurses_wattron ncurses_wattrset ncurses_wborder ncurses_wclear ncurses_wcolor_set ncurses_werase ncurses_wgetch ncurses_whline ncurses_wmouse_trafo ncurses_wmove ncurses_wnoutrefresh ncurses_wrefresh ncurses_wstandend ncurses_wstandout ncurses_wvline

  " network functions
  syn keyword phpCoreConstant contained LOG_CONS LOG_NDELAY LOG_ODELAY LOG_NOWAIT LOG_PERROR LOG_PID LOG_AUTH LOG_AUTHPRIV LOG_CRON LOG_DAEMON LOG_KERN LOG_LOCAL0 LOG_LPR LOG_MAIL LOG_NEWS LOG_SYSLOG LOG_USER LOG_UUCP LOG_EMERG LOG_ALERT LOG_CRIT LOG_ERR LOG_WARNING LOG_NOTICE LOG_INFO LOG_DEBUG DNS_A DNS_MX DNS_CNAME DNS_NS DNS_PTR DNS_HINFO DNS_SOA DNS_TXT DNS_ANY DNS_AAAA DNS_ALL
  syn keyword phpFunctions contained checkdnsrr closelog debugger_off debugger_on define_syslog_variables dns_check_record dns_get_mx dns_get_record fsockopen gethostbyaddr gethostbyname gethostbynamel getmxrr getprotobyname getprotobynumber getservbyname getservbyport header headers_list headers_sent inet_ntop inet_pton ip2long long2ip openlog pfsockopen setcookie setrawcookie socket_get_status socket_set_blocking socket_set_timeout syslog

  " newt functions
  syn keyword phpCoreConstant contained NEWT_EXIT_HOTKEY NEWT_EXIT_COMPONENT NEWT_EXIT_FDREADY NEWT_EXIT_TIMER NEWT_COLORSET_ROOT NEWT_COLORSET_BORDER NEWT_COLORSET_WINDOW NEWT_COLORSET_SHADOW NEWT_COLORSET_TITLE NEWT_COLORSET_BUTTON NEWT_COLORSET_ACTBUTTON NEWT_COLORSET_CHECKBOX NEWT_COLORSET_ACTCHECKBOX NEWT_COLORSET_ENTRY NEWT_COLORSET_LABEL NEWT_COLORSET_LISTBOX NEWT_COLORSET_ACTLISTBOX NEWT_COLORSET_TEXTBOX NEWT_COLORSET_ACTTEXTBOX NEWT_COLORSET_HELPLINE NEWT_COLORSET_ROOTTEXT NEWT_COLORSET_ROOTTEXT NEWT_COLORSET_EMPTYSCALE NEWT_COLORSET_FULLSCALE NEWT_COLORSET_DISENTRY NEWT_COLORSET_COMPACTBUTTON NEWT_COLORSET_ACTSELLISTBOX NEWT_COLORSET_SELLISTBOX NEWT_FLAGS_SET NEWT_FLAGS_RESET NEWT_FLAGS_TOGGLE NEWT_FLAG_RETURNEXIT NEWT_FLAG_HIDDEN NEWT_FLAG_SCROLL NEWT_FLAG_DISABLED NEWT_FLAG_BORDER NEWT_FLAG_WRAP NEWT_FLAG_NOF12 NEWT_FLAG_MULTIPLE NEWT_FLAG_SELECTED NEWT_FLAG_CHECKBOX NEWT_FLAG_PASSWORD NEWT_FLAG_SHOWCURSOR NEWT_FD_READ NEWT_FD_WRITE NEWT_FD_EXCEPT NEWT_CHECKBOXTREE_UNSELECTABLE
  syn keyword phpCoreConstant contained NEWT_CHECKBOXTREE_HIDE_BOX NEWT_CHECKBOXTREE_COLLAPSED NEWT_CHECKBOXTREE_EXPANDED NEWT_CHECKBOXTREE_UNSELECTED NEWT_CHECKBOXTREE_SELECTED NEWT_ENTRY_SCROLL NEWT_ENTRY_HIDDEN NEWT_ENTRY_RETURNEXIT NEWT_ENTRY_DISABLED NEWT_LISTBOX_RETURNEXIT NEWT_TEXTBOX_WRAP NEWT_TEXTBOX_SCROLL NEWT_FORM_NOF12 NEWT_KEY_TAB NEWT_KEY_ENTER NEWT_KEY_SUSPEND NEWT_KEY_ESCAPE NEWT_KEY_RETURN NEWT_KEY_EXTRA_BASE NEWT_KEY_UP NEWT_KEY_DOWN NEWT_KEY_LEFT NEWT_KEY_RIGHT NEWT_KEY_BKSPC NEWT_KEY_DELETE NEWT_KEY_HOME NEWT_KEY_END NEWT_KEY_UNTAB NEWT_KEY_PGUP NEWT_KEY_PGDN NEWT_KEY_INSERT NEWT_KEY_F1 NEWT_KEY_F2 NEWT_KEY_F3 NEWT_KEY_F4 NEWT_KEY_F5 NEWT_KEY_F6 NEWT_KEY_F7 NEWT_KEY_F8 NEWT_KEY_F9 NEWT_KEY_F10 NEWT_KEY_F11 NEWT_KEY_F12 NEWT_KEY_RESIZE NEWT_ANCHOR_LEFT NEWT_ANCHOR_RIGHT NEWT_ANCHOR_TOP NEWT_ANCHOR_BOTTOM NEWT_GRID_FLAG_GROWX NEWT_GRID_FLAG_GROWY NEWT_GRID_EMPTY NEWT_GRID_COMPONENT NEWT_GRID_SUBGRID
  syn keyword phpFunctions contained newt_bell newt_button_bar newt_button newt_centered_window newt_checkbox_get_value newt_checkbox_set_flags newt_checkbox_set_value newt_checkbox_tree_add_item newt_checkbox_tree_find_item newt_checkbox_tree_get_current newt_checkbox_tree_get_entry_value newt_checkbox_tree_get_multi_selection newt_checkbox_tree_get_selection newt_checkbox_tree_multi newt_checkbox_tree_set_current newt_checkbox_tree_set_entry_value newt_checkbox_tree_set_entry newt_checkbox_tree_set_width newt_checkbox_tree newt_checkbox newt_clear_key_buffer newt_cls newt_compact_button newt_component_add_callback newt_component_takes_focus newt_create_grid newt_cursor_off newt_cursor_on newt_delay newt_draw_form newt_draw_root_text newt_entry_get_value newt_entry_set_filter newt_entry_set_flags newt_entry_set newt_entry newt_finished newt_form_add_component newt_form_add_components newt_form_add_hot_key newt_form_destroy newt_form_get_current newt_form_run
  syn keyword phpFunctions contained newt_form_set_background newt_form_set_height newt_form_set_size newt_form_set_timer newt_form_set_width newt_form_watch_fd newt_form newt_get_screen_size newt_grid_add_components_to_form newt_grid_basic_window newt_grid_free newt_grid_get_size newt_grid_h_close_stacked newt_grid_h_stacked newt_grid_place newt_grid_set_field newt_grid_simple_window newt_grid_v_close_stacked newt_grid_v_stacked newt_grid_wrapped_window_at newt_grid_wrapped_window newt_init newt_label_set_text newt_label newt_listbox_append_entry newt_listbox_clear_selection newt_listbox_clear newt_listbox_delete_entry newt_listbox_get_current newt_listbox_get_selection newt_listbox_insert_entry newt_listbox_item_count newt_listbox_select_item newt_listbox_set_current_by_key newt_listbox_set_current newt_listbox_set_data newt_listbox_set_entry newt_listbox_set_width newt_listbox newt_listitem_get_data newt_listitem_set newt_listitem newt_open_window
  syn keyword phpFunctions contained newt_pop_help_line newt_pop_window newt_push_help_line newt_radio_get_current newt_radiobutton newt_redraw_help_line newt_reflow_text newt_refresh newt_resize_screen newt_resume newt_run_form newt_scale_set newt_scale newt_scrollbar_set newt_set_help_callback newt_set_suspend_callback newt_suspend newt_textbox_get_num_lines newt_textbox_reflowed newt_textbox_set_height newt_textbox_set_text newt_textbox newt_vertical_scrollbar newt_wait_for_key newt_win_choice newt_win_entries newt_win_menu newt_win_message newt_win_messagev newt_win_ternary

  " NSAPI functions
  syn keyword phpFunctions contained nsapi_request_headers nsapi_response_headers nsapi_virtual
  " NOTE: these functions are also implemented by the apache module
  syn keyword phpCoreConstant contained apache_request_headers apache_response_headers getallheaders virtual

  " object aggregation functions
  " NOTE: this extension is experimental
  syn keyword phpFunctions contained aggregate_info aggregate_methods_by_list aggregate_methods_by_regexp aggregate_methods aggregate_properties_by_list aggregate_properties_by_regexp aggregate_properties aggregate aggregation_info deaggregate

  " object overloading functions
  " NOTE: experimental and no longer needed in PHP 5
  " syn keyword phpFunctions contained overload





  " TODO: review function list from here:
  syn keyword	phpFunctions	array_change_key_case array_chunk array_combine array_count_values array_diff_assoc array_diff_uassoc array_diff array_fill array_filter array_flip array_intersect_assoc array_intersect array_key_exists array_keys array_map array_merge_recursive array_merge array_multisort array_pad array_pop array_push array_rand array_reduce array_reverse array_search array_shift array_slice array_splice array_sum array_udiff_assoc array_udiff_uassoc array_udiff array_unique array_unshift array_values array_walk arsort asort compact count current each end extract in_array key krsort ksort natcasesort natsort next pos prev range reset rsort shuffle sizeof sort uasort uksort usort	contained
  syn keyword	phpFunctions	bcadd bccomp bcdiv bcmod bcmul bcpow bcpowmod bcscale bcsqrt bcsub	contained
  syn keyword	phpFunctions	bzclose bzcompress bzdecompress bzerrno bzerror bzerrstr bzflush bzopen bzread bzwrite	contained
  syn keyword	phpFunctions	cal_days_in_month cal_from_jd cal_info cal_to_jd easter_date easter_days frenchtojd gregoriantojd jddayofweek jdmonthname jdtofrench jdtogregorian jdtojewish jdtojulian jdtounix jewishtojd juliantojd unixtojd	contained
  syn keyword	phpFunctions	call_user_method_array call_user_method class_exists get_class_methods get_class_vars get_class get_declared_classes get_object_vars get_parent_class is_a is_subclass_of method_exists property_exists contained
  syn keyword	phpFunctions	com VARIANT com_addref com_get com_invoke com_isenum com_load_typelib com_load com_propget com_propput com_propset com_release com_set	contained
  syn keyword	phpFunctions	cpdf_add_annotation cpdf_add_outline cpdf_arc cpdf_begin_text cpdf_circle cpdf_clip cpdf_close cpdf_closepath_fill_stroke cpdf_closepath_stroke cpdf_closepath cpdf_continue_text cpdf_curveto cpdf_end_text cpdf_fill_stroke cpdf_fill cpdf_finalize_page cpdf_finalize cpdf_global_set_document_limits cpdf_import_jpeg cpdf_lineto cpdf_moveto cpdf_newpath cpdf_open cpdf_output_buffer cpdf_page_init cpdf_place_inline_image cpdf_rect cpdf_restore cpdf_rlineto cpdf_rmoveto cpdf_rotate_text cpdf_rotate cpdf_save_to_file cpdf_save cpdf_scale cpdf_set_action_url cpdf_set_char_spacing cpdf_set_creator cpdf_set_current_page cpdf_set_font_directories cpdf_set_font_map_file cpdf_set_font cpdf_set_horiz_scaling cpdf_set_keywords cpdf_set_leading cpdf_set_page_animation cpdf_set_subject cpdf_set_text_matrix cpdf_set_text_pos cpdf_set_text_rendering cpdf_set_text_rise cpdf_set_title cpdf_set_viewer_preferences cpdf_set_word_spacing cpdf_setdash cpdf_setflat cpdf_setgray_fill cpdf_setgray_stroke cpdf_setgray cpdf_setlinecap cpdf_setlinejoin cpdf_setlinewidth cpdf_setmiterlimit cpdf_setrgbcolor_fill cpdf_setrgbcolor_stroke cpdf_setrgbcolor cpdf_show_xy cpdf_show cpdf_stringwidth cpdf_stroke cpdf_text cpdf_translate	contained
  syn keyword	phpFunctions	crack_check crack_closedict crack_getlastmessage crack_opendict	contained
  syn keyword	phpFunctions	ctype_alnum ctype_alpha ctype_cntrl ctype_digit ctype_graph ctype_lower ctype_print ctype_punct ctype_space ctype_upper ctype_xdigit	contained
  syn keyword	phpFunctions	curl_close curl_errno curl_error curl_exec curl_getinfo curl_init curl_multi_add_handle curl_multi_close curl_multi_exec curl_multi_getcontent curl_multi_info_read curl_multi_init curl_multi_remove_handle curl_multi_select curl_setopt curl_version	contained
  syn keyword	phpFunctions	cybercash_base64_decode cybercash_base64_encode cybercash_decr cybercash_encr	contained
  syn keyword	phpFunctions	cyrus_authenticate cyrus_bind cyrus_close cyrus_connect cyrus_query cyrus_unbind	contained
  syn keyword	phpFunctions	checkdate date getdate gettimeofday gmdate gmmktime gmstrftime localtime microtime mktime strftime strtotime time	contained
  syn keyword	phpFunctions	dba_close dba_delete dba_exists dba_fetch dba_firstkey dba_handlers dba_insert dba_key_split dba_list dba_nextkey dba_open dba_optimize dba_popen dba_replace dba_sync	contained
  syn keyword	phpFunctions	dbase_add_record dbase_close dbase_create dbase_delete_record dbase_get_header_info dbase_get_record_with_names dbase_get_record dbase_numfields dbase_numrecords dbase_open dbase_pack dbase_replace_record	contained
  syn keyword	phpFunctions	dblist dbmclose dbmdelete dbmexists dbmfetch dbmfirstkey dbminsert dbmnextkey dbmopen dbmreplace	contained
  syn keyword	phpFunctions	dbplus_add dbplus_aql dbplus_chdir dbplus_close dbplus_curr dbplus_errcode dbplus_errno dbplus_find dbplus_first dbplus_flush dbplus_freealllocks dbplus_freelock dbplus_freerlocks dbplus_getlock dbplus_getunique dbplus_info dbplus_last dbplus_lockrel dbplus_next dbplus_open dbplus_prev dbplus_rchperm dbplus_rcreate dbplus_rcrtexact dbplus_rcrtlike dbplus_resolve dbplus_restorepos dbplus_rkeys dbplus_ropen dbplus_rquery dbplus_rrename dbplus_rsecindex dbplus_runlink dbplus_rzap dbplus_savepos dbplus_setindex dbplus_setindexbynumber dbplus_sql dbplus_tcl dbplus_tremove dbplus_undo dbplus_undoprepare dbplus_unlockrel dbplus_unselect dbplus_update dbplus_xlockrel dbplus_xunlockrel	contained
  syn keyword	phpFunctions	dbx_close dbx_compare dbx_connect dbx_error dbx_escape_string dbx_fetch_row dbx_query dbx_sort	contained
  syn keyword	phpFunctions	dio_close dio_fcntl dio_open dio_read dio_seek dio_stat dio_tcsetattr dio_truncate dio_write	contained
  syn keyword	phpFunctions	chdir chroot dir closedir getcwd opendir readdir rewinddir scandir	contained
  syn keyword	phpFunctions	domxml_new_doc domxml_open_file domxml_open_mem domxml_version domxml_xmltree domxml_xslt_stylesheet_doc domxml_xslt_stylesheet_file domxml_xslt_stylesheet xpath_eval_expression xpath_eval xpath_new_context xptr_eval xptr_new_context	contained
  syn keyword	phpMethods	name specified value create_attribute create_cdata_section create_comment create_element_ns create_element create_entity_reference create_processing_instruction create_text_node doctype document_element dump_file dump_mem get_element_by_id get_elements_by_tagname html_dump_mem xinclude entities internal_subset name notations public_id system_id get_attribute_node get_attribute get_elements_by_tagname has_attribute remove_attribute set_attribute tagname add_namespace append_child append_sibling attributes child_nodes clone_node dump_node first_child get_content has_attributes has_child_nodes insert_before is_blank_node last_child next_sibling node_name node_type node_value owner_document parent_node prefix previous_sibling remove_child replace_child replace_node set_content set_name set_namespace unlink_node data target process result_dump_file result_dump_mem	contained
  syn keyword	phpFunctions	dotnet_load	contained
  syn keyword	phpFunctions	debug_backtrace debug_print_backtrace error_log error_reporting restore_error_handler set_error_handler trigger_error user_error	contained
  syn keyword	phpFunctions	escapeshellarg escapeshellcmd exec passthru proc_close proc_get_status proc_nice proc_open proc_terminate shell_exec system	contained
  syn keyword	phpFunctions	fam_cancel_monitor fam_close fam_monitor_collection fam_monitor_directory fam_monitor_file fam_next_event fam_open fam_pending fam_resume_monitor fam_suspend_monitor	contained
  syn keyword	phpFunctions	fbsql_affected_rows fbsql_autocommit fbsql_change_user fbsql_close fbsql_commit fbsql_connect fbsql_create_blob fbsql_create_clob fbsql_create_db fbsql_data_seek fbsql_database_password fbsql_database fbsql_db_query fbsql_db_status fbsql_drop_db fbsql_errno fbsql_error fbsql_fetch_array fbsql_fetch_assoc fbsql_fetch_field fbsql_fetch_lengths fbsql_fetch_object fbsql_fetch_row fbsql_field_flags fbsql_field_len fbsql_field_name fbsql_field_seek fbsql_field_table fbsql_field_type fbsql_free_result fbsql_get_autostart_info fbsql_hostname fbsql_insert_id fbsql_list_dbs fbsql_list_fields fbsql_list_tables fbsql_next_result fbsql_num_fields fbsql_num_rows fbsql_password fbsql_pconnect fbsql_query fbsql_read_blob fbsql_read_clob fbsql_result fbsql_rollback fbsql_select_db fbsql_set_lob_mode fbsql_set_transaction fbsql_start_db fbsql_stop_db fbsql_tablename fbsql_username fbsql_warnings	contained
  syn keyword	phpFunctions	fdf_add_doc_javascript fdf_add_template fdf_close fdf_create fdf_enum_values fdf_errno fdf_error fdf_get_ap fdf_get_attachment fdf_get_encoding fdf_get_file fdf_get_flags fdf_get_opt fdf_get_status fdf_get_value fdf_get_version fdf_header fdf_next_field_name fdf_open_string fdf_open fdf_remove_item fdf_save_string fdf_save fdf_set_ap fdf_set_encoding fdf_set_file fdf_set_flags fdf_set_javascript_action fdf_set_opt fdf_set_status fdf_set_submit_form_action fdf_set_target_frame fdf_set_value fdf_set_version	contained
  syn keyword	phpFunctions	filepro_fieldcount filepro_fieldname filepro_fieldtype filepro_fieldwidth filepro_retrieve filepro_rowcount filepro	contained
  syn keyword	phpFunctions	basename chgrp chmod chown clearstatcache copy delete dirname disk_free_space disk_total_space diskfreespace fclose feof fflush fgetc fgetcsv fgets fgetss file_exists file_get_contents file_put_contents file fileatime filectime filegroup fileinode filemtime fileowner fileperms filesize filetype flock fnmatch fopen fpassthru fputs fread fscanf fseek fstat ftell ftruncate fwrite glob is_dir is_executable is_file is_link is_readable is_uploaded_file is_writable is_writeable link linkinfo lstat mkdir move_uploaded_file parse_ini_file pathinfo pclose popen readfile readlink realpath rename rewind rmdir set_file_buffer stat symlink tempnam tmpfile touch umask unlink	contained
  syn keyword	phpFunctions	fribidi_log2vis	contained
  syn keyword	phpFunctions	ftp_alloc ftp_cdup ftp_chdir ftp_chmod ftp_close ftp_connect ftp_delete ftp_exec ftp_fget ftp_fput ftp_get_option ftp_get ftp_login ftp_mdtm ftp_mkdir ftp_nb_continue ftp_nb_fget ftp_nb_fput ftp_nb_get ftp_nb_put ftp_nlist ftp_pasv ftp_put ftp_pwd ftp_quit ftp_raw ftp_rawlist ftp_rename ftp_rmdir ftp_set_option ftp_site ftp_size ftp_ssl_connect ftp_systype	contained
  syn keyword	phpFunctions	call_user_func_array call_user_func create_function func_get_arg func_get_args func_num_args function_exists get_defined_functions register_shutdown_function register_tick_function unregister_tick_function	contained
  syn keyword	phpFunctions	bind_textdomain_codeset bindtextdomain dcgettext dcngettext dgettext dngettext gettext ngettext textdomain	contained
  syn keyword	phpFunctions	gmp_abs gmp_add gmp_and gmp_clrbit gmp_cmp gmp_com gmp_div_q gmp_div_qr gmp_div_r gmp_div gmp_divexact gmp_fact gmp_gcd gmp_gcdext gmp_hamdist gmp_init gmp_intval gmp_invert gmp_jacobi gmp_legendre gmp_mod gmp_mul gmp_neg gmp_or gmp_perfect_square gmp_popcount gmp_pow gmp_powm gmp_prob_prime gmp_random gmp_scan0 gmp_scan1 gmp_setbit gmp_sign gmp_sqrt gmp_sqrtrem gmp_sqrtrm gmp_strval gmp_sub gmp_xor	contained
  syn keyword	phpFunctions	header headers_list headers_sent setcookie	contained
  syn keyword	phpFunctions	hw_api_attribute hwapi_hgcsp hw_api_content hw_api_object	contained
  syn keyword	phpMethods	key langdepvalue value values checkin checkout children mimetype read content copy dbstat dcstat dstanchors dstofsrcanchors count reason find ftstat hwstat identify info insert insertanchor insertcollection insertdocument link lock move assign attreditable count insert remove title value object objectbyanchor parents description type remove replace setcommitedversion srcanchors srcsofdst unlock user userlist	contained
  syn keyword	phpFunctions	hw_Array2Objrec hw_changeobject hw_Children hw_ChildrenObj hw_Close hw_Connect hw_connection_info hw_cp hw_Deleteobject hw_DocByAnchor hw_DocByAnchorObj hw_Document_Attributes hw_Document_BodyTag hw_Document_Content hw_Document_SetContent hw_Document_Size hw_dummy hw_EditText hw_Error hw_ErrorMsg hw_Free_Document hw_GetAnchors hw_GetAnchorsObj hw_GetAndLock hw_GetChildColl hw_GetChildCollObj hw_GetChildDocColl hw_GetChildDocCollObj hw_GetObject hw_GetObjectByQuery hw_GetObjectByQueryColl hw_GetObjectByQueryCollObj hw_GetObjectByQueryObj hw_GetParents hw_GetParentsObj hw_getrellink hw_GetRemote hw_getremotechildren hw_GetSrcByDestObj hw_GetText hw_getusername hw_Identify hw_InCollections hw_Info hw_InsColl hw_InsDoc hw_insertanchors hw_InsertDocument hw_InsertObject hw_mapid hw_Modifyobject hw_mv hw_New_Document hw_objrec2array hw_Output_Document hw_pConnect hw_PipeDocument hw_Root hw_setlinkroot hw_stat hw_Unlock hw_Who	contained
  syn keyword	phpFunctions	ibase_add_user ibase_affected_rows ibase_blob_add ibase_blob_cancel ibase_blob_close ibase_blob_create ibase_blob_echo ibase_blob_get ibase_blob_import ibase_blob_info ibase_blob_open ibase_close ibase_commit_ret ibase_commit ibase_connect ibase_delete_user ibase_drop_db ibase_errcode ibase_errmsg ibase_execute ibase_fetch_assoc ibase_fetch_object ibase_fetch_row ibase_field_info ibase_free_event_handler ibase_free_query ibase_free_result ibase_gen_id ibase_modify_user ibase_name_result ibase_num_fields ibase_num_params ibase_param_info ibase_pconnect ibase_prepare ibase_query ibase_rollback_ret ibase_rollback ibase_set_event_handler ibase_timefmt ibase_trans ibase_wait_event	contained
  syn keyword	phpFunctions	iconv_get_encoding iconv_mime_decode_headers iconv_mime_decode iconv_mime_encode iconv_set_encoding iconv_strlen iconv_strpos iconv_strrpos iconv_substr iconv ob_iconv_handler	contained
  syn keyword	phpFunctions	ifx_affected_rows ifx_blobinfile_mode ifx_byteasvarchar ifx_close ifx_connect ifx_copy_blob ifx_create_blob ifx_create_char ifx_do ifx_error ifx_errormsg ifx_fetch_row ifx_fieldproperties ifx_fieldtypes ifx_free_blob ifx_free_char ifx_free_result ifx_get_blob ifx_get_char ifx_getsqlca ifx_htmltbl_result ifx_nullformat ifx_num_fields ifx_num_rows ifx_pconnect ifx_prepare ifx_query ifx_textasvarchar ifx_update_blob ifx_update_char ifxus_close_slob ifxus_create_slob ifxus_free_slob ifxus_open_slob ifxus_read_slob ifxus_seek_slob ifxus_tell_slob ifxus_write_slob	contained
  syn keyword	phpFunctions	exif_imagetype exif_read_data exif_thumbnail gd_info getimagesize image_type_to_mime_type image2wbmp imagealphablending imageantialias imagearc imagechar imagecharup imagecolorallocate imagecolorallocatealpha imagecolorat imagecolorclosest imagecolorclosestalpha imagecolorclosesthwb imagecolordeallocate imagecolorexact imagecolorexactalpha imagecolormatch imagecolorresolve imagecolorresolvealpha imagecolorset imagecolorsforindex imagecolorstotal imagecolortransparent imagecopy imagecopymerge imagecopymergegray imagecopyresampled imagecopyresized imagecreate imagecreatefromgd2 imagecreatefromgd2part imagecreatefromgd imagecreatefromgif imagecreatefromjpeg imagecreatefrompng imagecreatefromstring imagecreatefromwbmp imagecreatefromxbm imagecreatefromxpm imagecreatetruecolor imagedashedline imagedestroy imageellipse imagefill imagefilledarc imagefilledellipse imagefilledpolygon imagefilledrectangle imagefilltoborder imagefontheight imagefontwidth imageftbbox imagefttext imagegammacorrect imagegd2 imagegd imagegif imageinterlace imageistruecolor imagejpeg imageline imageloadfont imagepalettecopy imagepng imagepolygon imagepsbbox imagepscopyfont imagepsencodefont imagepsextendfont imagepsfreefont imagepsloadfont imagepsslantfont imagepstext imagerectangle imagerotate imagesavealpha imagesetbrush imagesetpixel imagesetstyle imagesetthickness imagesettile imagestring imagestringup imagesx imagesy imagetruecolortopalette imagettfbbox imagettftext imagetypes imagewbmp iptcembed iptcparse jpeg2wbmp png2wbmp read_exif_data	contained
  syn keyword	phpFunctions	imap_8bit imap_alerts imap_append imap_base64 imap_binary imap_body imap_bodystruct imap_check imap_clearflag_full imap_close imap_createmailbox imap_delete imap_deletemailbox imap_errors imap_expunge imap_fetch_overview imap_fetchbody imap_fetchheader imap_fetchstructure imap_get_quota imap_get_quotaroot imap_getacl imap_getmailboxes imap_getsubscribed imap_header imap_headerinfo imap_headers imap_last_error imap_list imap_listmailbox imap_listscan imap_listsubscribed imap_lsub imap_mail_compose imap_mail_copy imap_mail_move imap_mail imap_mailboxmsginfo imap_mime_header_decode imap_msgno imap_num_msg imap_num_recent imap_open imap_ping imap_qprint imap_renamemailbox imap_reopen imap_rfc822_parse_adrlist imap_rfc822_parse_headers imap_rfc822_write_address imap_scanmailbox imap_search imap_set_quota imap_setacl imap_setflag_full imap_sort imap_status imap_subscribe imap_thread imap_timeout imap_uid imap_undelete imap_unsubscribe imap_utf7_decode imap_utf7_encode imap_utf8	contained
  syn keyword	phpFunctions	assert_options assert dl extension_loaded get_cfg_var get_current_user get_defined_constants get_extension_funcs get_include_path get_included_files get_loaded_extensions get_magic_quotes_gpc get_magic_quotes_runtime get_required_files getenv getlastmod getmygid getmyinode getmypid getmyuid getopt getrusage ini_alter ini_get_all ini_get ini_restore ini_set main memory_get_usage php_ini_scanned_files php_logo_guid php_sapi_name php_uname phpcredits phpinfo phpversion putenv restore_include_path set_include_path set_magic_quotes_runtime set_time_limit version_compare zend_logo_guid zend_version	contained
  syn keyword	phpFunctions	ingres_autocommit ingres_close ingres_commit ingres_connect ingres_fetch_array ingres_fetch_object ingres_fetch_row ingres_field_length ingres_field_name ingres_field_nullable ingres_field_precision ingres_field_scale ingres_field_type ingres_num_fields ingres_num_rows ingres_pconnect ingres_query ingres_rollback	contained
  syn keyword	phpFunctions	ircg_channel_mode ircg_disconnect ircg_fetch_error_msg ircg_get_username ircg_html_encode ircg_ignore_add ircg_ignore_del ircg_is_conn_alive ircg_join ircg_kick ircg_lookup_format_messages ircg_msg ircg_nick ircg_nickname_escape ircg_nickname_unescape ircg_notice ircg_part ircg_pconnect ircg_register_format_messages ircg_set_current ircg_set_file ircg_set_on_die ircg_topic ircg_whois	contained
  syn keyword	phpFunctions	java_last_exception_clear java_last_exception_get	contained
  syn keyword	phpFunctions	ldap_8859_to_t61 ldap_add ldap_bind ldap_close ldap_compare ldap_connect ldap_count_entries ldap_delete ldap_dn2ufn ldap_err2str ldap_errno ldap_error ldap_explode_dn ldap_first_attribute ldap_first_entry ldap_first_reference ldap_free_result ldap_get_attributes ldap_get_dn ldap_get_entries ldap_get_option ldap_get_values_len ldap_get_values ldap_list ldap_mod_add ldap_mod_del ldap_mod_replace ldap_modify ldap_next_attribute ldap_next_entry ldap_next_reference ldap_parse_reference ldap_parse_result ldap_read ldap_rename ldap_search ldap_set_option ldap_set_rebind_proc ldap_sort ldap_start_tls ldap_t61_to_8859 ldap_unbind	contained
  syn keyword	phpFunctions	lzf_compress lzf_decompress lzf_optimized_for	contained
  syn keyword	phpFunctions	ezmlm_hash mail	contained
  syn keyword	phpFunctions	mailparse_determine_best_xfer_encoding mailparse_msg_create mailparse_msg_extract_part_file mailparse_msg_extract_part mailparse_msg_free mailparse_msg_get_part_data mailparse_msg_get_part mailparse_msg_get_structure mailparse_msg_parse_file mailparse_msg_parse mailparse_rfc822_parse_addresses mailparse_stream_encode mailparse_uudecode_all	contained
  syn keyword	phpFunctions	abs acos acosh asin asinh atan2 atan atanh base_convert bindec ceil cos cosh decbin dechex decoct deg2rad exp expm1 floor fmod getrandmax hexdec hypot is_finite is_infinite is_nan lcg_value log10 log1p log max min mt_getrandmax mt_rand mt_srand octdec pi pow rad2deg rand round sin sinh sqrt srand tan tanh	contained
  syn keyword	phpFunctions	mb_convert_case mb_convert_encoding mb_convert_kana mb_convert_variables mb_decode_mimeheader mb_decode_numericentity mb_detect_encoding mb_detect_order mb_encode_mimeheader mb_encode_numericentity mb_ereg_match mb_ereg_replace mb_ereg_search_getpos mb_ereg_search_getregs mb_ereg_search_init mb_ereg_search_pos mb_ereg_search_regs mb_ereg_search_setpos mb_ereg_search mb_ereg mb_eregi_replace mb_eregi mb_get_info mb_http_input mb_http_output mb_internal_encoding mb_language mb_output_handler mb_parse_str mb_preferred_mime_name mb_regex_encoding mb_regex_set_options mb_send_mail mb_split mb_strcut mb_strimwidth mb_strlen mb_strpos mb_strrpos mb_strtolower mb_strtoupper mb_strwidth mb_substitute_character mb_substr_count mb_substr	contained
  syn keyword	phpFunctions	mcal_append_event mcal_close mcal_create_calendar mcal_date_compare mcal_date_valid mcal_day_of_week mcal_day_of_year mcal_days_in_month mcal_delete_calendar mcal_delete_event mcal_event_add_attribute mcal_event_init mcal_event_set_alarm mcal_event_set_category mcal_event_set_class mcal_event_set_description mcal_event_set_end mcal_event_set_recur_daily mcal_event_set_recur_monthly_mday mcal_event_set_recur_monthly_wday mcal_event_set_recur_none mcal_event_set_recur_weekly mcal_event_set_recur_yearly mcal_event_set_start mcal_event_set_title mcal_expunge mcal_fetch_current_stream_event mcal_fetch_event mcal_is_leap_year mcal_list_alarms mcal_list_events mcal_next_recurrence mcal_open mcal_popen mcal_rename_calendar mcal_reopen mcal_snooze mcal_store_event mcal_time_valid mcal_week_of_year	contained
  syn keyword	phpFunctions	mcrypt_cbc mcrypt_cfb mcrypt_create_iv mcrypt_decrypt mcrypt_ecb mcrypt_enc_get_algorithms_name mcrypt_enc_get_block_size mcrypt_enc_get_iv_size mcrypt_enc_get_key_size mcrypt_enc_get_modes_name mcrypt_enc_get_supported_key_sizes mcrypt_enc_is_block_algorithm_mode mcrypt_enc_is_block_algorithm mcrypt_enc_is_block_mode mcrypt_enc_self_test mcrypt_encrypt mcrypt_generic_deinit mcrypt_generic_end mcrypt_generic_init mcrypt_generic mcrypt_get_block_size mcrypt_get_cipher_name mcrypt_get_iv_size mcrypt_get_key_size mcrypt_list_algorithms mcrypt_list_modes mcrypt_module_close mcrypt_module_get_algo_block_size mcrypt_module_get_algo_key_size mcrypt_module_get_supported_key_sizes mcrypt_module_is_block_algorithm_mode mcrypt_module_is_block_algorithm mcrypt_module_is_block_mode mcrypt_module_open mcrypt_module_self_test mcrypt_ofb mdecrypt_generic	contained
  syn keyword	phpFunctions	mcve_adduser mcve_adduserarg mcve_bt mcve_checkstatus mcve_chkpwd mcve_chngpwd mcve_completeauthorizations mcve_connect mcve_connectionerror mcve_deleteresponse mcve_deletetrans mcve_deleteusersetup mcve_deluser mcve_destroyconn mcve_destroyengine mcve_disableuser mcve_edituser mcve_enableuser mcve_force mcve_getcell mcve_getcellbynum mcve_getcommadelimited mcve_getheader mcve_getuserarg mcve_getuserparam mcve_gft mcve_gl mcve_gut mcve_initconn mcve_initengine mcve_initusersetup mcve_iscommadelimited mcve_liststats mcve_listusers mcve_maxconntimeout mcve_monitor mcve_numcolumns mcve_numrows mcve_override mcve_parsecommadelimited mcve_ping mcve_preauth mcve_preauthcompletion mcve_qc mcve_responseparam mcve_return mcve_returncode mcve_returnstatus mcve_sale mcve_setblocking mcve_setdropfile mcve_setip mcve_setssl_files mcve_setssl mcve_settimeout mcve_settle mcve_text_avs mcve_text_code mcve_text_cv mcve_transactionauth mcve_transactionavs mcve_transactionbatch mcve_transactioncv mcve_transactionid mcve_transactionitem mcve_transactionssent mcve_transactiontext mcve_transinqueue mcve_transnew mcve_transparam mcve_transsend mcve_ub mcve_uwait mcve_verifyconnection mcve_verifysslcert mcve_void	contained
  syn keyword	phpFunctions	mhash_count mhash_get_block_size mhash_get_hash_name mhash_keygen_s2k mhash	contained
  syn keyword	phpFunctions	mime_content_type	contained
  syn keyword	phpFunctions	ming_setcubicthreshold ming_setscale ming_useswfversion SWFAction SWFBitmap swfbutton_keypress SWFbutton SWFDisplayItem SWFFill SWFFont SWFGradient SWFMorph SWFMovie SWFShape SWFSprite SWFText SWFTextField	contained
  syn keyword	phpMethods	getHeight getWidth addAction addShape setAction setdown setHit setOver setUp addColor move moveTo multColor remove Rotate rotateTo scale scaleTo setDepth setName setRatio skewX skewXTo skewY skewYTo moveTo rotateTo scaleTo skewXTo skewYTo getwidth addEntry getshape1 getshape2 add nextframe output remove save setbackground setdimension setframes setrate streammp3 addFill drawCurve drawCurveTo drawLine drawLineTo movePen movePenTo setLeftFill setLine setRightFill add nextframe remove setframes addString getWidth moveTo setColor setFont setHeight setSpacing addstring align setbounds setcolor setFont setHeight setindentation setLeftMargin setLineSpacing setMargins setname setrightMargin	contained
  syn keyword	phpFunctions	connection_aborted connection_status connection_timeout constant define defined eval get_browser highlight_file highlight_string ignore_user_abort pack show_source sleep uniqid unpack usleep	contained
  syn keyword	phpFunctions	udm_add_search_limit udm_alloc_agent udm_api_version udm_cat_list udm_cat_path udm_check_charset udm_check_stored udm_clear_search_limits udm_close_stored udm_crc32 udm_errno udm_error udm_find udm_free_agent udm_free_ispell_data udm_free_res udm_get_doc_count udm_get_res_field udm_get_res_param udm_load_ispell_data udm_open_stored udm_set_agent_param	contained
  syn keyword	phpFunctions	msession_connect msession_count msession_create msession_destroy msession_disconnect msession_find msession_get_array msession_get msession_getdata msession_inc msession_list msession_listvar msession_lock msession_plugin msession_randstr msession_set_array msession_set msession_setdata msession_timeout msession_uniq msession_unlock	contained
  syn keyword	phpFunctions	msql_affected_rows msql_close msql_connect msql_create_db msql_createdb msql_data_seek msql_dbname msql_drop_db msql_dropdb msql_error msql_fetch_array msql_fetch_field msql_fetch_object msql_fetch_row msql_field_seek msql_fieldflags msql_fieldlen msql_fieldname msql_fieldtable msql_fieldtype msql_free_result msql_freeresult msql_list_dbs msql_list_fields msql_list_tables msql_listdbs msql_listfields msql_listtables msql_num_fields msql_num_rows msql_numfields msql_numrows msql_pconnect msql_query msql_regcase msql_result msql_select_db msql_selectdb msql_tablename msql	contained
  syn keyword	phpFunctions	mssql_bind mssql_close mssql_connect mssql_data_seek mssql_execute mssql_fetch_array mssql_fetch_assoc mssql_fetch_batch mssql_fetch_field mssql_fetch_object mssql_fetch_row mssql_field_length mssql_field_name mssql_field_seek mssql_field_type mssql_free_result mssql_free_statement mssql_get_last_message mssql_guid_string mssql_init mssql_min_error_severity mssql_min_message_severity mssql_next_result mssql_num_fields mssql_num_rows mssql_pconnect mssql_query mssql_result mssql_rows_affected mssql_select_db	contained
  syn keyword	phpFunctions	muscat_close muscat_get muscat_give muscat_setup_net muscat_setup	contained
  syn keyword	phpFunctions	mysql_affected_rows mysql_change_user mysql_client_encoding mysql_close mysql_connect mysql_create_db mysql_data_seek mysql_db_name mysql_db_query mysql_drop_db mysql_errno mysql_error mysql_escape_string mysql_fetch_array mysql_fetch_assoc mysql_fetch_field mysql_fetch_lengths mysql_fetch_object mysql_fetch_row mysql_field_flags mysql_field_len mysql_field_name mysql_field_seek mysql_field_table mysql_field_type mysql_free_result mysql_get_client_info mysql_get_host_info mysql_get_proto_info mysql_get_server_info mysql_info mysql_insert_id mysql_list_dbs mysql_list_fields mysql_list_processes mysql_list_tables mysql_num_fields mysql_num_rows mysql_pconnect mysql_ping mysql_query mysql_real_escape_string mysql_result mysql_select_db mysql_stat mysql_tablename mysql_thread_id mysql_unbuffered_query	contained
  syn keyword	phpFunctions	mysqli_affected_rows mysqli_autocommit mysqli_bind_param mysqli_bind_result mysqli_change_user mysqli_character_set_name mysqli_close mysqli_commit mysqli_connect mysqli_data_seek mysqli_debug mysqli_disable_reads_from_master mysqli_disable_rpl_parse mysqli_dump_debug_info mysqli_enable_reads_from_master mysqli_enable_rpl_parse mysqli_errno mysqli_error mysqli_execute mysqli_fetch_array mysqli_fetch_assoc mysqli_fetch_field_direct mysqli_fetch_field mysqli_fetch_fields mysqli_fetch_lengths mysqli_fetch_object mysqli_fetch_row mysqli_fetch mysqli_field_count mysqli_field_seek mysqli_field_tell mysqli_free_result mysqli_get_client_info mysqli_get_host_info mysqli_get_proto_info mysqli_get_server_info mysqli_get_server_version mysqli_info mysqli_init mysqli_insert_id mysqli_kill mysqli_master_query mysqli_num_fields mysqli_num_rows mysqli_options mysqli_param_count mysqli_ping mysqli_prepare_result mysqli_prepare mysqli_profiler mysqli_query mysqli_read_query_result mysqli_real_connect mysqli_real_escape_string mysqli_real_query mysqli_reload mysqli_rollback mysqli_rpl_parse_enabled mysqli_rpl_probe mysqli_rpl_query_type mysqli_select_db mysqli_send_long_data mysqli_send_query mysqli_slave_query mysqli_ssl_set mysqli_stat mysqli_stmt_affected_rows mysqli_stmt_close mysqli_stmt_errno mysqli_stmt_error mysqli_stmt_store_result mysqli_store_result mysqli_thread_id mysqli_thread_safe mysqli_use_result mysqli_warning_count	contained
  syn keyword	phpFunctions	ncurses_addch ncurses_addchnstr ncurses_addchstr ncurses_addnstr ncurses_addstr ncurses_assume_default_colors ncurses_attroff ncurses_attron ncurses_attrset ncurses_baudrate ncurses_beep ncurses_bkgd ncurses_bkgdset ncurses_border ncurses_bottom_panel ncurses_can_change_color ncurses_cbreak ncurses_clear ncurses_clrtobot ncurses_clrtoeol ncurses_color_content ncurses_color_set ncurses_curs_set ncurses_def_prog_mode ncurses_def_shell_mode ncurses_define_key ncurses_del_panel ncurses_delay_output ncurses_delch ncurses_deleteln ncurses_delwin ncurses_doupdate ncurses_echo ncurses_echochar ncurses_end ncurses_erase ncurses_erasechar ncurses_filter ncurses_flash ncurses_flushinp ncurses_getch ncurses_getmaxyx ncurses_getmouse ncurses_getyx ncurses_halfdelay ncurses_has_colors ncurses_has_ic ncurses_has_il ncurses_has_key ncurses_hide_panel ncurses_hline ncurses_inch ncurses_init_color ncurses_init_pair ncurses_init ncurses_insch ncurses_insdelln ncurses_insertln ncurses_insstr ncurses_instr ncurses_isendwin ncurses_keyok ncurses_keypad ncurses_killchar ncurses_longname ncurses_meta ncurses_mouse_trafo ncurses_mouseinterval ncurses_mousemask ncurses_move_panel ncurses_move ncurses_mvaddch ncurses_mvaddchnstr ncurses_mvaddchstr ncurses_mvaddnstr ncurses_mvaddstr ncurses_mvcur ncurses_mvdelch ncurses_mvgetch ncurses_mvhline ncurses_mvinch ncurses_mvvline ncurses_mvwaddstr ncurses_napms ncurses_new_panel ncurses_newpad ncurses_newwin ncurses_nl ncurses_nocbreak ncurses_noecho ncurses_nonl ncurses_noqiflush ncurses_noraw ncurses_pair_content ncurses_panel_above ncurses_panel_below ncurses_panel_window ncurses_pnoutrefresh ncurses_prefresh ncurses_putp ncurses_qiflush ncurses_raw ncurses_refresh ncurses_replace_panel ncurses_reset_prog_mode ncurses_reset_shell_mode ncurses_resetty ncurses_savetty ncurses_scr_dump ncurses_scr_init ncurses_scr_restore ncurses_scr_set ncurses_scrl ncurses_show_panel ncurses_slk_attr ncurses_slk_attroff ncurses_slk_attron ncurses_slk_attrset ncurses_slk_clear ncurses_slk_color ncurses_slk_init ncurses_slk_noutrefresh ncurses_slk_refresh ncurses_slk_restore ncurses_slk_set ncurses_slk_touch ncurses_standend ncurses_standout ncurses_start_color ncurses_termattrs ncurses_termname ncurses_timeout ncurses_top_panel ncurses_typeahead ncurses_ungetch ncurses_ungetmouse ncurses_update_panels ncurses_use_default_colors ncurses_use_env ncurses_use_extended_names ncurses_vidattr ncurses_vline ncurses_waddch ncurses_waddstr ncurses_wattroff ncurses_wattron ncurses_wattrset ncurses_wborder ncurses_wclear ncurses_wcolor_set ncurses_werase ncurses_wgetch ncurses_whline ncurses_wmouse_trafo ncurses_wmove ncurses_wnoutrefresh ncurses_wrefresh ncurses_wstandend ncurses_wstandout ncurses_wvline	contained
  syn keyword	phpFunctions	checkdnsrr closelog debugger_off debugger_on define_syslog_variables dns_check_record dns_get_mx dns_get_record fsockopen gethostbyaddr gethostbyname gethostbynamel getmxrr getprotobyname getprotobynumber getservbyname getservbyport ip2long long2ip openlog pfsockopen socket_get_status socket_set_blocking socket_set_timeout syslog	contained
  syn keyword	phpFunctions	yp_all yp_cat yp_err_string yp_errno yp_first yp_get_default_domain yp_master yp_match yp_next yp_order	contained
  syn keyword	phpFunctions	notes_body notes_copy_db notes_create_db notes_create_note notes_drop_db notes_find_note notes_header_info notes_list_msgs notes_mark_read notes_mark_unread notes_nav_create notes_search notes_unread notes_version	contained
  syn keyword	phpFunctions	nsapi_request_headers nsapi_response_headers nsapi_virtual	contained
  syn keyword	phpFunctions	aggregate_info aggregate_methods_by_list aggregate_methods_by_regexp aggregate_methods aggregate_properties_by_list aggregate_properties_by_regexp aggregate_properties aggregate aggregation_info deaggregate	contained
  syn keyword	phpFunctions	ocibindbyname ocicancel ocicloselob ocicollappend ocicollassign ocicollassignelem ocicollgetelem ocicollmax ocicollsize ocicolltrim ocicolumnisnull ocicolumnname ocicolumnprecision ocicolumnscale ocicolumnsize ocicolumntype ocicolumntyperaw ocicommit ocidefinebyname ocierror ociexecute ocifetch ocifetchinto ocifetchstatement ocifreecollection ocifreecursor ocifreedesc ocifreestatement ociinternaldebug ociloadlob ocilogoff ocilogon ocinewcollection ocinewcursor ocinewdescriptor ocinlogon ocinumcols ociparse ociplogon ociresult ocirollback ocirowcount ocisavelob ocisavelobfile ociserverversion ocisetprefetch ocistatementtype ociwritelobtofile ociwritetemporarylob	contained
  syn keyword	phpFunctions	odbc_autocommit odbc_binmode odbc_close_all odbc_close odbc_columnprivileges odbc_columns odbc_commit odbc_connect odbc_cursor odbc_data_source odbc_do odbc_error odbc_errormsg odbc_exec odbc_execute odbc_fetch_array odbc_fetch_into odbc_fetch_object odbc_fetch_row odbc_field_len odbc_field_name odbc_field_num odbc_field_precision odbc_field_scale odbc_field_type odbc_foreignkeys odbc_free_result odbc_gettypeinfo odbc_longreadlen odbc_next_result odbc_num_fields odbc_num_rows odbc_pconnect odbc_prepare odbc_primarykeys odbc_procedurecolumns odbc_procedures odbc_result_all odbc_result odbc_rollback odbc_setoption odbc_specialcolumns odbc_statistics odbc_tableprivileges odbc_tables	contained
  syn keyword	phpFunctions	openssl_csr_export_to_file openssl_csr_export openssl_csr_new openssl_csr_sign openssl_error_string openssl_free_key openssl_get_privatekey openssl_get_publickey openssl_open openssl_pkcs7_decrypt openssl_pkcs7_encrypt openssl_pkcs7_sign openssl_pkcs7_verify openssl_pkey_export_to_file openssl_pkey_export openssl_pkey_get_private openssl_pkey_get_public openssl_pkey_new openssl_private_decrypt openssl_private_encrypt openssl_public_decrypt openssl_public_encrypt openssl_seal openssl_sign openssl_verify openssl_x509_check_private_key openssl_x509_checkpurpose openssl_x509_export_to_file openssl_x509_export openssl_x509_free openssl_x509_parse openssl_x509_read	contained
  syn keyword	phpFunctions	ora_bind ora_close ora_columnname ora_columnsize ora_columntype ora_commit ora_commitoff ora_commiton ora_do ora_error ora_errorcode ora_exec ora_fetch_into ora_fetch ora_getcolumn ora_logoff ora_logon ora_numcols ora_numrows ora_open ora_parse ora_plogon ora_rollback	contained
  syn keyword	phpFunctions	flush ob_clean ob_end_clean ob_end_flush ob_flush ob_get_clean ob_get_contents ob_get_flush ob_get_length ob_get_level ob_get_status ob_gzhandler ob_implicit_flush ob_list_handlers ob_start output_add_rewrite_var output_reset_rewrite_vars	contained
  syn keyword	phpFunctions	overload	contained
  syn keyword	phpFunctions	ovrimos_close ovrimos_commit ovrimos_connect ovrimos_cursor ovrimos_exec ovrimos_execute ovrimos_fetch_into ovrimos_fetch_row ovrimos_field_len ovrimos_field_name ovrimos_field_num ovrimos_field_type ovrimos_free_result ovrimos_longreadlen ovrimos_num_fields ovrimos_num_rows ovrimos_prepare ovrimos_result_all ovrimos_result ovrimos_rollback	contained
  syn keyword	phpFunctions	pcntl_exec pcntl_fork pcntl_signal pcntl_waitpid pcntl_wexitstatus pcntl_wifexited pcntl_wifsignaled pcntl_wifstopped pcntl_wstopsig pcntl_wtermsig	contained
  syn keyword	phpFunctions	preg_grep preg_match_all preg_match preg_quote preg_replace_callback preg_replace preg_split	contained
  syn keyword	phpFunctions	pdf_add_annotation pdf_add_bookmark pdf_add_launchlink pdf_add_locallink pdf_add_note pdf_add_outline pdf_add_pdflink pdf_add_thumbnail pdf_add_weblink pdf_arc pdf_arcn pdf_attach_file pdf_begin_page pdf_begin_pattern pdf_begin_template pdf_circle pdf_clip pdf_close_image pdf_close_pdi_page pdf_close_pdi pdf_close pdf_closepath_fill_stroke pdf_closepath_stroke pdf_closepath pdf_concat pdf_continue_text pdf_curveto pdf_delete pdf_end_page pdf_end_pattern pdf_end_template pdf_endpath pdf_fill_stroke pdf_fill pdf_findfont pdf_get_buffer pdf_get_font pdf_get_fontname pdf_get_fontsize pdf_get_image_height pdf_get_image_width pdf_get_majorversion pdf_get_minorversion pdf_get_parameter pdf_get_pdi_parameter pdf_get_pdi_value pdf_get_value pdf_initgraphics pdf_lineto pdf_makespotcolor pdf_moveto pdf_new pdf_open_CCITT pdf_open_file pdf_open_gif pdf_open_image_file pdf_open_image pdf_open_jpeg pdf_open_memory_image pdf_open_pdi_page pdf_open_pdi pdf_open_png pdf_open_tiff pdf_open pdf_place_image pdf_place_pdi_page pdf_rect pdf_restore pdf_rotate pdf_save pdf_scale pdf_set_border_color pdf_set_border_dash pdf_set_border_style pdf_set_char_spacing pdf_set_duration pdf_set_font pdf_set_horiz_scaling pdf_set_info_author pdf_set_info_creator pdf_set_info_keywords pdf_set_info_subject pdf_set_info_title pdf_set_info pdf_set_leading pdf_set_parameter pdf_set_text_matrix pdf_set_text_pos pdf_set_text_rendering pdf_set_text_rise pdf_set_value pdf_set_word_spacing pdf_setcolor pdf_setdash pdf_setflat pdf_setfont pdf_setgray_fill pdf_setgray_stroke pdf_setgray pdf_setlinecap pdf_setlinejoin pdf_setlinewidth pdf_setmatrix pdf_setmiterlimit pdf_setpolydash pdf_setrgbcolor_fill pdf_setrgbcolor_stroke pdf_setrgbcolor pdf_show_boxed pdf_show_xy pdf_show pdf_skew pdf_stringwidth pdf_stroke pdf_translate	contained
  syn keyword	phpFunctions	pfpro_cleanup pfpro_init pfpro_process_raw pfpro_process pfpro_version	contained
  syn keyword	phpFunctions	pg_affected_rows pg_cancel_query pg_client_encoding pg_close pg_connect pg_connection_busy pg_connection_reset pg_connection_status pg_convert pg_copy_from pg_copy_to pg_dbname pg_delete pg_end_copy pg_escape_bytea pg_escape_string pg_fetch_all pg_fetch_array pg_fetch_assoc pg_fetch_object pg_fetch_result pg_fetch_row pg_field_is_null pg_field_name pg_field_num pg_field_prtlen pg_field_size pg_field_type pg_free_result pg_get_notify pg_get_pid pg_get_result pg_host pg_insert pg_last_error pg_last_notice pg_last_oid pg_lo_close pg_lo_create pg_lo_export pg_lo_import pg_lo_open pg_lo_read_all pg_lo_read pg_lo_seek pg_lo_tell pg_lo_unlink pg_lo_write pg_meta_data pg_num_fields pg_num_rows pg_options pg_pconnect pg_ping pg_port pg_put_line pg_query pg_result_error pg_result_seek pg_result_status pg_select pg_send_query pg_set_client_encoding pg_trace pg_tty pg_unescape_bytea pg_untrace pg_update	contained
  syn keyword	phpFunctions	posix_ctermid posix_get_last_error posix_getcwd posix_getegid posix_geteuid posix_getgid posix_getgrgid posix_getgrnam posix_getgroups posix_getlogin posix_getpgid posix_getpgrp posix_getpid posix_getppid posix_getpwnam posix_getpwuid posix_getrlimit posix_getsid posix_getuid posix_isatty posix_kill posix_mkfifo posix_setegid posix_seteuid posix_setgid posix_setpgid posix_setsid posix_setuid posix_strerror posix_times posix_ttyname posix_uname	contained
  syn keyword	phpFunctions	printer_abort printer_close printer_create_brush printer_create_dc printer_create_font printer_create_pen printer_delete_brush printer_delete_dc printer_delete_font printer_delete_pen printer_draw_bmp printer_draw_chord printer_draw_elipse printer_draw_line printer_draw_pie printer_draw_rectangle printer_draw_roundrect printer_draw_text printer_end_doc printer_end_page printer_get_option printer_list printer_logical_fontheight printer_open printer_select_brush printer_select_font printer_select_pen printer_set_option printer_start_doc printer_start_page printer_write	contained
  syn keyword	phpFunctions	pspell_add_to_personal pspell_add_to_session pspell_check pspell_clear_session pspell_config_create pspell_config_ignore pspell_config_mode pspell_config_personal pspell_config_repl pspell_config_runtogether pspell_config_save_repl pspell_new_config pspell_new_personal pspell_new pspell_save_wordlist pspell_store_replacement pspell_suggest	contained
  syn keyword	phpFunctions	qdom_error qdom_tree	contained
  syn keyword	phpFunctions	readline_add_history readline_clear_history readline_completion_function readline_info readline_list_history readline_read_history readline_write_history readline	contained
  syn keyword	phpFunctions	recode_file recode_string recode	contained
  syn keyword	phpFunctions	ereg_replace ereg eregi_replace eregi split spliti sql_regcase	contained
  syn keyword	phpFunctions	ftok msg_get_queue msg_receive msg_remove_queue msg_send msg_set_queue msg_stat_queue sem_acquire sem_get sem_release sem_remove shm_attach shm_detach shm_get_var shm_put_var shm_remove_var shm_remove	contained
  syn keyword	phpFunctions	sesam_affected_rows sesam_commit sesam_connect sesam_diagnostic sesam_disconnect sesam_errormsg sesam_execimm sesam_fetch_array sesam_fetch_result sesam_fetch_row sesam_field_array sesam_field_name sesam_free_result sesam_num_fields sesam_query sesam_rollback sesam_seek_row sesam_settransaction	contained
  syn keyword	phpFunctions	session_cache_expire session_cache_limiter session_decode session_destroy session_encode session_get_cookie_params session_id session_is_registered session_module_name session_name session_regenerate_id session_register session_save_path session_set_cookie_params session_set_save_handler session_start session_unregister session_unset session_write_close	contained
  syn keyword	phpFunctions	shmop_close shmop_delete shmop_open shmop_read shmop_size shmop_write	contained
  syn keyword	phpFunctions	snmp_get_quick_print snmp_set_quick_print snmpget snmprealwalk snmpset snmpwalk snmpwalkoid	contained
  syn keyword	phpFunctions	socket_accept socket_bind socket_clear_error socket_close socket_connect socket_create_listen socket_create_pair socket_create socket_get_option socket_getpeername socket_getsockname socket_iovec_add socket_iovec_alloc socket_iovec_delete socket_iovec_fetch socket_iovec_free socket_iovec_set socket_last_error socket_listen socket_read socket_readv socket_recv socket_recvfrom socket_recvmsg socket_select socket_send socket_sendmsg socket_sendto socket_set_block socket_set_nonblock socket_set_option socket_shutdown socket_strerror socket_write socket_writev	contained
  syn keyword	phpFunctions	sqlite_array_query sqlite_busy_timeout sqlite_changes sqlite_close sqlite_column sqlite_create_aggregate sqlite_create_function sqlite_current sqlite_error_string sqlite_escape_string sqlite_fetch_array sqlite_fetch_single sqlite_fetch_string sqlite_field_name sqlite_has_more sqlite_last_error sqlite_last_insert_rowid sqlite_libencoding sqlite_libversion sqlite_next sqlite_num_fields sqlite_num_rows sqlite_open sqlite_popen sqlite_query sqlite_rewind sqlite_seek sqlite_udf_decode_binary sqlite_udf_encode_binary sqlite_unbuffered_query	contained
  syn keyword	phpFunctions	stream_context_create stream_context_get_options stream_context_set_option stream_context_set_params stream_copy_to_stream stream_filter_append stream_filter_prepend stream_filter_register stream_get_contents stream_get_filters stream_get_line stream_get_meta_data stream_get_transports stream_get_wrappers stream_register_wrapper stream_select stream_set_blocking stream_set_timeout stream_set_write_buffer stream_socket_accept stream_socket_client stream_socket_get_name stream_socket_recvfrom stream_socket_sendto stream_socket_server stream_wrapper_register	contained
  syn keyword	phpFunctions	addcslashes addslashes bin2hex chop chr chunk_split convert_cyr_string count_chars crc32 crypt explode fprintf get_html_translation_table hebrev hebrevc html_entity_decode htmlentities htmlspecialchars implode join levenshtein localeconv ltrim md5_file md5 metaphone money_format nl_langinfo nl2br number_format ord parse_str print printf quoted_printable_decode quotemeta rtrim setlocale sha1_file sha1 similar_text soundex sprintf sscanf str_ireplace str_pad str_repeat str_replace str_rot13 str_shuffle str_split str_word_count strcasecmp strchr strcmp strcoll strcspn strip_tags stripcslashes stripos stripslashes stristr strlen strnatcasecmp strnatcmp strncasecmp strncmp strpos strrchr strrev strripos strrpos strspn strstr strtok strtolower strtoupper strtr substr_compare substr_count substr_replace substr trim ucfirst ucwords vprintf vsprintf wordwrap	contained
  syn keyword	phpFunctions	swf_actiongeturl swf_actiongotoframe swf_actiongotolabel swf_actionnextframe swf_actionplay swf_actionprevframe swf_actionsettarget swf_actionstop swf_actiontogglequality swf_actionwaitforframe swf_addbuttonrecord swf_addcolor swf_closefile swf_definebitmap swf_definefont swf_defineline swf_definepoly swf_definerect swf_definetext swf_endbutton swf_enddoaction swf_endshape swf_endsymbol swf_fontsize swf_fontslant swf_fonttracking swf_getbitmapinfo swf_getfontinfo swf_getframe swf_labelframe swf_lookat swf_modifyobject swf_mulcolor swf_nextid swf_oncondition swf_openfile swf_ortho2 swf_ortho swf_perspective swf_placeobject swf_polarview swf_popmatrix swf_posround swf_pushmatrix swf_removeobject swf_rotate swf_scale swf_setfont swf_setframe swf_shapearc swf_shapecurveto3 swf_shapecurveto swf_shapefillbitmapclip swf_shapefillbitmaptile swf_shapefilloff swf_shapefillsolid swf_shapelinesolid swf_shapelineto swf_shapemoveto swf_showframe swf_startbutton swf_startdoaction swf_startshape swf_startsymbol swf_textwidth swf_translate swf_viewport	contained
  syn keyword	phpFunctions	sybase_affected_rows sybase_close sybase_connect sybase_data_seek sybase_deadlock_retry_count sybase_fetch_array sybase_fetch_assoc sybase_fetch_field sybase_fetch_object sybase_fetch_row sybase_field_seek sybase_free_result sybase_get_last_message sybase_min_client_severity sybase_min_error_severity sybase_min_message_severity sybase_min_server_severity sybase_num_fields sybase_num_rows sybase_pconnect sybase_query sybase_result sybase_select_db sybase_set_message_handler sybase_unbuffered_query	contained
  syn keyword	phpFunctions	tidy_access_count tidy_clean_repair tidy_config_count tidy_diagnose tidy_error_count tidy_get_body tidy_get_config tidy_get_error_buffer tidy_get_head tidy_get_html_ver tidy_get_html tidy_get_output tidy_get_release tidy_get_root tidy_get_status tidy_getopt tidy_is_xhtml tidy_load_config tidy_parse_file tidy_parse_string tidy_repair_file tidy_repair_string tidy_reset_config tidy_save_config tidy_set_encoding tidy_setopt tidy_warning_count	contained
  syn keyword	phpMethods	attributes children get_attr get_nodes has_children has_siblings is_asp is_comment is_html is_jsp is_jste is_text is_xhtml is_xml next prev tidy_node	contained
  syn keyword	phpFunctions	token_get_all token_name	contained
  syn keyword	phpFunctions	base64_decode base64_encode get_meta_tags http_build_query parse_url rawurldecode rawurlencode urldecode urlencode	contained
  syn keyword	phpFunctions	doubleval empty floatval get_defined_vars get_resource_type gettype import_request_variables intval is_array is_bool is_callable is_double is_float is_int is_integer is_long is_null is_numeric is_object is_real is_resource is_scalar is_string isset print_r serialize settype strval unserialize unset var_dump var_export	contained
  syn keyword	phpFunctions	vpopmail_add_alias_domain_ex vpopmail_add_alias_domain vpopmail_add_domain_ex vpopmail_add_domain vpopmail_add_user vpopmail_alias_add vpopmail_alias_del_domain vpopmail_alias_del vpopmail_alias_get_all vpopmail_alias_get vpopmail_auth_user vpopmail_del_domain_ex vpopmail_del_domain vpopmail_del_user vpopmail_error vpopmail_passwd vpopmail_set_user_quota	contained
  syn keyword	phpFunctions	w32api_deftype w32api_init_dtype w32api_invoke_function w32api_register_function w32api_set_call_method	contained
  syn keyword	phpFunctions	wddx_add_vars wddx_deserialize wddx_packet_end wddx_packet_start wddx_serialize_value wddx_serialize_vars	contained
  syn keyword	phpFunctions	utf8_decode utf8_encode xml_error_string xml_get_current_byte_index xml_get_current_column_number xml_get_current_line_number xml_get_error_code xml_parse_into_struct xml_parse xml_parser_create_ns xml_parser_create xml_parser_free xml_parser_get_option xml_parser_set_option xml_set_character_data_handler xml_set_default_handler xml_set_element_handler xml_set_end_namespace_decl_handler xml_set_external_entity_ref_handler xml_set_notation_decl_handler xml_set_object xml_set_processing_instruction_handler xml_set_start_namespace_decl_handler xml_set_unparsed_entity_decl_handler	contained
  syn keyword	phpFunctions	xmlrpc_decode_request xmlrpc_decode xmlrpc_encode_request xmlrpc_encode xmlrpc_get_type xmlrpc_parse_method_descriptions xmlrpc_server_add_introspection_data xmlrpc_server_call_method xmlrpc_server_create xmlrpc_server_destroy xmlrpc_server_register_introspection_callback xmlrpc_server_register_method xmlrpc_set_type	contained
  syn keyword	phpFunctions	xslt_create xslt_errno xslt_error xslt_free xslt_output_process xslt_set_base xslt_set_encoding xslt_set_error_handler xslt_set_log xslt_set_sax_handler xslt_set_sax_handlers xslt_set_scheme_handler xslt_set_scheme_handlers	contained
  syn keyword	phpFunctions	yaz_addinfo yaz_ccl_conf yaz_ccl_parse yaz_close yaz_connect yaz_database yaz_element yaz_errno yaz_error yaz_es_result yaz_get_option yaz_hits yaz_itemorder yaz_present yaz_range yaz_record yaz_scan_result yaz_scan yaz_schema yaz_search yaz_set_option yaz_sort yaz_syntax yaz_wait	contained
  syn keyword	phpFunctions	zip_close zip_entry_close zip_entry_compressedsize zip_entry_compressionmethod zip_entry_filesize zip_entry_name zip_entry_open zip_entry_read zip_open zip_read	contained
  syn keyword	phpFunctions	gzclose gzcompress gzdeflate gzencode gzeof gzfile gzgetc gzgets gzgetss gzinflate gzopen gzpassthru gzputs gzread gzrewind gzseek gztell gzuncompress gzwrite readgzfile zlib_get_coding_type	contained
  syn keyword	phpFunctions	timezone_offset_get date_create
  syn keyword phpFunctions	date_default_timezone_set date_default_timezone_get
  " ext SPL:
  syn keyword phpFunctions	spl_autoload_call spl_autoload_extensions spl_autoload_functions
  syn keyword phpFunctions	spl_autoload_register spl_autoload_unregister spl_autoload
  syn keyword phpFunctions	spl_classes spl_object_hash
  syn keyword phpFunctions	class_implements class_parents iterator_count iterator_to_array
  
  " }}}2

  if s:show_baselib
    syn keyword	phpMethods	query next_record num_rows affected_rows nf f p np num_fields haltmsg seek link_id query_id metadata table_names nextid connect halt free register unregister is_registered delete url purl self_url pself_url hidden_session add_query padd_query reimport_get_vars reimport_post_vars reimport_cookie_vars set_container set_tokenname release_token put_headers get_id get_id put_id freeze thaw gc reimport_any_vars start url purl login_if is_authenticated auth_preauth auth_loginform auth_validatelogin auth_refreshlogin auth_registerform auth_doregister start check have_perm permsum perm_invalid	contained
    syn keyword	phpFunctions	page_open page_close sess_load sess_save	contained
  endif


" }}}1

" {{{1 REVIEW: }}}1


" Keyword
syn cluster phpClInClass add=phpClassDefine
syn keyword	phpClassDefine contained var const
hi link phpClassDefine phpDefine

if s:alt_arrays
  syn cluster phpClExpressions add=phpArrayRegion
  syn cluster phpClValues add=phpArrayRegionSimple
  " TODO: should the error highlighting be optional???
  if s:fold_arrays
    syn region phpArrayRegionSimple contained matchgroup=phpArrayParens start=/\<array\_s*(/ end=/)/
          \ keepend extend contains=@phpClValues,phpArrayPair,phpArrayComma
          \ matchgroup=Error end=/;/
          \ fold
    syn region phpArrayRegion contained matchgroup=phpArrayParens start=/\<array\_s*(/ end=/)/
          \ keepend extend contains=@phpClExpressions,phpArrayPair,phpArrayComma
          \ matchgroup=Error end=/;/
          \ fold
  else
    syn region phpArrayRegionSimple contained matchgroup=phpArrayParens start=/\<array\_s*(/ end=/)/
          \ keepend extend contains=@phpClValues,phpArrayPair,phpArrayComma
          \ matchgroup=Error end=/;/
    syn region phpArrayRegion contained matchgroup=phpArrayParens start=/\<array\_s*(/ end=/)/
          \ keepend extend contains=@phpClExpressions,phpArrayPair,phpArrayComma
          \ matchgroup=Error end=/;/
  endif
  syn match phpArrayComma contained display /,/

  syn cluster phpClExpressions add=phpListRegion
  " need to make a region for the 'list' keyword as well!!!
  " TODO: should the error highlighting be optional???
  " TODO: only allow variables and stuff inside the list() construct
  syn region phpListRegion contained matchgroup=phpList start=/\<list(/ end=/)/
        \ keepend extend contains=@phpClExpressions,phpListComma
        \ matchgroup=Error end=/;/
  syn match phpListComma contained display /,/
else
  " need to use a match instead of keyword here ... to stop it
  " from blocking a match later on.
  syn cluster phpClExpressions add=phpArray
  syn match phpArray contained display /\<array\>/

  syn keyword phpList contained list
endif

" Operators


" Peter Hodge - added support for array-building operator
" to stop the relations from mixing this up
if s:alt_arrays
  " highlight misuse of the '=>' operator
  syn cluster phpClExpressions add=phpArrayPairError
  syn match	phpArrayPairError /=>/ contained display

  " the next match is used only in the correct places
  syn match	phpArrayPair /=>/ contained display
else
  syn match phpOperator /=>/ contained display
endif

" relations
" Peter Hodge, June 17 2006
" - altered relations to match strict comparisons (=== and !==)
" - highlight the 'instanceof' operator as a relation operator
"   rather than a structure, if comparison support is a priority.
syn cluster phpClExpressions add=phpRelation
syn match phpRelation contained display /===\=/
syn match phpRelation contained display /!==\=/
syn match phpRelation contained display /<<\@!=\=/
syn match phpRelation contained display />>\@!=\=/

" 'instanceof' is also a relation, but may have alternate colours
syn cluster phpClExpressions add=phpInstanceof
syn keyword phpInstanceof contained instanceof
      \ nextgroup=@phpClStructures,phpStructureHere skipwhite skipempty

" how to treat '->'?

" Note: this is always needed for inside strings
syn match phpPropertySelector contained display /->/

if ! s:smart_members
  " NOTE: this match is for ANY '->' match, however the more specific
  " phpPropertySelector or phpDynamicSelector may match instead
  syn cluster phpClExpressions add=phpMemberSelector
  syn match phpMemberSelector contained display /->/
        \ nextgroup=@phpClProperties,@phpClMembers,phpMemberHere skipwhite skipempty

else
  " by default all -> matches are property access
  syn cluster phpClExpressions add=phpPropertySelector

  " make a match for a whole property name also
  syn cluster phpClExpressions add=phpPropertyAccess
  syn match phpPropertyAccess contained display /->\_s*\h\w*/
        \ contains=phpPropertySelector,@phpClProperties,phpPropertyHere

  " try match them as method calls first though
  " NOTE: have taken out phpMethods (because it just wasn't accurate)
  " but have added phpSpecialMethods because they are always special
  syn cluster phpClExpressions add=phpMethodCall
  syn cluster phpClMethodHere add=phpMethodCall
  syn match phpMethodCall contained display /->\_s*\h\w*\_s*(\@=/
        \ contained display contains=phpMemberSelector,@phpClMethods
  syn match phpMethodCall contained display /->\_s*\$\h\w*\_s*(\@=/
        \ contained display contains=phpIdentifier,phpMemberSelector
  syn match phpMemberSelector contained display /->/

  " for a dynamic {} property/method
  if s:alt_properties
    syn cluster phpClExpressions add=phpDynamicSelectorRegion
    syn region phpDynamicSelectorRegion contained keepend extend
          \ matchgroup=phpDynamicSelector start=/->\_s*{/ end=/}/
          \ contains=@phpClExpressions
  endif

"  " highlight incorrect use of -> as an error
"  syn cluster phpClExpressions add=phpMemberError
"  syn match phpMemberError /->\%#\@!\%(\_s*\)\@>[a-z{_$]\@!/ contained display

endif

syn region phpIdentifierComplex contained display matchgroup=phpVarSelector start=/\${/ end=/}/
      \ keepend extend
      \ contains=@phpClExpressions

" create an identifier match for double-quoted strings:

" Methoden

" Peter Hodge - added 'clone' keyword here
" Define
syn cluster phpClExpressions add=phpObjectOperator
syn keyword	phpObjectOperator contained new
      \ nextgroup=@phpClClasses,@phpClInterfaces,phpStructureHere skipwhite skipempty
syn keyword	phpObjectOperator contained clone

" Todo
syn keyword	phpTodo	contained todo fixme xxx containedin=phpComment

" Parent
if s:strict_blocks
  syn cluster phpClExpressions add=phpBlockRegion
  if s:folding == 2
      syn region phpBlockRegion matchgroup=phpBrace start=/{/ end=/}/ keepend extend
            \ transparent contained
            \ fold
  else
      syn region phpBlockRegion matchgroup=phpBrace start=/{/ end=/}/ keepend extend
            \ transparent contained
      if s:fold_manual
        syn region phpBlockRegion matchgroup=phpBrace start='{\ze\s*//\s*fold\s*$\c' end='}' keepend extend
              \ transparent contained
              \ fold
      endif
  endif

  " parenthesis for a foreach() block, not found automatically
  " (is triggered by a nextgroup=phpForeachRegion)
  " Note: the 'display' option on a foreach region (the part inside the '()')
  " would be bad, because it is possible for that to be spread over several
  " lines (well, I do it myself)
  if s:alt_arrays || s:alt_control_parents
    syn region phpForeachRegion matchgroup=phpControlParent start=/(/ end=/)/ keepend extend
            \ contained contains=@phpClExpressions,phpArrayPair
            \ nextgroup=phpSemicolonNotAllowedHere skipwhite skipempty
  endif

  " parenthesis for a for() block, not found automatically
  " (is triggered by a nextgroup=phpForRegion)
  if s:alt_arrays || s:alt_control_parents
    syn region phpForRegion matchgroup=phpControlParent
            \ start=/(/ end=/)/ keepend extend display
            \ contained contains=@phpClExpressions,phpForSemicolon
            \ nextgroup=phpSemicolonNotAllowedHere skipwhite skipempty
    syn match phpForSemicolon contained display /[,;]/
    hi! link phpForSemicolon phpConditional
  endif

  " special parent regions for 'if/while' blocks so we can catch a semicolon
  " which shouldn't be at the end
  " Note: having endings on those keywords helps speed things up alot.
  if s:no_empty_construct
    syn region phpConstructRegion keepend extend contained contains=@phpClExpressions
          \ nextgroup=phpSemicolonNotAllowedHere skipwhite skipempty
          \ matchgroup=phpControlParent start=/(/ end=/)/
          \ matchgroup=Error end=/}/ end=/\]/
            \ end=/\$\@<!\<\%(protected\|public\|private\)\>/
            \ end=/\$\@<!\<\%(final\|abstract\|static\|global\)\>/
            \ end=/\$\@<!\<\%(class\|function\|interface\|extends\)\>/
            \ end=/\$\@<!\<\%(return\|break\|continue\|case\|default\|echo\)\>/
    syn region phpSwitchConstructRegion keepend extend contained contains=@phpClExpressions
          \ nextgroup=phpSemicolonNotAllowedHere,phpSwitchBlock skipwhite skipempty
          \ matchgroup=phpControlParent start=/(/ end=/)/
          \ matchgroup=Error end=/}/ end=/\]/
            \ end=/\$\@<!\<\%(protected\|public\|private\)\>/
            \ end=/\$\@<!\<\%(final\|abstract\|static\|global\)\>/
            \ end=/\$\@<!\<\%(class\|function\|interface\|extends\)\>/
            \ end=/\$\@<!\<\%(return\|break\|continue\|case\|default\|echo\)\>/
    syn region phpDoWhileConstructRegion keepend extend contained contains=@phpClExpressions
          \ matchgroup=phpControlParent start=/(/ end=/)\_s*;/
          \ matchgroup=Error end=/}/ end=/\]/ end=/;/
            \ end=/\$\@<!\<\%(protected\|public\|private\)\>/
            \ end=/\$\@<!\<\%(final\|abstract\|static\|global\)\>/
            \ end=/\$\@<!\<\%(class\|function\|interface\|extends\)\>/
            \ end=/\$\@<!\<\%(return\|break\|continue\|case\|default\|echo\)\>/
  endif

  " match up ( and ), as well as [ and ]
  syn cluster phpClExpressions add=phpParentRegion,phpBracketRegion
  syn region phpParentRegion contained keepend extend contains=@phpClExpressions
        \ matchgroup=phpParent start=/(/ end=/)/
        \ matchgroup=Error end=/;/ end=/}/ end=/\]/
  " NOTE: the 'dispay' option on a [] region isn't so dangerous, as they are
  " normally only one line
  " TODO: does the 'display' option break folding for php_fold_arrays? The
  " answer is YES
  syn region phpBracketRegion contained keepend extend contains=@phpClExpressions
        \ matchgroup=phpParent start=/\[/ end=/\]/
        \ matchgroup=Error end=/;/

  " when a closing }, ) or ] is out of place ...
  if s:parent_error_close
    syn cluster phpClValues add=phpBraceError,phpParentError
    syn match phpBraceError  contained display /}/
    syn match phpParentError contained display /)/
    syn match phpParentError contained display /\]/
  endif

else
  syn match phpParent contained display /{/
  syn match phpParent contained display /}/
  syn match phpParent contained display /\[/
  syn match phpParent contained display /\]/
  syn match phpParent contained display /(/
  syn match phpParent contained display /)/
endif

syn cluster	phpClTop      add=phpFoldFunction,phpFoldClass,phpFoldInterface

" PHP Region
if s:long_tags
  syn region phpRegion matchgroup=phpRegionDelimiter start=/<?php\w\@!/ end=/?>/
        \ keepend extend contains=@phpClTop
else
  syn region phpRegion matchgroup=phpRegionDelimiter start=/<?\(php\w\@!\|=\)\=/ end=/?>/
        \ keepend extend contains=@phpClTop
endif

syn region phpRegionSc matchgroup=phpRegionDelimiter
      \ start=#<script language="php"># end=#</script>#
      \ contains=@phpClTop keepend extend

if s:asp_tags
  syn region phpRegionAsp matchgroup=phpRegionDelimiter start=/<%=\=/ end=/%>/
        \ keepend extend contains=@phpClTop
endif

if s:strict_blocks
  syn cluster phpClValues add=phpHTMLError
  syn match phpHTMLError /?>/ contained
endif

" if using strict blocks, need to look out for HTML inside
" blocks
if s:strict_blocks
  " only allow in base-level code (not inside () or [])
  syn cluster phpClCode add=htmlRegion
  if s:long_tags
    " only match full php tags
    syn region htmlRegion contained contains=TOP matchgroup=phpRegionDelimiter
          \ start=/?>/ end=/<?php\w\@!/ keepend extend
  else
    " match any php tags
    syn region htmlRegion contained contains=TOP matchgroup=phpRegionDelimiter
          \ start=/?>/ end=/<?\%(php\w\@!\|=\)\=/ keepend extend
  endif

  if s:asp_tags
    syn region htmlRegion contained contains=TOP matchgroup=phpRegionDelimiter
          \ start=/%>/ end=/<%=\=/ keepend extend
  endif
endif

" if strict curly-braces matching is enabled, then match braces
" properly
if s:strict_blocks
  " DEFINITIONS FOR:
  "   function ...() {
  "   class ... {
  "     method ...() {
  " these need to be done piece-by-piece so that we can use 'nextgroups'
  " to match the { } code blocks - we want to use special colors for them!
  " {{{1

  " Match the 'final' and 'abstract' keywords first, they can be inside the
  " global scope or inside a class declaration
  syn cluster phpClTop add=phpStructureType
  syn cluster phpClInClass add=phpStructureType
  syn keyword phpStructureType contained abstract final

  " the phpStructure keywords (class/interface) can be found anywhere in
  " global scope
  syn cluster phpClTop add=phpStructure

  " CLASSES: class myFoo extends baseFoo implements foo, Iterator { }: {{{2
  " I MATCH: <class myFoo> extends baseFoo implements foo, Iterator { }: {{{3
  
    " 2: match the start of the class declaration
    syn keyword phpStructure contained class
          \ nextgroup=phpDefineClassName skipwhite skipempty

    " 3: an empty placeholder for any class name (which in turn can contain
    " any of the known PHP class names)
    " NOTE: allow matching the class block immediately after the class name
    syn cluster phpClClassHere add=phpDefineClassName
    syn match phpDefineClassName /\h\w*/ contained contains=@phpClStructures
          \ nextgroup=@phpClDefineClassBlock skipwhite skipempty

  " II MATCH: class myFoo <extends baseFoo> implements foo, Iterator { }: {{{3

      " match the 'extends' keyword and follow it by the match
      " for class names in a declaration (as above)
      syn keyword phpStructure contained extends
            \ nextgroup=phpDefineClassName skipwhite skipempty

  " III MATCH: class myFoo extends baseFoo <implements foo, Iterator> { }: {{{3

      " 1: match the 'implements' keyword and follow it by the match
      " for class names in a declaration (as above)
      syn keyword phpStructure contained implements
            \ nextgroup=@phpClDefineClassImplements skipwhite skipempty

      " 2: define a place-holding for interfaces which matches any valid
      " interface name and also contains the recognized names
      syn cluster phpClDefineClassImplements add=phpDefineClassImplementsName
      syn cluster phpClInterfaceHere add=phpDefineClassImplementsName
      syn cluster phpClClassHere add=phpDefineClassImplementsName
      syn match phpDefineClassImplementsName /\h\w*/ contained contains=@phpClStructures
            \ nextgroup=@phpClDefineClassImplements skipwhite skipempty

      " 3: allow a comma in the list
      syn cluster phpClDefineClassImplements add=phpDefineClassImplementsComma
      syn match phpDefineClassImplementsComma /,/ contained
            \ nextgroup=@phpClDefineClassImplements skipwhite skipempty

      " 4: there might be a '#' or '//'-style comment in-between!
      syn cluster phpClDefineClassImplements add=phpDefineClassImplementsCommentOneLine
      syn region phpDefineClassImplementsCommentOneLine
            \ start=/#/ start=,//, end=/$/ end=/.\ze?>/ oneline
            \ contained contains=phpComment
            \ nextgroup=@phpClDefineClassImplements skipwhite skipempty

      " 5: there might a C-style comment (/*...*/) in-between
      syn cluster phpClDefineClassImplements add=phpDefineClassImplementsCommentCStyle
      syn region phpDefineClassImplementsCommentCStyle start=,/\*, end=,\*/, keepend
            \ contained contains=@Spell
            \ nextgroup=@phpClDefineClassImplements skipwhite skipempty
      hi link phpDefineClassImplementsCommentCStyle phpComment

      " 6: add the block to the list so it can match here also
      syn cluster phpClDefineClassImplements add=phpClassBlock

  " IV MATCH: class myFoo extends baseFoo implements foo, Iterator <{ }>: {{{3

    " 1: there might be a '#' or '//'-style comment in-between!
    syn cluster phpClDefineClassBlock add=phpDefineClassBlockCommentOneline
    syn region phpDefineClassBlockCommentOneline start=/#/ start=,//, end=/$/ end=/.\ze?>/ oneline
          \ contained contains=phpComment
          \ nextgroup=@phpClDefineClassBlock skipwhite skipempty

    " 2: there might a C-style comment (/*...*/) in-between
    syn cluster phpClDefineClassBlock add=phpDefineClassBlockCommentCStyle
    syn region phpDefineClassBlockCommentCStyle start=,/\*, end=,\*/, keepend
          \ contained contains=@Spell
          \ nextgroup=@phpClDefineClassBlock skipwhite skipempty
    hi link phpDefineClassBlockCommentCStyle phpComment

    " 3: look for the actual { } block
    syn cluster phpClDefineClassBlock add=phpClassBlock
    if (s:folding == 1) || (s:folding == 2)
      syn region phpClassBlock matchgroup=phpBraceClass start=/{/ end=/}/ keepend extend
            \ contained contains=@phpClInClass
            \ matchgroup=phpHTMLError end=/?>/
            \ fold
    else
      syn region phpClassBlock matchgroup=phpBraceClass start=/{/ end=/}/ keepend extend
            \ contained contains=@phpClInClass
            \ matchgroup=phpHTMLError end=/?>/
      if s:fold_manual
        syn region phpClassBlock matchgroup=phpBraceClass start='{\ze\s*//\s*fold\s*$\c' end='}' keepend extend
              \ contained contains=@phpClInClass
              \ matchgroup=phpHTMLError end=/?>/
              \ fold
      endif
    endif


  " }}}2

  " INTERFACES: interface myFoo extends baseFoo { }: {{{2
  " I MATCH: <interface myFoo> extends baseFoo { }: {{{3
  
    " 1: match the start of the interface declaration
    syn keyword phpStructure contained interface
          \ nextgroup=phpDefineInterfaceName skipwhite skipempty

    " 2: an empty placeholder for any interface name (which in turn can contain
    " any of the known PHP class names)
    " NOTE: allow matching the class block immediately after the class name
    " NOTE: maybe one day will make a separate block for interface bodies
    syn cluster phpClClassHere add=phpDefineInterfaceName
    syn cluster phpClInterfaceHere add=phpDefineInterfaceName
    syn match phpDefineInterfaceName /\h\w*/ contained contains=@phpClStructures
          \ nextgroup=@phpClDefineClassBlock skipwhite skipempty

  " II MATCH: interface myFoo <extends baseFoo> { }: {{{3

    " NOTE: this is handled in the class syntax handling above

  " IV MATCH: class myFoo extends baseFoo implements foo, Iterator <{ }>: {{{3

    " NOTE: this is handled in the class syntax handling above

  " }}}2

  " FUNCTIONS: function & somefunc($a = 0, &$b) { }: {{{2
  " I MATCH: <function> & somefunc($a = 0, &$b) { }: {{{3

    " if we are finding functions anywhere, allow this match only
    syn cluster phpClCode add=phpDefine

    syn keyword phpDefine function contained
          \ nextgroup=@phpClDefineFuncName,phpDefineFuncByRef
          \ skipwhite skipempty

  " II MATCH: function <&> somefunc($a = 0, &$b) { }: {{{3

    " second, there might be a '&' return-by-reference option, so add
    " a match for that.
    syn match phpDefineFuncByRef /&/ contained nextgroup=@phpClDefineFuncName skipwhite skipnl
    hi link phpDefineFuncByRef phpAssignByRef


  " III MATCH: function & <somefunc>($a = 0, &$b) { }: {{{3

    " what can go inside a function name? Anything that does will need
    " a 'nextgroup=phpDefineFuncProto' argument!

    " first up, an empty placeholder to match any valid function name.
    "     It should contain the special user-defineable function names.
    syn cluster phpClDefineFuncName add=phpDefineFuncName
    syn cluster phpClFunctionHere add=phpDefineFuncName
    syn match phpDefineFuncName /\h\w*/ contained
          \ contains=@phpClFunctions
          \ nextgroup=phpDefineFuncProto
          \ skipwhite skipempty
    " TODO: allow adding comments between 'function' and 'someFunc'


  " IV MATCH: function & somefunc<(>$a = 0, &$b<)> { }: {{{3
  " match the parenthesis surrounding the function arguments
  if s:folding
    syn region phpDefineFuncProto contained contains=@phpClDefineFuncProtoArgs
          \ matchgroup=phpParent start=/(/ end=/)/ keepend extend
          \ nextgroup=@phpClDefineFuncBlock
          \ skipwhite skipempty
          \ fold
  else
    syn region phpDefineFuncProto contained contains=@phpClDefineFuncProtoArgs
          \ matchgroup=phpParent start=/(/ end=/)/ keepend extend
          \ nextgroup=@phpClDefineFuncBlock
          \ skipwhite skipempty
  endif
  " TODO: allow comments in this cluster


  " V MATCH: function & somefunc(<stdClass> $a = 0, &$b) { }: {{{3
  " first: any valid class name
  syn cluster phpClDefineFuncProtoArgs add=@phpClClasses,@phpClInterfaces

  " we still need to match an 'array' keyword, because it can be used for
  " parameter type-requirements
  syn cluster phpClDefineFuncProtoArgs add=phpProtoArrayCheck
  syn match phpProtoArrayCheck /\<array\>/ contained
  hi link phpProtoArrayCheck phpArray

  " VI MATCH: function & somefunc(stdClass <$a => 0, <&$b>) { }: {{{3

    " 1: match the by-ref '&'
    syn cluster phpClDefineFuncProtoArgs add=phpProtoArgByRef
    syn match phpProtoArgByRef /&/ display contained
    hi link phpProtoArgByRef phpAssignByRef

    " 2: match a valid identifier
    syn cluster phpClDefineFuncProtoArgs add=phpIdentifier,phpAssign

  " VII MATCH: function & somefunc(stdClass $a = <0>, &$b) { }: {{{3
  " What about other items? numbers? strings? arrays()?
  syn cluster phpClDefineFuncProtoArgs add=@phpClProtoValues

    " 1: simple types (null, boolean)
    syn cluster phpClProtoValues add=phpNull
    syn cluster phpClProtoValues add=phpBoolean

    " 2: numbers and strings and constants
    syn cluster phpClProtoValues add=phpNumber,phpFloat
    syn cluster phpClProtoValues add=phpStringSingle,phpStringDouble
    syn cluster phpClProtoValues add=@phpClConstants

    " 3: arrays must be done separately to ensure ( and ) match correctly
    " (but only if using alt colors for arrays)
    if s:alt_arrays
      syn cluster phpClProtoValues add=phpProtoArray
      syn region phpProtoArray matchgroup=phpArrayParens start=/\<array\_s*(/ end=/)/ keepend extend
          \ contained contains=@phpClProtoValues,phpArrayPair

      " don't allow arbitrary parenthesis here!!
      syn cluster phpClProtoValues add=phpProtoParentError
      syn match phpProtoParentError /(/ contained display
      hi link phpProtoParentError phpParentError
    else
      syn cluster phpClProtoValues add=phpArray

      " need to allow arbitrary parenthesis for arrays
      syn cluster phpClProtoValues add=phpParentRegion
    endif

    " 4: comments
    syn cluster phpClProtoValues add=phpComment


  " VIII MATCH: function & somefunc(</* foo */>) { }: {{{3
  " What about comment items?
  syn cluster phpClDefineFuncProtoArgs add=phpComment

  " IX MATCH: function & somefunc(stdclass $a = 0, &$b) <{ }>: {{{3

    " 1: there might be a '#' or '//'-style comment in-between!
    syn cluster phpClDefineFuncBlock add=phpDefineFuncBlockCommentOneline
    syn region phpDefineFuncBlockCommentOneline start=/#/ start=,//, end=/$/ end=/.\ze?>/ oneline
          \ contained contains=phpComment
          \ nextgroup=@phpClDefineFuncBlock skipwhite skipempty

    " 2: there might a C-style comment (/*...*/) in-between
    syn cluster phpClDefineFuncBlock add=phpDefineFuncBlockCommentCStyle
    syn region phpDefineFuncBlockCommentCStyle start=,/\*, end=,\*/, keepend
          \ contained contains=@Spell
          \ nextgroup=@phpClDefineFuncBlock skipwhite skipempty
    hi link phpDefineFuncBlockCommentCStyle phpComment

    " 3: look for the actual { } block
    "    NOTE: how the function block will end at the next function
    "    declaration: this helps stop the region extending indefinitely,
    "    forcing the recalculation of all { } blocks for the rest of the file.
    "    Otherwise, inserting an open-brace will 
    "    NOTE: that the error can't happen on a 'final', 'abstract', 'class',
    "    or 'interface' keyword because they can't be contained in a function
    syn cluster phpClDefineFuncBlock add=phpFuncBlock

    let s:foldHere = s:folding ? 'fold' : ''
    let s:endEarly = s:nested_functions ? '' : 'matchgroup=Error end=/\%(^\|\s\)function\>/'

"    if s:folding
"      if s:nested_functions
"        syn region phpFuncBlock keepend extend matchgroup=phpBraceFunc start=/{/ end=/}/
"              \ matchgroup=Error end=/\%(^\|\s\)\%(public\|private\|protected\)\>/
"              \ contained contains=@phpClInFunction
"              \ fold
"      else
"        syn region phpFuncBlock keepend extend matchgroup=phpBraceFunc start=/{/ end=/}/
"              \ matchgroup=Error end=/\%(^\|\s\)function\>/
"              \ matchgroup=Error end=/\%(^\|\s\)\%(public\|private\|protected\)\>/
"              \ contained contains=@phpClInFunction
"              \ fold
"      endif
"    else
"      if s:nested_functions
"        syn region phpFuncBlock keepend extend matchgroup=phpBraceFunc start=/{/ end=/}/
"              \ matchgroup=Error end=/\%(^\|\s\)\%(public\|private\|protected\)\>/
"              \ contained contains=@phpClInFunction
"      else
"        syn region phpFuncBlock keepend extend matchgroup=phpBraceFunc start=/{/ end=/}/
"              \ matchgroup=Error end=/\%(^\|\s\)function\>/
"              \ matchgroup=Error end=/\%(^\|\s\)p\%(ublic\|rivate\|rotected\)\>/
"              \ contained contains=@phpClInFunction
"      endif
"    endif

        execute 'syn region phpFuncBlock keepend extend matchgroup=phpBraceFunc'
              \ 'end=/}/ start=/{/'
              \ 'matchgroup=Error end=/\%(^\|\s\)\%(public\|private\|protected\)\>/'
              \ s:endEarly
              \ 'contained contains=@phpClInFunction'
              \ s:foldHere
        " for manual folding, we use an alternate start
        if s:fold_manual
          execute 'syn region phpFuncBlock keepend extend matchgroup=phpBraceFunc'
                \ 'start=#{\ze\s*//\s*fold\s*$\c# end=/}/'
                \ 'matchgroup=Error end=/\%(^\|\s\)\%(public\|private\|protected\)\>/'
                \ s:endEarly
                \ 'contained contains=@phpClInFunction'
                \ s:foldHere
        endif
    unlet s:foldHere s:endEarly

  " }}}2

  " METHODS: protected function & somefunc($a = 0, &$b) { }: {{{2
  " I MATCH: <protected function> somefunc($a = 0, &$b) { }: {{{3

    " 1: match the final / abstract / private keywords at start
    " TODO: rename 'phpStorageClass' to Typedef (for global and static keywords)
    " and rename 'phpStorageClass2' to 'phpStorageClass'
    syn cluster phpClInClass add=phpStorageClass2
    syn keyword phpStorageClass2 contained private protected public static final abstract
    hi link phpStorageClass2 phpStorageClass

    syn keyword phpDefineMethod function contained containedin=phpClassBlock
          \ nextgroup=@phpClDefineMethodName,phpDefineMethodByRef
          \ skipwhite skipempty
    " TODO: add phpDefineFunction in the proper place
    hi link phpDefineFunction phpDefine
    hi link phpDefineMethod phpDefineFunction

  " II MATCH: protected function <&> somefunc($a = 0, &$b) { }: {{{3
  " second, there might be a '&' return-by-reference option, so add
  " a match for that.
  syn match phpDefineMethodByRef /&/ contained
        \ nextgroup=@phpClDefineMethodName skipwhite skipnl
  hi link phpDefineMethodByRef phpDefineFuncByRef

  " III MATCH: protected function & <somefunc>($a = 0, &$b) { }: {{{3
  " what can go inside a method name? Anything that does will need
  " a 'nextgroup=phpDefineMethodProto' argument!

    " An empty placeholder to match any valid method name.
    " It should contain the special user-defineable method names.
    " NOTE: how we are just re-using 'function' block instead of
    " making more stuff to have a special 'method' block also.
    " I don't think it would be worthwhile at this stage.
    " NOTE: phpSpecialFunction must be included as well, because
    " that's a reserved function name and will break things.
    " TODO: cater for a new group, phpReservedFunction
    syn cluster phpClDefineMethodName add=phpDefineMethodName
    syn cluster phpClMethodHere add=phpDefineMethodName
    syn match phpDefineMethodName /\h\w*/ contained
          \ contains=phpSpecialFunction,@phpClMethods
          \ nextgroup=phpDefineFuncProto
          \ skipwhite skipempty
    " TODO: allow adding comments between 'function' and 'someFunc'

  " }}}2

  " EXCEPTIONS: try/catch { } {{{2

    syn cluster phpClCode add=phpException
  
    " 1: match the start of a try block
    syn keyword phpException try contained nextgroup=@phpClTryBlock skipwhite skipnl

    " TODO: 2: allow having comments preceding the { } block?
  
    " 3: match the try block
    syn cluster phpClTryBlock add=phpTryBlock
    " TODO: manual folding from here (search for \<fold\>)
    if s:folding == 2
      syn region phpTryBlock matchgroup=phpBraceException start=/{/ end=/}/ keepend extend
            \ contained transparent
            \ fold
    else
      syn region phpTryBlock matchgroup=phpBraceException start=/{/ end=/}/ keepend extend
            \ contained transparent
    endif

    " 3: match the start of the catch block
    syn keyword phpException catch contained nextgroup=phpCatchRegion skipwhite skipnl
    syn region phpCatchRegion matchgroup=phpParent start=/(/ end=/)/ keepend extend
          \ contained contains=@phpClExpressions
          \ nextgroup=@phpClCatchBlock skipwhite skipnl

    " TODO: 4: allow having comments preceding the { } block?

    " 5: match the catch block
    syn cluster phpClCatchBlock add=phpCatchBlock
    if s:folding == 2
      syn region phpCatchBlock matchgroup=phpBraceException start=/{/ end=/}/ keepend extend
            \ contained transparent
            \ fold
    else
      syn region phpCatchBlock matchgroup=phpBraceException start=/{/ end=/}/ keepend extend
            \ contained transparent
    endif

  " }}}2

  " }}}1

  " make sure 'static' and 'global' work inside a function block
  " TODO: refactor this?
  syn keyword phpStorageClass static global contained

  " set foldmethod if folding
  if s:folding
    set foldmethod=syntax
  endif
else
  " Fold
  if s:folding == 1 " {{{1
  " match one line constructs here and skip them at folding
    syn keyword	phpSCKeyword	abstract final private protected public static	contained
    syn keyword	phpFCKeyword	function	contained
    syn keyword	phpStorageClass	global	contained
    syn match	phpDefine	"\(\s\|^\)\(abstract\s\+\|final\s\+\|private\s\+\|protected\s\+\|public\s\+\|static\s\+\)*function\(\s\+.*[;}]\)\@="	contained contains=phpSCKeyword
    syn match	phpStructure	"\(\s\|^\)\(abstract\s\+\|final\s\+\)*class\(\s\+.*}\)\@="	contained
    syn match	phpStructure	"\(\s\|^\)interface\(\s\+.*}\)\@="	contained
    syn match	phpException	"\(\s\|^\)try\(\s\+.*}\)\@="	contained
    syn match	phpException	"\(\s\|^\)catch\(\s\+.*}\)\@="	contained

    set foldmethod=syntax
    syn region	phpFoldHtmlInside contained transparent contains=@htmlTop
          \ matchgroup=phpRegionDelimiter start="?>" end="<?\(php\w\@!\)\="
    syn region	phpFoldFunction contained transparent fold extend
          \ matchgroup=StorageClass
          \ start="^\z(\s*\)\(abstract\s\+\|final\s\+\|private\s\+\|protected\s\+\|public\s\+\|static\s\+\)*function\s\([^};]*$\)\@="rs=e-9
          \ matchgroup=Delimiter end="^\z1}"
          \ contains=@phpClInFunction,phpFoldHtmlInside,phpFCKeyword
    syn region	phpFoldFunction	matchgroup=Define start="^function\s\([^};]*$\)\@="
          \ matchgroup=Delimiter end="^}"
          \ contains=@phpClInFunction,phpFoldHtmlInside contained transparent fold extend
    syn region	phpFoldClass	matchgroup=Structure start="^\z(\s*\)\(abstract\s\+\|final\s\+\)*class\s\+\([^}]*$\)\@=" matchgroup=Delimiter end="^\z1}"
          \ contains=@phpClInFunction,phpFoldFunction,phpSCKeyword contained transparent fold extend
    syn region	phpFoldInterface	matchgroup=Structure start="^\z(\s*\)interface\s\+\([^}]*$\)\@=" matchgroup=Delimiter end="^\z1}" contains=@phpClFunction,phpFoldFunction contained transparent fold extend
    syn region	phpFoldCatch	matchgroup=Exception start="^\z(\s*\)catch\%([^}]*$\)\@=" matchgroup=Delimiter end="^\z1}" contains=@phpClFunction,phpFoldFunction contained transparent fold extend
    syn region	phpFoldTry	matchgroup=Exception start="^\z(\s*\)try\s\+\([^}]*$\)\@=" matchgroup=Delimiter end="^\z1}" contains=@phpClFunction,phpFoldFunction contained transparent fold extend

  elseif s:folding == 2 " {{{1
    syn keyword	phpDefine	function	contained
    syn keyword	phpStructure	abstract class interface	contained
    syn keyword	phpException	catch throw try	contained
    syn keyword	phpStorageClass	final global private protected public static	contained

    set foldmethod=syntax
    syn region	phpFoldHtmlInside	matchgroup=phpRegionDelimiter start="?>" end="<?\(php\w\@!\)\=" contained transparent contains=@htmlTop
    syn region	phpParent	matchgroup=Delimiter start="{" end="}" keepend extend contained contains=@phpClFunction,phpFoldHtmlInside transparent fold

  elseif s:folding == 3 " {{{1
    " match one line constructs here and skip them at folding
    syn keyword	phpSCKeyword	abstract final private protected public static	contained
    syn keyword	phpFCKeyword	function	contained
    syn keyword	phpStorageClass	global static contained
    syn keyword	phpException	catch throw try	contained

    syn match	phpDefine	"\(\s\|^\)\(abstract\s\+\|final\s\+\|private\s\+\|protected\s\+\|public\s\+\|static\s\+\)*function\(\s\+.*[;}]\)\@="	contained contains=phpSCKeyword
    syn match	phpStructure	"\(\s\|^\)\(abstract\s\+\|final\s\+\)*class\(\s\+.*}\)\@="	contained
    syn match	phpStructure	"\(\s\|^\)interface\(\s\+.*}\)\@="	contained
    syn match	phpException	"\(\s\|^\)try\(\s\+.*}\)\@="	contained
    syn match	phpException	"\(\s\|^\)catch\(\s\+.*}\)\@="	contained

    " fold these:
    set foldmethod=syntax
    syn region	phpFoldFunction	matchgroup=StorageClass start="^\z(\s*\)\(abstract\s\+\|final\s\+\|private\s\+\|protected\s\+\|public\s\+\|static\s\+\)*function\s\([^};]*$\)\@="rs=e-9 matchgroup=Delimiter end="^\z1}" contains=@phpClFunction,phpFoldHtmlInside,phpFCKeyword contained transparent fold extend
    syn region	phpFoldFunction	matchgroup=Define start="^function\s\([^};]*$\)\@=" matchgroup=Delimiter end="^}" contains=@phpClFunction,phpFoldHtmlInside contained transparent fold extend

    " don't fold these:
    syn region	phpFoldHtmlInside	matchgroup=phpRegionDelimiter start="?>" end="<?\(php\w\@!\)\=" contained transparent contains=@htmlTop
    syn region	phpFoldClass	matchgroup=Structure start="^\z(\s*\)\(abstract\s\+\|final\s\+\)*class\s\+\([^}]*$\)\@=" matchgroup=Delimiter end="^\z1}" contains=@phpClFunction,phpFoldFunction,phpSCKeyword contained transparent extend
    syn region	phpFoldInterface	matchgroup=Structure start="^\z(\s*\)interface\s\+\([^}]*$\)\@=" matchgroup=Delimiter end="^\z1}" contains=@phpClFunction,phpFoldFunction contained transparent extend
    syn region	phpFoldCatch	matchgroup=Exception start="^\z(\s*\)catch\s\+\([^}]*$\)\@=" matchgroup=Delimiter end="^\z1}" contains=@phpClFunction,phpFoldFunction contained transparent extend
    syn region	phpFoldTry	matchgroup=Exception start="^\z(\s*\)try\s\+\([^}]*$\)\@=" matchgroup=Delimiter end="^\z1}" contains=@phpClFunction,phpFoldFunction contained transparent extend

  else " {{{1
    syn keyword	phpDefine       contained function
    syn keyword	phpStructure	contained abstract class interface
    syn keyword	phpException	contained catch throw try
    syn keyword	phpStorageClass	contained final global private protected public static
  endif " }}} 1

  " always need these
  syn keyword phpStructure contained extends implements
endif

" ================================================================
" Peter Hodge - June 9, 2006
" Some of these changes (highlighting isset/unset/echo etc) are not so
" critical, but they make things more colourful. :-)


" different syntax highlighting for 'echo', 'print', 'switch', 'die' and 'list' keywords
" to better indicate what they are.
" TODO: convert 'echo' and 'include' to regions so that it cannot contain other control
" structures
"syn cluster phpClCode add=phpEcho
"syn keyword phpEcho contained echo
syn cluster phpClCode add=phpEchoRegion
if s:strict_blocks
  syn region phpEchoRegion contained keepend extend contains=@phpClExpressions,phpEchoComma
        \ matchgroup=phpEcho start=/\$\@<!\<echo\>/ end=/;/ end=/\ze?>/
        \ matchgroup=Error end=/[})\]]/
else
  syn cluster phpClCode add=phpEcho
  syn keyword phpEcho contained echo
endif

syn match phpEchoComma contained display /,/
hi! link phpEchoComma phpEcho

syn cluster phpClExpressions add=phpPrint,phpInclude
syn keyword phpPrint contained print
syn keyword	phpInclude contained include require include_once require_once

" match when a '(type)' is used to cast the type of something

" Highlighting for PHP5's user-definable magic class methods
syn cluster phpClMethods add=phpSpecialMethods
syn keyword phpSpecialMethods contained containedin=phpRegion
    \ __construct __destruct __sleep __wakeup __clone __set_state __toString
    \ __set __get __unset __isset __call

" these methods are used by the SPL Interfaces
syn cluster phpClMethods add=phpSPLMethods
" Serializable
syn keyword phpSPLMethods contained serialize unserialize
" ArrayAccess
syn keyword phpSPLMethods contained offsetSet offsetGet offsetExists offsetUnset
" Iterator
syn keyword phpSPLMethods contained current next key valid rewind
" IteratorAggregate
syn keyword phpSPLMethods contained getIterator
" RecursiveIterator
syn keyword phpSPLMethods contained hasChildren getChildren current next key valid rewind
" OuterIterator
syn keyword phpSPLMethods contained getInnerIterator current next key valid rewind
" SeekableIterator
syn keyword phpSPLMethods contained seek current next key valid rewind 
" Countable
syn keyword phpSPLMethods contained count 
" SplObserver
syn keyword phpSPLMethods contained update
" SplSubject
syn keyword phpSPLMethods contained attach detach notify 
" Reflector
syn keyword phpSPLMethods contained export
hi link phpSPLMethods phpSpecialMethods

syn keyword phpSpecialFunction contained __autoload

" Highlighting for PHP5's built-in classes
" - built-in classes harvested from get_declared_classes() in 5.1.4
syn cluster phpClClasses add=phpClasses
syn keyword phpClasses contained containedin=phpRegion
	\ stdClass __PHP_Incomplete_Class php_user_filter Directory ArrayObject
	\ Exception ErrorException LogicException BadFunctionCallException BadMethodCallException DomainException
	\ RecursiveIteratorIterator IteratorIterator FilterIterator RecursiveFilterIterator ParentIterator LimitIterator
	\ CachingIterator RecursiveCachingIterator NoRewindIterator AppendIterator InfiniteIterator EmptyIterator
	\ ArrayIterator RecursiveArrayIterator DirectoryIterator RecursiveDirectoryIterator
	\ InvalidArgumentException LengthException OutOfRangeException RuntimeException OutOfBoundsException
	\ OverflowException RangeException UnderflowException UnexpectedValueException
	\ PDO PDOException PDOStatement PDORow
	\ Reflection ReflectionFunction ReflectionParameter ReflectionMethod ReflectionClass
	\ ReflectionObject ReflectionProperty ReflectionExtension ReflectionException
	\ SplFileInfo SplFileObject SplTempFileObject SplObjectStorage
	\ XMLWriter LibXMLError XMLReader SimpleXMLElement SimpleXMLIterator
	\ DOMException DOMStringList DOMNameList DOMDomError DOMErrorHandler
	\ DOMImplementation DOMImplementationList DOMImplementationSource
	\ DOMNode DOMNameSpaceNode DOMDocumentFragment DOMDocument DOMNodeList DOMNamedNodeMap
	\ DOMCharacterData DOMAttr DOMElement DOMText DOMComment DOMTypeinfo DOMUserDataHandler
	\ DOMLocator DOMConfiguration DOMCdataSection DOMDocumentType DOMNotation DOMEntity
	\ DOMEntityReference DOMProcessingInstruction DOMStringExtend DOMXPath
  \ DateTime DateTimeZone

" Highlighting for PHP5's built-in interfaces
" - built-in classes harvested from get_declared_interfaces() in 5.1.4
syn cluster phpClInterfaces add=phpInterfaces
syn keyword phpInterfaces contained
	\ Iterator IteratorAggregate RecursiveIterator OuterIterator SeekableIterator
	\ Traversable ArrayAccess Serializable Countable SplObserver SplSubject Reflector
"
" add php_errormsg as a special variable


"syn cluster phpClExpressions add=phpProperty
"syn match phpProperty /->\_s*\%(\$\=\h\w*\)\@>\ze\_s*(\@!/ display extend
"      \ contained contains=phpPropertySelector,phpIdentifier,@phpClProperties
"syn match phpPropertySelector /->/ contained display

" for going in string where can be followed by () without making it a method
" call
"syn match phpPropertySelectorInString /->/ contained display
"hi link phpPropertySelectorInString phpPropertySelector

if s:special_functions
  " Highlighting for PHP built-in functions which exhibit special behaviours
  " - isset()/unset()/empty() are not real functions.
  " - compact()/extract() directly manipulate variables in the local scope where
  "   regular functions would not be able to.
  " - eval() and assert()
  " - user_error()/trigger_error() can be overloaded by set_error_handler and also
  "   have the capacity to terminate your script when type is E_USER_ERROR.
  syn cluster phpClFunctions add=phpSpecialFunction
  syn keyword phpSpecialFunction contained
        \ user_error trigger_error isset unset empty eval assert extract compact __halt_compiler
endif

" special highlighting for '=&' operator
syn cluster phpClExpressions add=phpAssignByRef
syn match phpAssignByRef /=\_s*&/ contained display

" call-time pass-by-reference
syn match phpAssignByRef /&\$\@=/	contained display

" highlighting for the '@' error-supressing operator
syn cluster phpClExpressions add=phpSupressErrors
syn match phpSupressErrors /@/ contained display

" ================================================================

" Sync
if s:sync == -1
"  syn sync match phpSyncKeyword grouphere phpRegion /\ze\$\@<!\<class\>/
"  syn sync match phpSyncKeyword grouphere phpRegion /\ze\$\@<!\<interface\>/

  " best things to look for are the class/interface keywords or
  " private/protected/public keywords, as we can be 100% confident where we
  " are when we find them
  if s:strict_blocks
    syn sync match phpClassStart grouphere phpClassBlock
          \ /\$\@<!\<class\%(\%(\s\+\w\+\)\+\)\@>\s*{/
    " Note: the 'var' and 'const' sync methods have been causing Vim to miss
    " out the '?>' at the end of a file, so I had to drop them out.  I'm not
    " sure if it syncs faster
"    syn sync match phpSyncKeyword grouphere phpClassBlock /\ze\$\@<!\<var\>/
"    syn sync match phpSyncKeyword grouphere phpClassBlock /\ze\$\@<!\<const\>/
"    syn sync match phpSyncKeyword grouphere phpClassBlock
"          \ /\$\@<!\<p\%(rivate\|rotected\|ublic\)\>/
  endif

  syn sync match phpSyncStartOfFile grouphere NONE /\%^/

  " watch out for strings and comments in syncing process
  " TODO: make sure this actually works
  syn sync region phpSyncComment start=/\/\// start=/#/ end=/$/
  syn sync region phpSyncString start=/\z(['"]\)/ skip=/\\./ end=/\z1/

  if s:long_tags
    syn sync match phpRegionSync grouphere phpRegion "^\s*<?php\s*$"
  else
    syn sync match phpRegionSync grouphere phpRegion "^\s*<?\(php\w\@!\)\=\s*$"
  endif
"  syn sync match phpRegionSync grouphere phpRegionSc +^\s*<script language="php">\s*$+
"  if s:asp_tags
"    syn sync match phpRegionSync grouphere phpRegionAsp "^\s*<%\(=\)\=\s*$"
"  endif
"  syn sync match phpRegionSync grouphere NONE "^\s*?>\s*$"
"  syn sync match phpRegionSync grouphere NONE "^\s*%>\s*$"
"  syn sync match phpRegionSync grouphere phpRegion "function\s.*(.*\$"
"  "syn sync match phpRegionSync grouphere NONE "/\i*>\s*$"

" Sync backwards a certain number of lines?
"elseif s:sync > 0
"  exec "syn sync minlines=" . s:sync

else
  syn sync fromstart
endif

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_php_syn_inits")
  if version < 508
    let did_php_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    "command -nargs=+ HiLink hi def link <args>
    command -nargs=+ HiLink hi link <args>
  endif

  " Peter Hodge, June 17 2006
  " - I'm optimizing these highlight links for the default
  "   colorscheme, or 'elflord' when it would make a major
  "   difference.
  "   After most HiLinks I have noted which color the group
  "   will revert back to under default or elflord.

  " TODO: remove this testing
  let s:is_elflord = (exists('g:colors_name') && g:colors_name == 'elflord')

  if exists("php_oldStyle")
    hi phpOperator guifg=SeaGreen ctermfg=DarkGreen
    hi phpIdentifier guifg=DarkGray ctermfg=Brown
"   hi phpIdentifierSimply guifg=DarkGray ctermfg=Brown
    hi phpVarSelector guifg=SeaGreen ctermfg=DarkGreen

    hi phpRelation guifg=SeaGreen ctermfg=DarkGreen

    hi phpSuperglobal guifg=Red ctermfg=DarkRed
  else
    HiLink phpOperator          Operator    " => Statement(Yellow) / Operator(Red)
    HiLink phpIdentifier        Identifier  " => Identifier(Cyan)
    HiLink phpIdentifierErratic	phpIdentifier

    HiLink phpVarSelector       Operator
    HiLink phpVarSelectorDeref  PreProc
    HiLink phpVarSelectorError  Error

    HiLink phpType              Type

    " list() / arrays
    HiLink phpList              phpType
    HiLink phpArray             phpType
    if s:alt_arrays
      HiLink phpArrayParens phpArray
      HiLink phpArrayPair phpArray

      if s:alt_arrays == 2
        HiLink phpArrayComma phpArrayParens
      endif
    else
      HiLink phpArrayParens phpParent
      HiLink phpArrayPair phpOperator
    endif
    HiLink phpListComma phpArrayComma
    HiLink phpArrayPairError Error

    if s:alt_comparisons
      if s:is_elflord
        HiLink phpRelation	Statement " => Yellow
      else
        HiLink phpRelation	Constant  " => Constant (SlateBlue)
      endif
    else
      HiLink phpRelation     phpOperator
    endif

    " special variables support:
    if s:special_vars
      " NOTE: this is better highlighted using the 'Operator' colour ...
      HiLink phpSuperglobal Operator         " => Special (orange/red)
    else
      HiLink phpSuperglobal phpIdentifier
    endif
  endif

  " support for other variables
  HiLink phpBuiltinVar phpSuperglobal
  HiLink phpLongVar    phpSuperglobal
  HiLink phpEnvVar     phpSuperglobal

  " language:
  HiLink phpComment       Comment       " Slateblue

  HiLink phpSemicolon      Macro        " => PreProc (LightMagenta)
  HiLink phpSemicolonNotAllowedHere Error

  HiLink phpDefine         Define       " => PreProc (LightMagenta)
  HiLink phpObjectOperator phpDefine
  HiLink phpInclude        Include      " => PreProc (LightMagenta)

  HiLink phpEcho           Macro        " => PreProc (LightMagenta)
  HiLink phpPrint          phpEcho

  HiLink phpParent        Delimiter     " => Special (Red)
  if s:alt_control_parents
    HiLink phpControlParent phpConditional
  else
    HiLink phpControlParent phpParent
  endif
  HiLink phpBrace         phpParent     " => Special (Red)
  HiLink phpBraceError    Error         " => Error

  if s:alt_blocks
    HiLink phpBraceFunc      phpDefine
    HiLink phpBraceClass     phpStructure
    HiLink phpBraceException phpException
  else
    HiLink phpBraceFunc      phpBrace
    HiLink phpBraceClass     phpBrace
    HiLink phpBraceException phpBrace
  endif

  " other operations
  HiLink phpSupressErrors     PreProc   " LightMagenta

  if s:alt_refs
    HiLink phpAssignByRef     Type      " Green
  else
    HiLink phpAssignByRef     Operator  " Red
  endif

  HiLink phpMemberSelector    Structure " => Type (Green)
  if s:alt_properties
    HiLink phpPropertySelector  Function  " => Identifier (Cyan) / (White)
    HiLink phpDynamicSelector   Operator  " => Operator (Red) / (White)
  else
    HiLink phpPropertySelector  phpMemberSelector
    HiLink phpDynamicSelector   phpMemberSelector
  endif


  " execution control structures
  HiLink phpConditional   Conditional   " => Statement (Yellow) / Repeat (White)
  HiLink phpRepeat        Repeat        " => Statement (Yellow) / Repeat (White)
  HiLink phpStatement     Statement     " (Yellow / Brown)
  HiLink phpCase          Label         " => Statement (Yellow / Brown)
  HiLink phpException     Exception     " => Statement (Yellow)

  " constants
  HiLink phpMagicConstant Constant      " Pink / Magenta
  HiLink phpCoreConstant  Constant      " Pink / Magenta
  HiLink phpNumber        Number        " => Constant (Pink)
  HiLink phpFloat         Float         " => Constant (Pink)
  HiLink phpBoolean       phpType
  HiLink phpNull          phpType

  HiLink phpStringSingle         String
  HiLink phpStringDouble         phpStringSingle
  HiLink phpStringDoubleConstant phpStringSingle
  HiLink phpBacktick             phpStringSingle

  HiLink phpStringLiteral SpecialChar

  HiLink phpSpecialChar   SpecialChar   " => Special (Orange / Red)

  " keywords (mainly class / function definitions)
  HiLink phpStorageClass  StorageClass  " => Type (Green)
  HiLink phpSCKeyword     phpStorageClass

  HiLink phpStructure     Structure     " => Type (Green)
  HiLink phpStructureType phpStructure

  HiLink phpFCKeyword     phpDefine
  HiLink phpMagicClass    StorageClass
  if s:alt_comparisons
    HiLink phpInstanceof  phpRelation
  else
    HiLink phpInstanceof  phpMagicClass
  endif

  if s:show_quotes
    HiLink phpQuoteSingle   String
    HiLink phpQuoteDouble   String
  else
    HiLink phpQuoteSingle   Normal
    HiLink phpQuoteDouble   Normal
  endif

  " always highlight backtick quotes like an operator
  " (seeing as it executes stuff)
  HiLink phpQuoteBacktick phpOperator

  " built-in langauge functions / classes
  HiLink phpFunctions       Function        " => Identifier (Cyan) / Function (White)
  HiLink phpClasses         phpFunctions
  HiLink phpMethods         phpFunctions
  HiLink phpInterfaces      phpCoreConstant
  HiLink phpSpecialFunction SpecialComment  " => Special (Orange / Red)
  HiLink phpSpecialMethods  phpSpecialFunction

  " other items
  HiLink phpMemberError     Error
  HiLink phpParentError     Error
  HiLink phpHTMLError       Error
  HiLink phpOctalError      Error
  HiLink phpTodo            Todo

  " Peter Hodge June 17, 2006:
  " changed matchgroup for phpRegion from Delimiter to phpRegionDelimiter
  HiLink phpRegionDelimiter   Debug               " => Special (Orange / Red)

  " changed matchgroup for phpHereDoc to phpHereDocDelimiter
  HiLink phpHereDocDelimiter  phpRegionDelimiter  " => Special (Orange / Red)

  delcommand HiLink
endif

" optional support for PCRE extension (preg_* functions)
if s:show_pcre
    " ===================================================
    " Note: I have deliberately neglected to support the '\cx' functionality
    "       - it would do more harm than good by complicating this already-
    "       mind-numbing syntax file when nobody really needs this feature in
    "       PHP.
    " TODO: add support for '\cx' sequences (I changed my mind)

    " 1) Allow for dropping out of SQ and concatenating a variable {{{

      " flag a lone quote as an error!
      syn match pregError /'/ display contained containedin=pregPattern_S
      syn match pregError /"/ display contained containedin=pregPattern_D

      " find real concatenations (overrides the errors)
      syn region pregConcat matchgroup=phpQuoteSingle start=#'\ze\%(\%(\_s*\|\/\*.\{-}\*\/\|\/\/.*\n\)*\)\@>\.# end=/'/
        \ skip=/\['.\{-}'\]\|('.\{-}'[,)]/
        \ keepend extend
        \ contained containedin=pregPattern_S
        \ contains=@phpClExpressions
      syn region pregConcat matchgroup=phpQuoteDouble start=/"/ end=/"/
        \ skip=/\[".\{-}"\]\|(".\{-}"[,)]/
        \ keepend extend
        \ contained containedin=pregPattern_D
        \ contains=@phpClExpressions
    " }}}

    " 2) look for special characters {{{

      " TODO: re-examine how \$ is going to fit into a double-quoted string ...
      syn match pregSpecial /\$/ contained containedin=pregPattern_S display
      syn match pregSpecial /\$/ contained containedin=pregPattern_D display
        \ contains=phpIdentifierInString,phpIdentifierInStringComplex
      syn match pregSpecial /\^/ contained containedin=@pregPattern_Q display
      syn match pregSpecial /|/  contained containedin=@pregPattern_Q display
      syn match pregDot     /\./ contained containedin=@pregPattern_Q display

      " TODO: move these things out of here???
      " find a ] character at the start of a character range ...
      syn match pregClassIncStartBracket /\]/ contained display containedin=@pregClassIncStart_Q
      syn match pregClassExcStartBracket /\]/ contained display containedin=@pregClassExcStart_Q
      hi link pregClassIncStartBracket pregClassInc
      hi link pregClassExcStartBracket pregClassExc
    " }}}

    " 3) look for escape sequences {{{

      " look for any escape sequence inside the pattern and mark them as errors
      " by default, all escape sequences are errors
      " NOTE: adding 'display' to this next one can break the highlighting
      " (because it contains sequences such as \" which aren't supposed to end
      " the string)
      syn match pregEscapeUnknown /\\./ contained containedin=@pregPattern_Q

      " TODO: when \$ is encountered, the \ is a PHP escape and prevents
      " variable expansion, but the '$' becomes the end-of-line wildcard.
      " \\$ will match a literal '$', but the '$' might be part of a variable
      " name also. \\\$ is the proper way to match

      " TODO: deprecate these clusters?
      " TODO: deprecate pregClass_any
      syn cluster pregClass_any add=@pregClassInc,pregClassExc
      syn cluster pregClassRange_any_S add=pregClassIncRange_S,pregClassExcRange_S
      syn cluster pregClassRange_any_D add=pregClassIncRange_D,pregClassExcRange_D

      syn match pregClassEscapeUnknown /\\[^\^\-\]]/ contained containedin=@pregClass_any_Q display
      syn match pregClassEscape /\\[^a-zA-Z0-9]/ contained containedin=@pregClass_any_Q display extend

      " known escape sequences:
      syn match pregClassIncEscapeKnown /\C\\[abtnfret]/ contained display
            \ containedin=@pregClassInc_Q,@pregClassIncRange_Q
      syn match pregClassIncEscapeRange /\\[dsw]/ contained display
            \ containedin=@pregClassInc_Q,@pregClassIncRange_Q
      syn match pregClassExcEscapeKnown /\C\\[abtnfret]/ contained display
            \ containedin=@pregClassExc_Q,@pregClassExcRange_Q
      syn match pregClassExcEscapeRange /\\[dsw]/ contained display
            \ containedin=@pregClassExc_Q,@pregClassExcRange_Q

      " ... including hex sequences
      syn match pregClassIncEscapeKnown /\C\\x\x\{0,2}/ contained display
        \ containedin=@pregClassInc_Q,@pregClassIncRange_Q
      syn match pregClassExcEscapeKnown /\C\\x\x\{0,2}/ contained display
        \ containedin=@pregClassExc_Q,@pregClassExcRange_Q

      " ... and octal sequences
      syn match pregClassIncEscapeKnown /\\\o\{1,3}/ contained display
        \ containedin=@pregClassInc_Q,@pregClassIncRange_Q
      syn match pregClassExcEscapeKnown /\\\o\{1,3}/ contained display
        \ containedin=@pregClassExc_Q,@pregClassExcRange_Q

      syn match pregClassEscapeMainQuote /\\'/ contained transparent display contains=pregEscapePHP 
        \ containedin=@pregClass_any_S,@pregClassRange_any_S
      syn match pregClassEscapeMainQuote /\\"/ contained transparent display contains=pregEscapePHP 
        \ containedin=@pregClass_any_D,@pregClassRange_any_D

      syn match pregClassEscape /\\\\\ze\\'/ contained display
        \ containedin=@pregClass_any_S contains=pregEscapePHP
        \ nextgroup=pregClassEscapeMainQuote
      syn match pregClassEscape /\\\\\ze\\"/ contained display
        \ containedin=@pregClass_any_D contains=pregEscapePHP
        \ nextgroup=pregClassEscapeMainQuote

      syn match pregClassEscapeDouble1 /\\\\\ze\\\\/ contained containedin=@pregClass_any_Q display
        \ contains=pregEscapePHP
        \ nextgroup=pregClassEscapeDouble2
      syn match pregClassEscapeDouble2 /\\\\/ contained transparent display
        \ containedin=@pregClassRange_any_S,@pregClassRange_any_D
        \ contains=pregEscapePHP
      hi link pregClassEscapeDouble1 pregClassEscape

      " in the unknown escapes, match those that make a special character
      " take on its literal meaning (except for <single-quote> which is covered next)
      " NOTE: am changing these from being contained inside pregEscapeUnknown
      " to being in the main scope to make SQ and DQ containment easier
      syn match pregEscapeLiteral /\\[^A-Za-z0-9]/ contained containedin=@pregPattern_Q display
      syn match pregEscapeLiteral /\\\{4}/ contained containedin=@pregPattern_Q display

      " for single-quoted strings
      syn match pregEscapeLiteral /\\"/ contained containedin=pregPattern_S display
      syn match pregEscapeLiteral /\\\\\\'/ contained containedin=pregPattern_S display contains=pregEscapePHP

      " for double-quoted strings
      syn match pregEscapeLiteral /\\'/ contained containedin=pregPattern_D display
      syn match pregEscapeLiteral /\\\\\\"/ contained containedin=pregPattern_D display contains=pregEscapePHP

      syn match pregEscapeMainQuote /\\'/ contained containedin=pregPattern_S display
      syn match pregEscapeMainQuote /\\"/ contained containedin=pregPattern_D display

      " match the escaped strings which are known
      syn match pregBackreference /\\[1-9][0-9]\=/ contained containedin=pregEscapeUnknown display
      syn match pregEscapeSpecial /\C\\[rnt]/ contained containedin=pregEscapeUnknown display
      syn match pregEscapeSpecial /\C\\x\x\{0,2}/ contained containedin=pregEscapeUnknown display
      syn match pregEscapeSpecial /\\\%(0\o\{0,2}\|\o\o\o\)/ contained containedin=pregEscapeUnknown display
      syn match pregEscapeRange   /\\[wsd]/ contained containedin=pregEscapeUnknown display
      syn match pregEscapeAnchor  /\C\\[AbBGzZ]/ contained containedin=pregEscapeUnknown display

      " unicode characters
      syn match pregEscapeUnicode /\C\\X/ contained containedin=pregEscapeUnknown display
      syn match pregEscapeUnicodeError /\c\\p{\^\=\w\+}/ contained display
            \ containedin=pregEscapeUnknown,pregClassEscapeUnknown
      syn match pregEscapeUnicode /\\p{^\=/ contained containedin=pregEscapeUnicodeError display
      syn match pregEscapeUnicode /\CC[cfnos]\=/ contained containedin=pregEscapeUnicodeError display
      syn match pregEscapeUnicode /\CL[lmotu]\=/ contained containedin=pregEscapeUnicodeError display
      syn match pregEscapeUnicode /\CM[cen]\=/ contained containedin=pregEscapeUnicodeError display
      syn match pregEscapeUnicode /\CN[dlo]\=/ contained containedin=pregEscapeUnicodeError display
      syn match pregEscapeUnicode /\CP[cdefios]\=/ contained containedin=pregEscapeUnicodeError display
      syn match pregEscapeUnicode /\CS[ckmo]\=/ contained containedin=pregEscapeUnicodeError display
      syn match pregEscapeUnicode /\CZ[lps]\=/ contained containedin=pregEscapeUnicodeError display
      syn match pregEscapeUnicode /}/ contained containedin=pregEscapeUnicodeError display
      " shorthand
      syn match pregEscapeUnicode /\C\\[pP][CLMNPSZ]/ contained display
            \ containedin=pregEscapeUnknown,pregClassEscapeUnknown

      " match the PHP escaping in literal escapes
      syn match pregEscapePHP /\\./he=s+1 contained display containedin=pregEscapeMainQuote
      syn match pregEscapePHP /\\\\/he=s+1 contained display containedin=pregEscapeLiteral

      " this captures confusing usage of escape characters:
      " - need to make sure they don't capture the quote character because
      "   that wouldn't right
      syn match pregEscapeNotNeeded /\\\ze\\[^\\']/ contained display containedin=pregPattern_S,@pregClass_any_S
      syn match pregEscapeNotNeeded /\\\ze\\[^\\"]/ contained display containedin=pregPattern_D,@pregClass_any_D

      " a triple-backslash can be dangerous: it is not obvious that
      " the meaning of the 3rd backslash is dependent on the following
      " character; if the following character is changed to a
      " single-quote or backslash, it will change the meaning of the 3
      " backslashes
      syn match pregEscapeLiteral /\\\{3}\ze[^\\']/ contained display containedin=pregPattern_S
      syn match pregEscapeLiteral /\\\{3}\ze[^\\"]/ contained display containedin=pregPattern_D
      syn match pregClassEscape /\\\{3}\ze[^\\']/ contained display contains=pregClassEscapePHP containedin=@pregClass_any_S
      syn match pregClassEscape /\\\{3}\ze[^\\"]/ contained display contains=pregClassEscapePHP containedin=@pregClass_any_D
      syn match pregClassEscapePHP /\\\\/he=s+1 contained
      hi link pregClassEscapePHP  pregEscapePHP
    " }}}

    " 4) Look for quantifiers ?*+{1,2} {{{

      syn match pregQuantifier /\*?\=/ contained containedin=@pregPattern_Q display
      syn match pregQuantifier /+?\=/  contained containedin=@pregPattern_Q display
      syn match pregQuantifier /??\=/  contained containedin=@pregPattern_Q display

      syn match pregQuantifierComplex /{\d\+\(,\d*\)\=}/ contained containedin=@pregPattern_Q display
      syn match pregQuantifierComplex /{,\d\+}/ contained containedin=@pregPattern_Q display
      syn match pregQuantifier /\d\+/ contained containedin=pregQuantifierComplex display
    " }}}

    " 5) Look for sub-patterns {{{
      syn match pregParens /(/ contained containedin=@pregPattern_Q display
      syn match pregParens /(?<[=!]/ contained containedin=@pregPattern_Q display extend
      syn match pregParens /(?[:>=!]/ contained containedin=@pregPattern_Q display extend
      syn match pregParens /(?(?<\=[=!]/ contained containedin=@pregPattern_Q display extend

      " recursion
      syn match pregParens /(?R)/ contained containedin=@pregPattern_Q display extend
      syn match pregParens /(?[1-9]\d\=)/ contained containedin=@pregPattern_Q display extend
            \ contains=pregBackreferenceNumber

      " conditional sub-patterns
      syn match pregParens /(?(\d\+)/ contained containedin=@pregPattern_Q display
            \ contains=pregBackreferenceNumber
      syn match pregBackreferenceNumber /\d\+/ contained display
      " TODO: move hi link out of here?
      hi link pregBackreferenceNumber pregBackreference
      syn match pregParens /(?\a\+\(-\a\+\)\=[):]/ contained containedin=@pregPattern_Q display
        \ contains=pregOption
      syn match pregParens /(?-\a\+[):]/ contained containedin=@pregPattern_Q display
        \ contains=pregOption
      syn match pregParens /)/ contained containedin=@pregPattern_Q display

      " find a named backreference
      syn match pregBackreference contained containedin=@pregPattern_Q /(?P>\w\+)/ display
            \ contains=pregNamedBackreference
      syn match pregParens contained containedin=@pregPattern_Q /(?P<\w\+>/ display
            \ contains=pregNamedBackreference

      syn match pregNamedBackreference /(?P>\zs\w\+\ze)/ contained display
      syn match pregNamedBackreference /(?P<\zs\w\+\ze>/ contained display
      hi link pregNamedBackreference pregEscapeRange
    " }}}

    " 6) Look for PCRE patterns {{{
      syn cluster phpClFunctions add=phpPREGFunctions

      " look for preg_* functions which take a single pattern
      syn keyword phpPREGFunctions contained preg_match preg_match_all preg_split preg_grep
            \ nextgroup=phpPREGOpenParent,phpPREGRegion

      " special case for preg_replace functions which can take an array of
      " patterns
      syn keyword phpPREGFunctions contained preg_replace preg_replace_callback
            \ nextgroup=phpPREGOpenParentMulti,phpPREGRegionMulti skipwhite skipempty

      if s:strict_blocks
        " regions for ( ) after name of preg_* function
        syn region phpPREGRegion matchgroup=phpParent start=/(/ end=/)/ keepend extend
              \ contained contains=@phpClExpressions,phpPREGStringStarter
        syn region phpPREGRegionMulti matchgroup=phpParent start=/(/ end=/)/ keepend extend
              \ contained contains=@phpClExpressions,phpPREGStringStarter,phpPREGArray

        " match an array of preg patterns
        if s:alt_arrays
          syn region phpPREGArray matchgroup=phpArrayParens start=/\%((\_s*\)\@<=array\_s*(/ end=/)/
                \ keepend extend
                \ contained
                \ contains=@phpClExpressions,phpPREGStringStarter,phpPREGArrayComma,phpPREGArrayComment
        else
          syn match phpPREGArray /\%((\_s*\)\@<=array/ contained
                \ nextgroup=phpPREGArrayRegion skipwhite skipempty
          
          syn region phpPREGArrayRegion matchgroup=phpParent start=/(/ end=/)/
                \ keepend extend
                \ contained
                \ contains=phpPREGStringStarter,phpPREGArrayComment,phpPREGArrayComma
        endif
        hi link phpPREGArray phpArray

        " a special match to open a pattern string immediately after a '('
        " TODO: will this work as a match instead?
"        syn region phpPREGStringStarter start=/\%((\_s*\)\@<=\z(['"]\)/ end=/\z1/ extend 
"              \ contained contains=@phpPREGString_any
        syn match phpPREGStringStarter /\%((\_s*\)\@<=['"]/ extend 
              \ contained contains=@phpPREGString_any

        " TODO: move 'hi link' commands out of here
        hi link phpPREGArrayComma phpArrayComma
      else
        " highlight the opening parenthesis
        syn match phpPREGOpenParent /(/ contained nextgroup=@phpPREGString_any display
        hi link phpPREGOpenParent phpParent
        syn match phpPREGOpenParentMulti /(/ contained display
              \ nextgroup=@phpPREGString_any,phpPREGArray skipwhite skipnl skipempty
        hi link phpPREGOpenParentMulti phpPREGOpenParent

        " TODO: move 'hi link' commands out of here
        " match an array of preg patterns
        syn keyword phpPREGArray array contained nextgroup=phpPREGArrayOpenParent
        hi link phpPREGArray phpType
        syn match phpPREGArrayOpenParent /(/ contained display
          \ nextgroup=@phpPREGArrayString_any skipwhite skipnl skipempty
        hi link phpPREGArrayOpenParent phpPREGOpenParent
      endif

      " match a phpString (single or double-quoted) which is able to contain a
      " pregPattern
      " NOTE: we can only error on comma-ending as long as the delimiter is
      " not a comma!!!
      syn cluster phpPREGString_any add=phpPREGStringSingle,phpPREGStringDouble
      syn region phpPREGStringSingle matchgroup=phpQuoteSingle start=/'\ze\z(.\)/ end=/'/
        \ keepend extend contained contains=pregPattern_S
        \ matchgroup=Error end=/\z1\@!,/
      syn region phpPREGStringDouble matchgroup=phpQuoteSingle start=/"\ze\z(.\)/ end=/"/
        \ keepend extend contained contains=pregPattern_D
        \ matchgroup=Error end=/\z1\@!,/

      " match a single-quoted string inside an array, followed by a comma
      " and another string
      " TODO: remove hi link commands from here
      syn cluster phpPREGArrayString_any add=phpPREGArrayStringSingle,phpPREGArrayStringDouble
      syn region phpPREGArrayStringSingle matchgroup=phpQuoteSingle start=/'/ end=/'/
        \ keepend extend contained contains=pregPattern_S
        \ nextgroup=phpPREGArrayComma skipwhite skipnl skipempty
      hi link phpPREGArrayStringSingle phpPREGStringSingle
      syn region phpPREGArrayStringDouble matchgroup=phpQuoteDouble start=/"/ end=/"/
        \ keepend extend contained contains=pregPattern_D
        \ nextgroup=phpPREGArrayComma skipwhite skipnl skipempty
      hi link phpPREGArrayStringDouble phpPREGStringDouble

      " use the comma inside a pattern array to trigger off the next pattern
      syn match phpPREGArrayComma /,/ contained
            \ nextgroup=@phpPREGArrayString_any skipwhite skipnl skipempty

      " use the comments inside a pattern array to trigger off the next pattern
      syn region phpPREGArrayComment start=#//# end=#$# contained keepend extend contains=@Spell
            \ nextgroup=@phpPREGArrayString_any skipwhite skipnl skipempty
      syn region phpPREGArrayComment start=#/\*# end=#\*/# contained keepend extend contains=@Spell
            \ nextgroup=@phpPREGArrayString_any skipwhite skipnl skipempty
      hi link phpPREGArrayComment phpComment
    " }}}

    " 7) Look for pattern delimiters {{{
      syn cluster pregPattern_Q add=pregPattern_S,pregPattern_D

      " add a region which starts with any valid delimiter character
      " and ends when the delimiter character is met again
      syn region pregPattern_S matchgroup=pregDelimiter
        \ start=/\z([ !"#$%&*+,-./:;=?@^_`|~]\)/ start=/\z(\\'\)/
        \ end=/\z1/ skip=/\\\\\{2,3}\|\\\\\z1\=\|\\\z1/ keepend extend
        \ contained nextgroup=pregOptionError_S
        \ contains=pregCommentMultiline
      " repeat above command, but this time instead of the multi-line comment,
      " make it 'oneline'
      syn region pregPattern_S matchgroup=pregDelimiter
        \ start=/\z([ !"#$%&*+,-./:;=?@^_`|~]\)/ start=/\z(\\'\)/
        \ end=/\z1/ skip=/\\\\\{2,3}\|\\\\\z1\=\|\\\z1/ keepend extend
        \ contained nextgroup=pregOptionError_S
        \ oneline

      function! s:pregPattern_S(open, close)
        execute 'syntax region pregPattern_S matchgroup=pregDelimiter'
              \ 'start=/' . a:open . '/'
              \ 'end=/' . a:close . '/'
              \ 'skip=/\\\{4}\|\\\\\=./ keepend extend'
              \ 'contained nextgroup=pregOptionError_S'
      endfunction
      function! s:pregPattern_D(open, close)
        execute 'syntax region pregPattern_D matchgroup=pregDelimiter'
              \ 'start=/' . a:open . '/'
              \ 'end=/' . a:close . '/'
              \ 'skip=/\\\{4}\|\\\\\=./ keepend extend'
              \ 'contained nextgroup=pregOptionError_D'
      endfunction
      call s:pregPattern_S('(', ')')
      call s:pregPattern_S('<', '>')
      call s:pregPattern_S('\[', '\]')
      call s:pregPattern_S('{', '}')
      call s:pregPattern_D('(', ')')
      call s:pregPattern_D('<', '>')
      call s:pregPattern_D('\[', '\]')
      call s:pregPattern_D('{', '}')

      " TODO: make a cluster for the things which go inside double-quoted
      " strings!
      syn region pregPattern_D matchgroup=pregDelimiter
        \ start=/\z([ !'#$%&*+,-./:;=?@^_`|~]\)/ start=/\z(\\"\)/
        \ end=/\z1/ skip=/\\\\\{2,3}\|\\\\\z1\=\|\\\z1/
        \ keepend extend
        \ contained nextgroup=pregOptionError_D
        \ contains=phpIdentifierInString,phpIdentifierInStringComplex,pregCommentMultiline
      " repeat above command, but this time instead of the multi-line comment,
      " make it 'oneline'
      syn region pregPattern_D matchgroup=pregDelimiter
        \ start=/\z([ !'#$%&*+,-./:;=?@^_`|~]\)/ start=/\z(\\"\)/
        \ end=/\z1/ skip=/\\\\\{2,3}\|\\\\\z1\=\|\\\z1/
        \ keepend extend
        \ contained nextgroup=pregOptionError_D
        \ contains=phpIdentifierInString,phpIdentifierInStringComplex
        \ oneline

      " TODO: work out how to have '$' as delimiter in a double-quoted string
"      syn region pregPattern_D matchgroup=pregDelimiter
"        \ start=/\\\$\|\$[a-z_]\@!\%({[a-z_$]\)\@!/
"        \ end=/\\\$\|\$[a-z_]\@!\%({[a-z_$]\)\@!/ skip=/\\\{4}\|\\\{3}[^$]\|\\\\\$/
"        \ keepend extend
"        \ contained nextgroup=pregOptionError_D
"        \ contains=phpIdentifierInString,phpIdentifierInStringComplex

      " TODO move hi link out of here
      hi link pregPattern_S pregPattern
      hi link pregPattern_D pregPattern
    " }}}

    " 8) Look for character classes {{{
      " Inc[lusive] and Exc[lusive] character classes:
      "  if the first char is ']'
      " that is tricky so is handled by another match below
      syn cluster pregClassInc_Q add=pregClassInc_S,pregClassInc_D
      syn cluster pregClassExc_Q add=pregClassExc_S,pregClassExc_D
      syn cluster pregClass_any_S add=pregClassInc_S,pregClassExc_S
      syn cluster pregClass_any_D add=pregClassInc_D,pregClassExc_D
      syn cluster pregClass_any_Q add=@pregClassInc_Q,@pregClassExc_Q

      " TODO: does that 'skip' need to be copied to the line below?
      syn region pregClassInc_S matchgroup=pregClassParent start=/\[\ze[^\^\]]/ end=/\]/ skip=/\\\%(\\\\\]\)\@!\&\\./
            \ keepend display contained containedin=pregPattern_S
      syn region pregClassInc_D matchgroup=pregClassParent start=/\[\ze[^\^\]]/ end=/\]/ skip=/\\./
            \ keepend display contained containedin=pregPattern_D
      " TODO: move these out of here???
      hi link pregClassInc_S pregClassInc
      hi link pregClassInc_D pregClassInc
      hi link pregClassExc_S pregClassExc
      hi link pregClassExc_D pregClassExc

      syn region pregClassExc_S matchgroup=pregClassParent start=/\[\^\]\@!/ end=/\]/ skip=/\\./
        \ keepend display contained containedin=pregPattern_S
      syn region pregClassExc_D matchgroup=pregClassParent start=/\[\^\]\@!/ end=/\]/ skip=/\\./
        \ keepend display contained containedin=pregPattern_D

      " TODO: move hi link commands out of here

      " TODO: just use one match for all character classes???
      " this is an alternate form of the character class region,
      " it is not contained in @pregPattern_Q and can only be activated
      " by a nextgroup=pregClassInc.
      " 'EXECUTE'ed:
      "syntax region pregClassInc_S start=/\ze./ matchgroup=pregClassParent end=/\]/ skip=/\\\\\|\\]/ contained display
      "syntax region pregClassInc_D start=/\ze./ matchgroup=pregClassParent end=/\]/ skip=/\\\\\|\\]/ contained display
      "syntax region pregClassExc_S start=/\ze./ matchgroup=pregClassParent end=/\]/ skip=/\\\\\|\\]/ contained display
      "syntax region pregClassExc_D start=/\ze./ matchgroup=pregClassParent end=/\]/ skip=/\\\\\|\\]/ contained display
      let s:command = 'syntax region pregClass<TYPE> start=/\ze./ matchgroup=pregClassParent end=/\]/'
        \ . ' skip=/\\\\\|\\]/ contained display keepend'
      execute substitute(s:command, '<TYPE>', 'Inc_S', 'g')
      execute substitute(s:command, '<TYPE>', 'Inc_D', 'g')
      execute substitute(s:command, '<TYPE>', 'Exc_S', 'g')
      execute substitute(s:command, '<TYPE>', 'Exc_D', 'g')
      unlet! s:command

      " this is a special match to start off the character class
      " region when the very first character inside it is ']',
      " because otherwise the character class region would end
      " immediately
      syn cluster pregClassIncStart_Q add=pregClassIncStart_S,pregClassIncStart_D
      syn cluster pregClassExcStart_Q add=pregClassExcStart_S,pregClassExcStart_D
      let s:command = 'syntax match pregClassIncStart_<QUOTE> /\[\]/ contained display'
        \ . ' containedin=pregPattern_<QUOTE> nextgroup=pregClassInc_<QUOTE>,pregClassIncEnd'
      execute substitute(s:command, '<QUOTE>', 'S', 'g')
      execute substitute(s:command, '<QUOTE>', 'D', 'g')
      let s:command = 'syntax match pregClassExcStart_<QUOTE> /\[\^\]/ contained display'
        \ . ' containedin=pregPattern_<QUOTE> nextgroup=pregClassExc_<QUOTE>,pregClassExcEnd'
      execute substitute(s:command, '<QUOTE>', 'S', 'g')
      execute substitute(s:command, '<QUOTE>', 'D', 'g')
      unlet! s:command

      " TODO: move hi link commands out of here
      hi link pregClassIncStart_S pregClassParent
      hi link pregClassIncStart_D pregClassParent
      hi link pregClassExcStart_S pregClassParent
      hi link pregClassExcStart_D pregClassParent

      " this is a special match to end off the character class immediately
      " should a ']' be followed immediately by another ']'
      " TODO: move hi link commands out of here
      syn match pregClassIncEnd /\]/ contained display
      hi link pregClassIncEnd pregClassParent
      syn match pregClassExcEnd /\]/ contained display
      hi link pregClassExcEnd pregClassParent

      " add the range-matching string here
      syn cluster pregClassIncRange_Q add=pregClassIncRange_S,pregClassIncRange_D
      syn cluster pregClassExcRange_Q add=pregClassExcRange_S,pregClassExcRange_D
      syn match pregClassIncRange_S contained display
        \ containedin=pregClassInc_S,pregClassIncStart_S
        \ /\%([^\\]\|\\\%(\\\{2}[\\']\=\|x\x\{0,2}\|\o\{1,3}\|[^dsw]\)\)-\%(\\\{3,4}\|\\[^dsw]\|[^\\\]]\)/
      syn match pregClassIncRange_D contained display
        \ containedin=pregClassInc_D,pregClassIncStart_D
        \ /\%([^\\]\|\\\%(\\\{2}[\\"]\=\|x\x\{0,2}\|\o\{1,3}\|[^dsw]\)\)-\%(\\\{3,4}\|\\[^dsw]\|[^\\\]]\)/
      syn match pregClassExcRange_S contained display
        \ containedin=pregClassExc_S,pregClassExcStart_S
        \ /\%([^\\]\|\\\%(\\\{2}[\\']\=\|x\x\{0,2}\|\o\{1,3}\|[^dsw]\)\)-\%(\\\{3,4}\|\\[^dsw]\|[^\\\]]\)/
      syn match pregClassExcRange_D contained display
        \ containedin=pregClassExc_D,pregClassExcStart_D
        \ /\%([^\\]\|\\\%(\\\{2}[\\']\=\|x\x\{0,2}\|\o\{1,3}\|[^dsw]\)\)-\%(\\\{3,4}\|\\[^dsw]\|[^\\\]]\)/
      hi link pregClassIncRange_S pregClassIncRange
      hi link pregClassIncRange_D pregClassIncRange
      hi link pregClassExcRange_S pregClassExcRange
      hi link pregClassExcRange_D pregClassExcRange

      " what about the pre-defined sets using [:space:]?
      syn region pregClassSetRegion matchgroup=pregClassSet start=/\[:/ end=/:\]/
            \ extend keepend
            \ contained containedin=@pregClass_any_Q contains=pregClassSet
      hi link pregClassSetRegion Error
      syn keyword pregClassSet contained
            \ alnum digit punct
            \ alpha graph space
            \ blank lower upper
            \ cntrl print xdigit
      hi link pregClassSet pregEscapeRange

      " highlighted a lone single/double quote as an error
      syn match pregClassQuoteError contained display /'/ containedin=@pregClass_any_S
      syn match pregClassQuoteError contained display /"/ containedin=@pregClass_any_D
      hi link pregClassQuoteError Error

    " }}}
      
    " 9) Look for escaping using \Q and \E {{{
      syn region pregNonSpecial_S matchgroup=pregParens start=/\C\\Q/ end=/\C\\E/
            \ contained containedin=pregPattern_S
      syn region pregNonSpecial_D matchgroup=pregParens start=/\C\\Q/ end=/\C\\E/
            \ contained containedin=pregPattern_D
      hi link pregNonSpecial_S pregNonSpecial
      hi link pregNonSpecial_D pregNonSpecial
      hi link pregNonSpecial pregPattern

	  " I'm just going to rebuild escapes here to make it easier
	  syn match pregError /'/ contained containedin=pregNonSpecial_S display
	  syn match pregError /"/ contained containedin=pregNonSpecial_D display
	  syn match pregNonSpecialEscape /\\['\\]/ contained containedin=pregNonSpecial_S display
	  syn match pregNonSpecialEscape /\\["\\$]/ contained containedin=pregNonSpecial_D display
	  syn match pregNonSpecialEscapePHP /\\./he=s+1 contained containedin=pregNonSpecialEscape display
	  syn match pregNonSpecialEscapePHP /\\[rnt]/ contained containedin=pregNonSpecial_D display
      hi link pregNonSpecialEscapePHP pregEscapePHP
    " }}}

    " 10) Match PCRE pattern options {{{
      syn match pregOptionError_S /\%(\\[\\']\|[^']\)\+/ contained contains=pregOption display
      syn match pregOptionError_D /\%(\\[\\"]\|[^"]\)\+/ contained display
            \ contains=pregOption,phpIdentifierInString,phpIdentifierInStringComplex
      syn match pregOption /\C[eimsuxADSUX]\+/ contained display
      " TODO: move hi links out of here?
      hi link pregOptionError_S pregOptionError
      hi link pregOptionError_D pregOptionError
    " }}}

    " 11) PCRE pattern comments {{{
      syn match pregComment /\v\(\?\#[^)]*\)/ contained containedin=@pregPattern_Q contains=@Spell

      " TODO: multi-line comments must be turned on explicitly!?
      " syntax match pregComment /\v\#(.*)@>/ contained containedin=@pregPattern_Q
"      if exists('b:php_preg_multiline')
        syntax match pregCommentMultiline /\#\(.*\)\@>/ contained contains=@Spell
        hi! link pregCommentMultiline pregComment
"      endif
    " }}}

    " 12) highlight links {{{
      command -nargs=+ HiLink hi link <args>

      HiLink phpPREGFunctions phpFunctions
      HiLink phpPREGOpenParent phpParent
      HiLink phpPREGStringSingle phpStringSingle
      HiLink phpPREGStringDouble phpStringDouble

      HiLink pregError Error
      HiLink pregAmbiguous Todo

      HiLink pregDelimiter Statement

      HiLink pregOptionError Error
      HiLink pregOption Type

      HiLink pregComment phpComment

      HiLink pregEscapeDelimiter pregDelimiter
      HiLink pregEscapeUnknown pregAmbiguous
      HiLink pregEscapeLiteral Comment
      HiLink pregEscapeSpecial Number
      HiLink pregEscapeAnchor  Typedef
      HiLink pregEscapeRange   Identifier
      HiLink pregEscapePHP	   phpSpecialChar
      HiLink pregEscapeUnicode pregEscapeRange
      HiLink pregEscapeUnicodeError pregError

      HiLink pregEscapeNotNeeded pregEscapePHP

      HiLink pregPattern	Normal
      HiLink pregSpecial	Typedef
      HiLink pregDot        Typedef
      HiLink pregParens	PreProc
      HiLink pregBackreference pregParens

      HiLink pregQuantifier        Typedef
      HiLink pregQuantifierComplex Typedef

      HiLink pregClassParent  pregParens
      HiLink pregClassInc     pregClassParent
      HiLink pregClassExc     pregClassParent
      HiLink pregClassIncRange Identifier
      HiLink pregClassExcRange Identifier
      HiLink pregClassEscape   Comment
      HiLink pregClassIncEscapeKnown pregEscapeSpecial
      HiLink pregClassIncEscapeRange pregClassIncRange
      HiLink pregClassExcEscapeKnown Type
      HiLink pregClassExcEscapeRange pregClassExcRange
      HiLink pregClassEscapeUnknown pregAmbiguous

      delcommand HiLink
    " }}}
endif

" ================================================================

let b:current_syntax = "php"

if main_syntax == 'php'
  unlet main_syntax
endif

" vim: sw=2 sts=2 et fdm=marker fdc=1
