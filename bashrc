	# =============================================================== #
#
# PERSONAL $HOME/.bashrc FILE for bash-3.0 (or later)
# By Emmanuel Rouat [no-email]
# Example URL : http://tldp.org/LDP/abs/html/sample-bashrc.html
# Last modified: Tue Nov 20 22:04:47 CET 2012

#  This file is normally read by interactive shells only.
#+ Here is the place to define your aliases, functions and
#+ other interactive features like your prompt.
#
#  The majority of the code here assumes you are on a GNU
#+ system (most likely a Linux box) and is often based on code
#+ found on Usenet or Internet.
#
#  See for instance:
#  http://tldp.org/LDP/abs/html/index.html
#  http://www.caliban.org/bash
#  http://www.shelldorado.com/scripts/categories.html
#  http://www.dotfiles.org
#
#  The choice of colors was done for a shell with a dark background
#+ (white on black), and this is usually also suited for pure text-mode
#+ consoles (no X server available). If you use a white background,
#+ you'll have to do some other choices for readability.
#
#  This bashrc file is a bit overcrowded.
#  Remember, it is just just an example.
#  Tailor it to your needs.
#
# =============================================================== #

# --> Comments added by HOWTO author.

# If not running interactively, don't do anything
[ -z "$PS1" ] && return


#-------------------------------------------------------------
# Source global definitions (if any)
#-------------------------------------------------------------


if [ -f /etc/bashrc ]; then
      . /etc/bashrc   # --> Read /etc/bashrc, if present.
fi

#-------------------------------------------------------------
# Some settings
#-------------------------------------------------------------

#set -o nounset     # These  two options are useful for debugging.
#set -o xtrace
alias debug="set -o nounset; set -o xtrace"

ulimit -S -c 0      # Don't want coredumps.
set -o notify
set -o noclobber
set -o ignoreeof


# Enable options:
shopt -s cdspell
shopt -s cdable_vars
shopt -s checkhash
shopt -s checkwinsize
shopt -s sourcepath
shopt -s no_empty_cmd_completion
shopt -s cmdhist
shopt -s histappend histreedit histverify
shopt -s extglob       # Necessary for programmable completion.

# Disable options:
shopt -u mailwarn
unset MAILCHECK        # Don't want my shell to warn me of incoming mail.


#==========================================================
#
#  COMMAND PROMPT
#==========================================================

PS1="(\[\e[33m\]\u\[\e[0m\]@\h:\[\e[36m\][\w]:\[\e[0m\])
 -> "


#============================================================
#
#  ALIASES AND FUNCTIONS
#
#  Arguably, some functions defined here are quite big.
#  If you want to make this file smaller, these functions can
#+ be converted into scripts and removed from here.
#
#============================================================

#-------------------
# Personnal Aliases
#-------------------

alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
# -> Prevents accidentally clobbering files.
alias mkdir='mkdir -p'

alias h='history'
alias j='jobs -l'
alias which='type -a'
alias ..='cd ..'

# Pretty-print of some PATH variables:
alias path='echo -e ${PATH//:/\\n}'
alias libpath='echo -e ${LD_LIBRARY_PATH//:/\\n}'


alias du='du -kh'    # Makes a more readable output.
alias df='df -kTh'

#-------------------------------------------------------------
# The 'ls' family (this assumes you use a recent GNU ls).
#-------------------------------------------------------------
# Add colors for filetype and  human-readable sizes by default on 'ls':
alias ls='ls -hG'
alias lk='ls -lSr'         #  Sort by size, biggest last.
alias lt='ls -ltr'         #  Sort by date, most recent last.
alias lc='ls -ltcr'        #  Sort by/show change time,most recent last.
alias lu='ls -ltur'        #  Sort by/show access time,most recent last.

# The ubiquitous 'll': directories first, with alphanumeric sorting:
alias l="ls -l"
alias ll="ls -lv"
alias lm='ll |more'        #  Pipe through 'more'
alias lr='ll -R'           #  Recursive ls.
alias la='ll -A'           #  Show hidden files.


#--------------------------------------------------------------
# Git stuff
#--------------------------------------------------------------

alias gs='git status'		# git status
alias gc='git commit -am'	# git commit with message
alias g='git'				# git shortcut


