# .bashrc

# Avoid init files other than ~/.bashrc.  When there is more than one, the
# rules for which one is used and when are complicated and inconsistently
# supported across environments.

#--------------------------------
# Login Settings
#--------------------------------

if [[ -z "$BASHRC_ONCE" ]] ; then
    export BASHRC_ONCE=1

    [[ -r /etc/bashrc ]] && . /etc/bashrc

    export P4CONFIG=.p4
    if [[ "${OSTYPE:0:6}" = darwin ]]; then
        PATH="/usr/local/bin:$PATH"  # MacPorts
    fi
    PATH="$HOME/local/bin:$PATH"

    # host-specific login settings
    [[ -r ~/.bashrc-local ]] && . ~/.bashrc-local

    # BASH_ENV may be set by emacs to ensure .bashrc is run on subcommands.
    # Clear it now to make emacs sub-shells look more like other shells.
    export -n BASH_ENV
fi

if [[ -z "$PS1" ]] ; then
   return
fi

#--------------------------------
# Interactive Settings
#--------------------------------

if [[ "$TERM" == "dumb" && -n "$INSIDE_EMACS" ]] ; then
    # git: Set TERM='' to get colors (which Emacs comint handles fine).
    export TERM=''
    export PAGER=cat
    # Rely on emacs for command line editing.
    export NODE_NO_READLINE=1
else
    export EDITOR=emacs
fi

PS1='\w] '

if [[ -n "$SSH_CLIENT" ]]; then
  PS1='['"$HOSTNAME"'] \w: '
fi

alias ls="ls -F"

# recycle
re() {
  mv "$@" ~/.Trash/
}

pathedit() {
    echo "$PATH" | tr : '\n' > /tmp/path.txt
    ${EDITOR} /tmp/path.txt
    export PATH=$(cat /tmp/path.txt | tr '\n' :)
}

# cds: cd to saved directory by selecting from a list.  Directories
#    are preserved across shell sessions.
#
cds() {
  local a b
  touch ~/.cds
  if [[ -z "$1" ]] ; then
    # no arg: list
    n=0
    while read a && (( n < 9 )) ; do
      echo "$((++n)) $a"
    done < ~/.cds
    echo 'Use -v to see variables'
  elif [[ "$1" == "-v" ]]; then
    IFS='	'
    while read b a ; do
      echo "$b = $a"
    done < ~/.cdvars
  elif [[ -z "${1/[0-9]*/}" ]] ; then
    # numeric arg: retrieve & cd
    n="$1"
    while (( n > 0 )) ; do (( --n )) ; read a ; done < ~/.cds
    'cd' "$a"
  elif [[ -d "${1}" ]] ; then
    # directory arg: cd & save / move to top
    'cd' "$1"
    d=`pwd`
    echo "$d" > ~/.cds.new
    while read a ; do
      if [[ "$a" != "$d" ]] ; then echo "$a" ; fi
    done < ~/.cds >> ~/.cds.new
    mv ~/.cds.new ~/.cds
  elif [[ "$1" == "-e" ]] ; then
    $EDITOR ~/.cds
  elif [[ "$1" == "-ev" ]] ; then
    $EDITOR ~/.cdvars
  elif [[ "$1" == "-h" ]] ; then
    echo 'Usage:'
    echo '  cds        List saved directories'
    echo '  cds <n>    cd to saved dir #n'
    echo '  cds <dir>  cd to dir and add to list'
  else
    # else: look for match
    IFS='	'
    while read b a ; do
      if [[ "$b" = "$1" ]] ; then break ; fi
    done < ~/.cdvars
    if [[ -z "$a" ]] ; then
      while read a ; do
        if [[ -z "${a/*$1*/}" ]] ; then break ; fi
      done < ~/.cds
    fi
    if [[ -z "$a" ]] ; then
       echo "cds: not a directory: $1;  try 'cds -h' for help, or one of these:"
       cds
    else
       'cd' "$a"
    fi
  fi
}

# host-specific interactive settings
[[ -r ~/.bashrc-local-i ]] && . ~/.bashrc-local-i
