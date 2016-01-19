# .bashrc

# Decades of evolution have made the rules for shell startup file processing
# a tangled web.
#
# Bash chooses .bash_profile over .bashrc in the following cases:
#   OSX terminal windows
#   Linux interactive login shells (via SSH, console)
#
# Bash chooses .bashrc over .bash_profile in the following cases:
#   Cygwin terminal windows
#   Linux SSH command execution
#   Non-interactive shells
#
# In order to guarantee a functioning environment, we rely only on the
# precedence rules we implement ourselves.  We put all logic in bashrc and
# have .bash_profile do nothing but source .bashrc.
#
# BASHRC_ONCE guards environment variable settings.  It is inherited by child
#     shells along with other environment variables.
#
# PS1 guards interactive shell settings (aliases, etc.) since it should be
#     unset in non-interactive shells.
#

#--------------------------------
# Environment Variables
#--------------------------------

if [ -z "$BASHRC_ONCE" ] ; then
    export BASHRC_ONCE=1

    # When run from Cygwin, Mercurial does not recognize Win32 paths in any of
    # TMPDIR, TEMP, or TMP.  It passes the first valid Cygwin path to the
    # editor.  To work with Windows editors, the temporary directory name must
    # mean the same thing in both Cygwin and Windows environments
    # (e.g. "c:\tmp" mounted on Cygwin's "/tmp").

    export EDITOR="${EDITOR:-emacsclient}"
    export ALTERNATE_EDITOR=emacs
    export P4CONFIG=.p4

    if [ -f /etc/bashrc ]; then
    	. /etc/bashrc
    fi

    if [ "${OSTYPE:0:6}" = darwin ]; then
        export PATH="$HOME/local/bin:/usr/local/bin:$PATH"
	export MANPATH="/opt/local/share/man:$MANPATH"
        for P in ~/adt-bundle-mac/sdk/platform-tools \
                 ~/git/depot_tools \
                 /usr/local/bin \
                 $HOME/local/bin \
                 $HOME/bin \
                 $HOME/bin/osx
        do
           if [ -d "$P" ] ; then PATH="$P:$PATH" ; fi
        done
    elif [ "$OSTYPE" = "cygwin" ]; then
        export TMPDIR="/tmp"
        export PATH="$HOME/local/bin:$PATH"
    elif [ "${OSTYPE:0:5}" = "linux" ]; then
        export PATH="$HOME/local/bin:$PATH"
    fi

    # host-specific environment settings

    if [ -f ~/.bashrc-local ]; then
	. ~/.bashrc-local
    fi
fi


#--------------------------------
# Every time
#--------------------------------
if [ "$OSTYPE" = "cygwin" ]; then
  # Ignore CRs, and make sure this is set for every shell.
  # Set BASH_ENV to ensure this gets set when running shell scripts.
  # BASH_ENV is not already set when bash is invoked via SSH on Cygwin.
  set -o igncr
  export BASH_ENV="$HOME/.bashrc"
fi

#--------------------------------
# Interactive Shell Settings
#--------------------------------

if [ -z "$PS1" ] ; then
   return
fi

PS1='\w] '

if [ -n "$SSH_CLIENT" ] ; then
  PS1='['"$HOSTNAME"'] \w: '
fi

if [ "$OSTYPE" = "cygwin" ]; then
  # Cygwin EDITOR: depends on SSH and emacs shell
  if [ -n "$SSH_CLIENT" ] ; then
    if [ "$EDITOR" != "emacsclient" ] ; then
      export EDITOR=/usr/bin/emacs
    fi
    alias emacs="$EDITOR"
  fi
  export P4EDITOR="$EDITOR"
fi

alias ls="ls -GF"

# recycle
re() {
  mv "$@" ~/.Trash/
}

pathedit() {
  echo "$PATH" | lua -e 'io.write((io.read("*a"):gsub(":","\n")))' > /tmp/path.txt
  ${EDITOR} /tmp/path.txt
  export PATH=`lua -e 'io.write((io.read("*a"):gsub("\n",":")))' < /tmp/path.txt`
}

# cds: cd to saved directory by selecting from a list.  Directories
#    are preserved across shell sessions.
#
cds() {
  touch ~/.cds
  if [ -z "$1" ] ; then
    # no arg: list
    n=0
    while read a && (( n < 9 )) ; do
      echo "$((++n)) $a"
    done < ~/.cds
    echo 'Use -v to see variables'
  elif [ "$1" == "-v" ]; then
    IFS='	'
    while read b a ; do
      echo "$b = $a"
    done < ~/.cdvars
  elif [ -z "${1/[0-9]*/}" ] ; then
    # numeric arg: retrieve & cd
    n="$1"
    while (( n > 0 )) ; do (( --n )) ; read a ; done < ~/.cds
    'cd' "$a"
  elif [ -d "${1}" ] ; then
    # directory arg: cd & save / move to top
    'cd' "$1"
    d=`pwd`
    echo "$d" > ~/.cds.new
    while read a ; do
      if [ "$a" != "$d" ] ; then echo "$a" ; fi
    done < ~/.cds >> ~/.cds.new
    mv ~/.cds.new ~/.cds
  elif [ "$1" == "-e" ] ; then
    $EDITOR ~/.cds
  elif [ "$1" == "-ev" ] ; then
    $EDITOR ~/.cdvars
  elif [ "$1" == "-h" ] ; then
    echo 'Usage:'
    echo '  cds        List saved directories'
    echo '  cds <n>    cd to saved dir #n'
    echo '  cds <dir>  cd to dir and add to list'
  else
    # else: look for match
    IFS='	'
    while read b a ; do
      if [ "$b" = "$1" ] ; then break ; fi
    done < ~/.cdvars
    if [ -z "$a" ] ; then
      while read a ; do
        if [ -z "${a/*$1*/}" ] ; then break ; fi
      done < ~/.cds
    fi
    if [ -z "$a" ] ; then
       echo "cds: not a directory: $1;  try 'cds -h' for help, or one of these:"
       cds
    else
       'cd' "$a"
    fi
  fi
}

alias luai="lua -l ix -i"

# MacPorts location of git:
if [ "$EMACS" != "t" -a -f /usr/local/git/contrib/completion/git-completion.bash ]; then
  . /usr/local/git/contrib/completion/git-completion.bash
fi

if [ "$TERM" == "dumb" ] ; then
   export NODE_NO_READLINE=1
fi

# host-specific interactive settings

alias vce="python ~/local/bin/vce/vce.py"
