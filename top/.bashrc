# .bashrc

# See ~/.home/shells.txt

#--------------------------------
# Login Settings
#--------------------------------

if [[ -z "$BASHRC_ONCE" ]] ; then
    export BASHRC_ONCE=1

    [[ -r /etc/bashrc ]] && . /etc/bashrc

    export P4CONFIG=.p4
    if [[ "${OSTYPE:0:6}" = darwin ]]; then
        # /usr/local/bin = MacPorts
        PATH="/usr/local/bin:$HOME/local/bin/mac:$PATH"
        export BASH_SILENCE_DEPRECATION_WARNING=1
    fi
    PATH="$HOME/.cargo/bin:$PATH"
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
    export TERM=emacs
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

# host-specific interactive settings
[[ -r ~/.bashrc-local-i ]] && . ~/.bashrc-local-i
