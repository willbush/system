# Fix tramp login hang (see: https://www.emacswiki.org/emacs/TrampMode)
if [[ $TERM == "dumb" ]]; then
	unsetopt zle
	unset zle_bracketed_paste
	export PS1='%m %~ $ '
fi

# https://github.com/akermu/emacs-libvterm#shell-side-configuration
vterm_printf() {
	if [ -n "$TMUX" ]; then
		# Tell tmux to pass the escape sequences through
		# (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
		printf "\ePtmux;\e\e]%s\007\e\\" "$1"
	elif [ "${TERM%%-*}" = "screen" ]; then
		# GNU screen (screen, screen-256color, screen-256color-bce)
		printf "\eP\e]%s\007\e\\" "$1"
	else
		printf "\e]%s\e\\" "$1"
	fi
}

# https://github.com/akermu/emacs-libvterm#vterm-clear-scrollback
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
  alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

# https://github.com/akermu/emacs-libvterm#vterm-buffer-name-string
autoload -U add-zsh-hook
add-zsh-hook -Uz chpwd () { print -Pn "\e]2;%m:%2~\a" }

# https://github.com/akermu/emacs-libvterm#directory-tracking-and-prompt-tracking
vterm_prompt_end() {
  vterm_printf "51;A$(whoami)@$(hostname):$(pwd)";
}
setopt PROMPT_SUBST
PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
