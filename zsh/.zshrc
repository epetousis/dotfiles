# Start tmux
# Adapted from https://unix.stackexchange.com/a/113768
if command -v tmux &> /dev/null && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
  # No exec is used here, just in case we don't actually want tmux (or it's broken)
  tmux
fi

# Discourage instinctively opening default macOS Terminal
if [[ $TERM_PROGRAM == "Apple_Terminal" ]]; then
  tput setab 3;echo "=== STOP! You are using the built-in macOS terminal when you have opted for an alternative terminal. ==="
  tput setab 3;echo "Disregard if opening Terminal was intended."
fi

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Add .zfunc folder to completions path
fpath+=~/.zfunc

if type brew &>/dev/null; then
  FPATH=$(brew --prefix)/share/zsh-completions:$FPATH
fi

autoload -Uz compinit
compinit

export TERM="screen-256color"

# Set default editor
export VISUAL=nvim
export EDITOR="$VISUAL"

bindkey -v # vim mode
bindkey "^H" backward-delete-char
bindkey "^?" backward-delete-char

# Kill key timeout so escape is instant
KEYTIMEOUT=1

# Terminal prompt theming - optional powerlevel10k support
[[ ! -d /opt/homebrew/opt/powerlevel10k ]] || source /opt/homebrew/opt/powerlevel10k/powerlevel10k.zsh-theme
[[ ! -d ~/.powerlevel10k ]] || source ~/.powerlevel10k/powerlevel10k.zsh-theme
[[ -d ~/.powerlevel10k ]] || PROMPT='%n@%m:%(4~|...|)%3~ %% '

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# Nix - this should stay in .zshrc due to https://github.com/NixOS/nix/issues/4169
if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then . $HOME/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

# Pyenv - init should only be run in login shells, see https://github.com/pyenv/pyenv#advanced-configuration
! iscommand "pyenv" || eval "$(pyenv init -)"
# Check for Homebrew pyenv-virtualenv, or the plugin installation method
! (iscommand "pyenv-activate" || (iscommand "pyenv" && [[ -d $(pyenv root)/plugins/pyenv-virtualenv ]])) || eval "$(pyenv virtualenv-init -)"

# thefuck autocompletions
! iscommand "thefuck" || eval $(thefuck --alias)

# Run keychain agent on non-macOS Unix-like systems
! iscommand "keychain" || eval `keychain --eval --agents ssh id_rsa`
