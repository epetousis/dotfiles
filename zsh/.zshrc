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

iscommand() {
  type "$1" > /dev/null
}

if type brew &>/dev/null; then
  FPATH=$(brew --prefix)/share/zsh-completions:$FPATH
  
  autoload -Uz compinit
  compinit
fi

export TERM="screen-256color"

# Set default editor
export VISUAL=nvim
export EDITOR="$VISUAL"

bindkey -v # vim mode
bindkey "^H" backward-delete-char
bindkey "^?" backward-delete-char

# Kill key timeout so escape is instant
KEYTIMEOUT=1

# fnm
[[ ! -d ~/.fnm ]] || export PATH=/home/epetousis/.fnm:$PATH
! iscommand "fnm" || eval "`fnm env`"

! iscommand "yarn" || export PATH="$PATH:$(yarn global bin)"


export PATH="$HOME/.poetry/bin:$PATH"
alias poetry="python3 $HOME/.poetry/bin/poetry"

fpath+=~/.zfunc
[[ ! -d /opt/homebrew ]] || source /opt/homebrew/opt/powerlevel10k/powerlevel10k.zsh-theme
[[ ! -d ~/.powerlevel10k ]] || source ~/.powerlevel10k/powerlevel10k.zsh-theme
[[ -d ~/.powerlevel10k ]] || PROMPT='%n@%m:%(4~|...|)%3~ %% '

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh


export iCloudDrive="$HOME/Library/Mobile Documents/com~apple~CloudDocs"
if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then . $HOME/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

[[ ! -d ~/.pyenv ]] || export PATH="$HOME/.pyenv/bin:$PATH"
! iscommand "pyenv" || eval "$(pyenv init -)"
# Check for Homebrew pyenv-virtualenv, or the plugin installation method
! (iscommand "pyenv-activate" || (iscommand "pyenv" && [[ -d $(pyenv root)/plugins/pyenv-virtualenv ]])) || eval "$(pyenv virtualenv-init -)"

! iscommand "thefuck" || eval $(thefuck --alias)

! iscommand "keychain" || eval `keychain --eval --agents ssh id_rsa`
