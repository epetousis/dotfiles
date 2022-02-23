iscommand() {
  type "$1" > /dev/null
}

# Cargo
[[ ! -d ~/.cargo ]] || . "$HOME/.cargo/env"

export iCloudDrive="$HOME/Library/Mobile Documents/com~apple~CloudDocs"

# fnm
[[ ! -d ~/.fnm ]] || export PATH=/home/epetousis/.fnm:$PATH
! iscommand "fnm" || eval "`fnm env`"

! iscommand "yarn" || export PATH="$PATH:$(yarn global bin)"

# Poetry
export PATH="$HOME/.poetry/bin:$PATH"
alias poetry="python3 $HOME/.poetry/bin/poetry"

# Pyenv
[[ ! -d ~/.pyenv ]] || export PATH="$HOME/.pyenv/bin:$PATH"
