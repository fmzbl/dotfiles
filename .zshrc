parse_git_branch() {
    git branch 2> /dev/null | sed -n -e 's/^\* \(.*\)/[\1]/p'
}
COLOR_DEF='%f'
COLOR_USR='%F{243}'
COLOR_DIR='%F{197}'
COLOR_GIT='%F{39}'
NEWLINE=$'\n'
setopt PROMPT_SUBST
export PROMPT='${COLOR_USR}%n@%M ${COLOR_DIR}%~ ${COLOR_GIT}$(parse_git_branch)${COLOR_DEF}${NEWLINE}%% '

source /opt/homebrew/opt/chruby/share/chruby/auto.sh
source /opt/homebrew/opt/chruby/share/chruby/chruby.sh
alias vi="nvim"
alias vim="nvim"
chruby ruby-3.1.2

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
