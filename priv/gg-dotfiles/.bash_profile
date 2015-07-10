PS1='\[\e[4m\e[32m\]\t:\W\[\e[0;36m\]$(__git_ps1 "(%s)") \[\e[m\]\$ '; [[ $EUID == 0 ]] &&
PS1='\[\e[4m\e[31m\]\t:\W\[\e[1;36m\]$(__git_ps1 "(%s)") \[\e[m\]\$ '
export GIT_PS1_SHOWDIRTYSTATE=1
