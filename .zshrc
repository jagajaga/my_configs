# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/jaga/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
bindkey '\e[3~' delete-char # del
bindkey ';5D' backward-word # ctrl+left 
bindkey ';5C' forward-word #ctrl+right
[[ -n ${key[Home]}    ]]  && bindkey  "${key[Home]}"    beginning-of-line
[[ -n ${key[End]}     ]]  && bindkey  "${key[End]}"     end-of-line
[[ -n ${key[Insert]}  ]]  && bindkey  "${key[Insert]}"  overwrite-mode
[[ -n ${key[Delete]}  ]]  && bindkey  "${key[Delete]}"  delete-char
[[ -n ${key[Up]}      ]]  && bindkey  "${key[Up]}"      up-line-or-history
[[ -n ${key[Down]}    ]]  && bindkey  "${key[Down]}"    down-line-or-history
[[ -n ${key[Left]}    ]]  && bindkey  "${key[Left]}"    backward-char
[[ -n ${key[Right]}   ]]  && bindkey  "${key[Right]}"   forward-char
[[ -n ${key[Backspace]}   ]]  && bindkey  "${key[Backspace]}"   backward-delete-char
zmodload -a zsh/stat stat
zmodload -a zsh/zpty zpty
zmodload -a zsh/zprof zprof
zmodload -ap zsh/mapfile mapfile

# Completions 
zstyle ':completion:*::::' completer _expand _complete _ignored _approximate
zstyle -e ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX+$#SUFFIX)/3 )) numeric )'
zstyle ':completion:*:expand:*' tag-order all-expansions
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*:corrections' format '%B%d (errors: %e)%b'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters
zstyle ':completion:*:*:(^rm):*:*files' ignored-patterns '*?.o' '*?.c~''*?.old' '*?.pro'
zstyle ':completion:*:functions' ignored-patterns '_*'

# менюшку нам для астокомплита 
zstyle ':completion:*' menu yes select

# 
# различные опцие шела 
# 
# Позволяем разворачивать сокращенный ввод, к примеру cd /u/sh в /usr/share 
autoload -U compinit && compinit

# файл истории команд 
HISTFILE=~/.zhistory

# Число команд, сохраняемых в HISTFILE 
SAVEHIST=5000

# Дополнение файла истории 
setopt  APPEND_HISTORY

# Игнорировать все повторения команд 
setopt  HIST_IGNORE_ALL_DUPS

# Игнорировать лишние пробелы 
setopt  HIST_IGNORE_SPACE

# не пищать при дополнении или ошибках 
setopt NO_BEEP

# если набрали путь к директории без комманды CD, то перейти 
setopt AUTO_CD

# исправлять неверно набранные комманды 
setopt CORRECT_ALL

# zsh будет обращаться с пробелами так же, как и bash 
setopt SH_WORD_SPLIT

# последние комманды в начале файла и не хранить дубликаты 
setopt histexpiredupsfirst histfindnodups

# ещё всякая херь про истоию 
setopt histignoredups histnostore histverify histignorespace extended_history  share_history

# Установка и снятие различных опций шелла 
setopt   notify globdots correct pushdtohome cdablevars autolist
setopt   correctall autocd recexact longlistjobs
setopt   autoresume histignoredups pushdsilent noclobber
setopt   autopushd pushdminus extendedglob rcquotes mailwarning
unsetopt bgnice autoparamslash

# Не считать Control+C за выход из оболочки 
setopt  IGNORE_EOF

# автоматическое удаление одинакового из этого массива 
typeset -U path cdpath fpath manpath

# загружаем список цветов 
autoload colors && colors

# 
# Установка PROMT 
# 
# левый 
# вопрос на автокоррекцию 
#PROMPT='zsh: Заменить '\''%R'\'' на '\''%r'\'' ? [Yes/No/Abort/Edit] '

# симпотное добавления для kill 
zstyle ':completion:*:*:kill:*:processes' list-colors "=(#b) #([0-9]#)*=$color[cyan]=$color[red]"

# заголовки и прочее. 

precmd() {
	 [[ -t 1 ]] || return
	case $TERM in
	*xterm*|rxvt|(dt|k|E|a)term*) print -Pn "\e]0;[%~] %m\a"	;;
	screen(-bce|.linux)) print -Pn "\ek[%~]\e\" && print -Pn "\e]0;[%~] %m (screen)\a" ;;  #заголовок для скрина
	esac
}
preexec() {
	[[ -t 1 ]] || return
	case $TERM in
	*xterm*|rxvt|(dt|k|E|a)term*) print -Pn "\e]0;<$1> [%~] %m\a" ;;
	screen(-bce|.linux)) print -Pn "\ek<$1> [%~]\e\" && print -Pn "\e]0;<$1> [%~] %m (screen)\a" ;; #заголовок для скрина
	esac
}
typeset -g -A key

# 
# экранируем спецсимволы в url, например &, ?, ~ и так далее 
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

# 
# мои хоткеи 
# 
# дополнение по истории, ^X^Z включить ^Z выключить 
autoload -U predict-on
zle -N predict-on
zle -N predict-off
bindkey -M emacs "^X^Z" predict-on
bindkey -M emacs "^Z" predict-off

# peжuм нaвuгaцuu в cтuлe emacs 
bindkey -e

# режим редактирования команды, вызывает для этого то что в $EDITOR 
autoload -U edit-command-line

# Вызов редактора для редактирования строки ввода (хоткей в стиле emacs) 
# bindkey -M vicmd v edit-command-line для командного режима vi 
zle -N  edit-command-line
bindkey -M emacs "^X^E" edit-command-line

#завершить слово команду 
bindkey -M emacs "^N" complete-word

#вызов диалога удаления файлов в папке 
function dialogrun; { rm -rf $(dialog --separate-output --checklist file 100 100 100 $(for l in $(ls -A); do echo "$l" "$(test -d $l && echo "dir" || echo "file")" 0; done) --stdout); clear  }
zle -N dialogrun
bindkey -M emacs "^X^O" dialogrun

# куда же мы без калькулятора 
autoload -U zcalc

# 
# мои функции 
# 
ccd() { cd && ls}

# создать директорию и перейти в нее 
mcd(){ mkdir $1; cd $1 }

# если текущая директория пустая, то удалить ее и перейти в родительскую директорию 
rcd(){ local P="`pwd`"; cd .. && rmdir "$P" || cd "$P"; }

# быстрое переименование 
name() {
    name=$1
    vared -c -p 'rename to: ' name
    command mv $1 $name
}

# распаковка архива 
extract () {
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)   tar xjf $1        ;;
            *.tar.gz)    tar xzf $1     ;;
            *.bz2)       bunzip2 $1       ;;
            *.rar)       unrar x $1     ;;
            *.gz)        gunzip $1     ;;
            *.tar)       tar xf $1        ;;
            *.tbz2)      tar xjf $1      ;;
            *.tgz)       tar xzf $1       ;;
            *.zip)       unzip $1     ;;
            *.Z)         uncompress $1  ;;
            *.7z)        7z x $1    ;;
            *)           echo "я не в курсе как распаковать '$1'..." ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

# упаковка в архив 
pk () {
    if [ $1 ] ; then
        case $1 in
            tbz)   	tar cjvf $2.tar.bz2 $2      ;;
            tgz)   	tar czvf $2.tar.gz  $2   	;;
            tar)  	tar cpvf $2.tar  $2       ;;
			bz2)	bzip $2 ;;
            gz)		gzip -c -9 -n $2 > $2.gz ;;
			zip)   	zip -r $2.zip $2   ;;
            7z)    	7z a $2.7z $2    ;;
            *)     	echo "'$1' cannot be packed via pk()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

# мой cd 
cdpath=( . ~ /mnt/MLIVE )

# mp3 в нормальную кодировку 
mp32utf() { find -iname '*.mp3' -print0 | xargs -0 mid3iconv -eCP1251 --remove-v1 }

# конвертируем всякую дурь 
mpg2flv() { ffmpeg -i $1 -ar 22050 -ab 32 -f flv -s 320x240 `echo $1 | awk -F . '{print $1}'`.flv }
flv2xvid() { mencoder "$1" -vf scale=320:240  -ovc xvid -xvidencopts bitrate=250:autoaspect -vf pp=lb -oac mp3lame  -lameopts fast:preset=standard -o  "./basename $1.avi" }
flv2divx() { mencoder "$1" --vf scale=320:240  -ovc lavc -lavcopts vcodec=mpeg4:vbitrate=250:mbd=2:v4mv:autoaspect -vf pp=lb -oac mp3lame  -lameopts fast:preset=standard -o  "./basename $1.avi" }

# top по имени процесса, правда только по полному 
pidtop() {top -p `pidof $@ | tr ' ' ','`}

# простой калькулятор 
calc() {echo "${1}"|bc -l;}

# мой айпишник 
myip() {lynx --source http://www.formyip.com/ |grep The | awk {'print $5'}}

# великий рандом для перемешивания строк в файле 
rand() { awk '{print rand()"\t"$0}'|sort|awk -F'\t' '{print $2}'  }

# копипаст в консоли 
ccopy(){ cp $1 /tmp/ccopy.$1; }
alias cpaste="ls /tmp/ccopy.* | sed 's|/tmp/ccopy.||' | xargs -I % mv /tmp/ccopy.% ./%"

# 
# переменные окружения и прочая чушь 
# 
# перенаправляем
READNULLCMD=${PAGER}

#оформим подсветку в grep 
export GREP_COLOR="1;33"

# если стоит most то заюзаем в качестве $PAGER 
[[ -x $(whence -p most) ]] && export PAGER=$(whence -p most)

# редактор по дефолту 
export EDITOR=vim

autoload zkbd
[[ ! -f ${ZDOTDIR:-$HOME}/.zkbd/konsole-:0 ]] && zkbd
source ${ZDOTDIR:-$HOME}/.zkbd/konsole-:0

[[ -n ${key[Backspace]} ]] && bindkey "${key[Backspace]}" backward-delete-char
[[ -n ${key[Insert]} ]] && bindkey "${key[Insert]}" overwrite-mode
[[ -n ${key[Home]} ]] && bindkey "${key[Home]}" beginning-of-line
[[ -n ${key[PageUp]} ]] && bindkey "${key[PageUp]}" up-line-or-history
[[ -n ${key[Delete]} ]] && bindkey "${key[Delete]}" delete-char
[[ -n ${key[End]} ]] && bindkey "${key[End]}" end-of-line
[[ -n ${key[PageDown]} ]] && bindkey "${key[PageDown]}" down-line-or-history
[[ -n ${key[Up]} ]] && bindkey "${key[Up]}" up-line-or-search
[[ -n ${key[Left]} ]] && bindkey "${key[Left]}" backward-char
[[ -n ${key[Down]} ]] && bindkey "${key[Down]}" down-line-or-search
[[ -n ${key[Right]} ]] && bindkey "${key[Right]}" forward-char
# пути где искать бинарники 
export PATH="$PATH:~/soft/bin/"
PATH=$PATH:/opt/pycharm/bin:/opt/sublime-text;
export PATH;


# ООо и русские имена файлов 
export OOO_FORCE_DESKTOP=gnome

# забыл зачем ставил 
export LESSCHARSET=UTF-8

#разукрашиваем ls и автодополнение 
export LS_COLORS='no=00;37:fi=00;37:di=01;36:ln=04;36:pi=33:so=01;35:do=01;35:bd=33;01:cd=33;01:or=31;01:su=37:sg=30:tw=30:ow=34:st=37:ex=01;31:*.cmd=01;31:*.exe=01;31:*.com=01;31:*.btm=01;31:*.sh=01;31:*.run=01;31:*.tar=33:*.tgz=33:*.arj=33:*.taz=33:*.lzh=33:*.zip=33:*.z=33:*.Z=33:*.gz=33:*.bz2=33:*.deb=33:*.rpm=33:*.jar=33:*.rar=33:*.jpg=32:*.jpeg=32:*.gif=32:*.bmp=32:*.pbm=32:*.pgm=32:*.ppm=32:*.tga=32:*.xbm=32:*.xpm=32:*.tif=32:*.tiff=32:*.png=32:*.mov=34:*.mpg=34:*.mpeg=34:*.avi=34:*.fli=34:*.flv=34:*.3gp=34:*.mp4=34:*.divx=34:*.gl=32:*.dl=32:*.xcf=32:*.xwd=32:*.flac=35:*.mp3=35:*.mpc=35:*.ogg=35:*.wav=35:*.m3u=35:';
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

# значение цветов	 #30 черный текст	 #40 черный фон 
#00 восстановление цвета по умолчанию	#31 красный текст	 #41 красный фон 
#01 включить яркие цвета	 #32 зеленый текст	 #42 зеленый фон 
#04 подчеркнутый текст	 #33 желтый (или коричневый) текст	#43 желтый (или коричневый) фон 
#05 мигающий текст	 #34 синий текст	 #44 синий фон 
# ну или color юзать	 #35 фиолетовый текст	 #45 фиолетовый фон 
#	 #36 cyan текст	 #46 cyan фон 
# алиасы	 #37 белый (или серый) текст	 #47 белый (или серый) фон 
# 
# цветной grep 
alias grep='grep --color=auto'

# более человекочитаемые df и du 
alias df='df -h'
alias du='du -h'

# переименование-перемещение c пogтвepжgeнueм без коррекции 
alias mv='nocorrect mv -i'

# рекурсивное копирование с подтверждением без коррекции 
alias cp='nocorrect cp -iR'

# удаление с подтверждением без коррекции 
alias rm='nocorrect rm -i'

# принудимтельное удаление без коррекции 
alias rmf='nocorrect rm -f'

# принудительное рекурсивное удаление без коррекции 
alias rmrf='nocorrect rm -fR'

# создание каталогов без коррекции 
alias mkdir='nocorrect mkdir'

# показ файлов в цвете 
alias ls='ls -F --color=auto'

# разукрашиваем некоторые команды с помощью grc 
[[ -f /usr/bin/grc ]] && {
  alias ping="grc --colour=auto ping"
  alias traceroute="grc --colour=auto traceroute"
  alias make="grc --colour=auto make"
  alias diff="grc --colour=auto diff"
  alias cvs="grc --colour=auto cvs"
  alias netstat="grc --colour=auto netstat"
}

# разукрашиваем логи с помощью grc 
alias logc="grc cat"
alias logt="grc tail"
alias logh="grc head"

# 
# запуск программ 
# 
# везде 
alias -s txt=$PAGER
alias -s py=python

# в иксах 
alias -s {png,gif,jpg,jpeg}=feh
alias -s {pdf,djvu}=evince

# без иксов 
[[ -z $DISPLAY ]] && {
	alias -s {odt,doc,sxw,xls,doc,rtf}=catdoc
	alias -s {png,gif,jpg,jpeg}="fbi -a"
	alias -s {pdf,djvu}=evince
}

# html сам пусть соображает чё запускать 
autoload -U pick-web-browser
alias -s {html,htm}=pick-web-browser

# 
# глобальные алиасы 
# 
alias -g H="| head"
alias -g T="| tail"
alias -g G="| grep"
alias -g L="| less"
alias -g M="| most"
alias -g B="&|"
alias -g HL="--help"
alias -g LL="2>&1 | less"
alias -g CA="2>&1 | cat -A"
alias -g NE="2> /dev/null"
alias -g NUL="> /dev/null 2>&1"


# 
# sudo 
alias halt="sudo halt"
alias reboot="sudo reboot"
alias gparted="sudo gparted"

# родной скрин 
alias screen="screen -DR"

# ну так привычнее :) 
alias ncmpc="ncmpcpp"

# lastfm 
alias shell-fm="shell-fm lastfm://user/tiss93"

# список удаленных файлов с NTFS, FAT, UFS1/2, FFS, Ext2 и Ext3 
# пакет sleuthkit, утилита icat для восстановления 
alias fls="fls -rd"

# показываев дерево директорий 
alias dirf='find . -type d | sed -e "s/[^-][^\/]*\//  |/g" -e "s/|\([^ ]\)/|-\1/"'

# grep по ps aux 
alias psgrep='ps aux | grep $(echo $1 | sed "s/^\(.\)/[\1]/g")'

# удаляем пустые строки и комментарии 
alias delspacecomm="sed '/ *#/d; /^ *$/d' $1"


# 
# команды при запуске zsh 
# 

if [[ $EUID == 0 ]] 
then
PROMPT=$'%{\e[1;31m%}%n %{\e[1;34m%}%~ #%{\e[0m%} ' # user dir %
else
PROMPT=$'%{\e[1;32m%}%n %{\e[1;34m%}%~ %#%{\e[0m%} ' # root dir #
fi

alias ls='ls --color=auto'
alias grep='grep --colour=auto'
export PATH="/opt/android-sdk/tools:${PATH}"
export PATH="/opt/android-sdk/platform-tools:${PATH}"
export PATH="~/.gem/ruby/1.9.1/bin:${PATH}"


alias h='htop'
alias n='sudo nethogs wlan0'
alias io='sudo iotop'
alias editawesome='vim /etc/xdg/awesome/rc.lua'
alias update='sudo yaourt -Syua'
alias screenshot='sleep 10;import -window root myscreen.png'
alias pacman='sudo pacman'
alias open='xdg-open'
alias connect='sudo netcfg -r Jaga-Jaga'
alias showdisk='sudo fdisk -l'
alias cp='pycp'


unsetopt correct_all
zstyle ':completion:*:processes' command 'ps -au$USER' 
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;32'
source /usr/share/doc/pkgfile/command-not-found.zsh
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on'
