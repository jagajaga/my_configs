#!/usr/bin/env bash
# translate clipboard contents
# usage: screen-translate.sh [desination language]
# default transtation is to russian
# requeries pbpaste to read from clipboard
# requeries wget, grep
export PATH=/usr/local/bin:$PATH

query=$(pbpaste)
if [[ $? -ne 0 ]]; then
    query="NULL"
fi

rawurlencode() {
  local string="${1}"
  local strlen=${#string}
  local encoded=""
  for (( pos=0 ; pos<strlen ; pos++ )); do
     c=${string:$pos:1}
     case "$c" in
        [-_.~a-zA-Z0-9а-яА-Я] ) o="${c}" ;;
        * )               printf -v o '%%%02x' "'$c"
     esac
     encoded+="${o}"
  done
  REPLY="${encoded}"   #+or echo the result (EASIER)... or both... :p
}

show-translation-notification() {
  resu=`echo $1 | tr -d '"'`
  osascript -e 'tell app "System Events" to display alert "Translation" message "'"$resu"'"'
}
rawurlencode "$query"
LANG="ru"
if [ "$1" == "en" ]
then
    LANG="$1"
fi
trans=$(wget -qO- https://translate.yandex.net/api/v1.5/tr/translate\?key\=trnsl.1.1.20131216T210116Z.376bb5a521f8f30a.41d2cf22821568e6d931091bb5fe6aaaac979c7e\&text\=$REPLY\&lang\=$LANG)
if [[ $? -ne 0 ]]; then
    show-translation-notification "Failed to translate clipboard contents."'"'
fi
tag="text"
langfrom=$(echo $trans | ggrep -Poe '(?<=lang=").*?(?=-)')
echo $langfrom
trans=$(echo $trans | ggrep -oPm1 "(?<=<text>)[^<]+")
echo $trans

show-translation-notification "$langfrom → ru\n\n$query\n→\n$trans"
if [[ $? -ne 0 ]]; then
    show-translation-notification "Failed to translate clipboard contents."
fi
exit
