#!/usr/bin/env bash
# needs xsel to read from clipboard

query=$(xsel)
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
rawurlencode "$query"
LANG="ru"
if [ "$1" == "en" ]
then
    LANG="$1"
fi
trans=$(wget -qO- https://translate.yandex.net/api/v1.5/tr/translate\?key\=trnsl.1.1.20131216T210116Z.376bb5a521f8f30a.41d2cf22821568e6d931091bb5fe6aaaac979c7e\&text\=$REPLY\&lang\=$LANG)
tag="text"
trans=$(echo $trans | grep -oPm1 "(?<=<text>)[^<]+") 
notify-send "$query → $trans"
exit
