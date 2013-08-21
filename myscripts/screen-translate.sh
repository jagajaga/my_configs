#!/bin/bash
# translate-selection.sh - google translation of selected text
# needs xsel to read from clipboard
google-translate() {
    source=auto
    target="$2"
    result=$(curl -s -i --user-agent "" -d "sl=$source" -d "tl=$target" --data-urlencode "text=$1" http://translate.google.com)
    encoding=$(awk '/Content-Type: .* charset=/ {sub(/^.*charset=["'\'']?/,""); sub(/[ "'\''].*$/,""); print}' <<<"$result")
    #iconv -f $encoding <<<"$result" | awk 'BEGIN {RS="<div"};/<span[^>]* id=["'\'']?result_box["'\'']?/ {sub(/^.*id=["'\'']?result_box["'\'']?(>| [^>]*>)([ \n\t]*<[^>]*>)*/,"");sub(/<.*$/,"");print}' | html2text -utf8
    iconv -f $encoding <<<"$result" |  awk 'BEGIN {RS="</div>"};/<span[^>]* id=["'\'']?result_box["'\'']?/' | html2text 
}

query=$(xsel)
#notify-send "Google Translate" "Query is ${query}"
translation=$(google-translate "$query" "ru")
notify-send "$query → $translation"
#zenity --info --title "Translation" --text "$translation"
exit
