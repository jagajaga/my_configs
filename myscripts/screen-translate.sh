#!/bin/bash
# translate-selection.sh - google translation of selected text
# needs xsel to read from clipboard

query=$(xsel)
#notify-send "Google Translate" "Query is ${query}"
translation=$(/home/jaga/myscripts/google-translate.sh "$query" "ru")
notify-send "$query → $translation"
#zenity --info --title "Translation" --text "$translation"
exit
