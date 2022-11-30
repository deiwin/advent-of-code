#!/bin/bash
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")"

if [ $# -eq 0 ]; then
  day="$(TZ='Europe/Tallinn' date '+%d' | sed 's/^0*//' | xargs printf '%02d')"

  echo "Creating code file ..."
  cp DayXX.hs "Day${day}.hs"
  sed -i '' -e "s/XX/${day}/" "Day${day}.hs"

  echo "Waiting for input .."
  mkdir -p inputs
  if [ "$(TZ='Europe/Tallinn' date '+%H')" -ge 7 ]; then
    aocdl -output "inputs/Day${day}.txt"
  else
    aocdl -output "inputs/Day${day}.txt" -wait
  fi

  echo "Watching the main function .."
  ghcid --test='main' -W -c "stack ghci --ghc-options='-w' Day${day}.hs"
elif [ $# -eq 2 ]; then
  year="$1"
  day="$(printf '%02d' "$2")"
  name="Y${year}D${day}"

  echo "Creating code file ..."
  mkdir -p "Practice"
  cp DayXX.hs "Practice/${name}.hs"
  sed -i '' -e "s/module DayXX/module Practice.${name}/" "Practice/${name}.hs"
  sed -i '' -e "s|inputs/DayXX|Practice/inputs/${name}|" "Practice/${name}.hs"

  echo "Waiting for input .."
  mkdir -p "Practice/inputs"
  aocdl -output "Practice/inputs/${name}.txt" -day "$day" -year "$year"

  echo "Watching the main function .."
  ghcid --test='main' -W -c "stack ghci --ghc-options='-w' Practice/${name}.hs"
else
  echo "I don't understand" >&2
  exit 1
fi
