#!/bin/bash
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")"

if [ $# -eq 0 ]; then
  if [ "$(TZ='America/New_York' date '+%H')" -ge 23 ]; then
    day="$(TZ='America/New_York' date -v '+1d' '+%d' | sed 's/^0*//' | xargs printf '%02d')"
  else
    day="$(TZ='America/New_York' date '+%d' | sed 's/^0*//' | xargs printf '%02d')"
  fi

  echo "Creating code file ..."
  cp DayXX.hs "Day${day}.hs"
  sed -i '' -e "s/XX/${day}/" "Day${day}.hs"

  echo "Waiting for input .."
  mkdir -p inputs
  if [ "$(TZ='America/New_York' date '+%H')" -ge 23 ]; then
    aocdl -output "inputs/Day${day}.txt" -wait
  else
    aocdl -output "inputs/Day${day}.txt"
  fi

  echo "Watching the main function .."
  ghcid --test='main' -W -c "stack ghci --ghc-options='-w' Day${day}.hs"
elif [ $# -eq 1 ]; then
  day="$(printf '%02d' "$1")"

  echo "Creating code file ..."
  cp DayXX.hs "Day${day}.hs"
  sed -i '' -e "s/XX/${day}/" "Day${day}.hs"

  echo "Waiting for input .."
  mkdir -p inputs
  aocdl -output "inputs/Day${day}.txt"

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
