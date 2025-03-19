#!/bin/bash

# Указание путей к входному и выходному файлам
INPUT_FILE="../java-solutions/info/kgeorgiy/ja/korshunov/exam/bundles/bundle_ru_UTF.properties"
OUTPUT_FILE="../java-solutions/info/kgeorgiy/ja/korshunov/exam/bundles/bundle_ru_RU.properties"

convert_escape() {
  local input="$1"
  local output=""
  for (( i=0; i<${#input}; i++ )); do
    char="${input:$i:1}"
    ascii=$(printf "%d" "'$char")
    if [ "$ascii" -ge 128 ]; then
      hex=$(printf "%04X" "$ascii")
      tmp="\\u$hex"
      output+="$tmp"
    else
      output+="$char"
    fi
  done
  echo "$output"
}

# Очистка выходного файла
> "$OUTPUT_FILE"

# Обработка строк из входного файла и запись в выходной файл
while IFS= read -r line; do
  outLine=$(convert_escape "$line")
  echo "$outLine" >> "$OUTPUT_FILE"
done < "$INPUT_FILE"

read -p "Press any key to continue . . ."
