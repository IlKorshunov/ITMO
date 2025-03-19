#!/bin/bash

SCRIPTS_DIR=$(pwd)

LOCALDIR="../local"
PARENTDIR="../parent"
REMOTEDIR="../remote"
RMIDIR="../rmi"

DIRS=("$LOCALDIR" "$PARENTDIR" "$REMOTEDIR" "$RMIDIR")

OUTPUT_DIR="./compile"
FLAT_DIR="./all"
mkdir -p "$OUTPUT_DIR"
mkdir -p "$FLAT_DIR"

JAVA_FILES=()
for dir in "${DIRS[@]}"; do
    while IFS= read -r -d $'\0' file; do
        JAVA_FILES+=("$file")
    done < <(find "$SCRIPTS_DIR/$dir" -type f -name '*.java' -print0)
done

javac -d "$OUTPUT_DIR" "${JAVA_FILES[@]}"

find "$OUTPUT_DIR" -type f -name '*.class' -exec mv {} "$FLAT_DIR" \;

read -p "Press any key to continue . . ."
