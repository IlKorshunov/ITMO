#!/bin/bash

SOLUTION_DIR="../java-solutions/info/kgeorgiy/ja/korshunov"
SCRIPTS_DIR=$(pwd)

IMPLEMENTATOR="$SOLUTION_DIR/implementor"
Implementor="$IMPLEMENTATOR/Implementor.java"
myCompiler="$IMPLEMENTATOR/myCompiler.java"
JarImpler="../../java-advanced-2024/modules/info.kgeorgiy.java.advanced.implementor/info/kgeorgiy/java/advanced/implementor/JarImpler.java"
Impler="../../java-advanced-2024/modules/info.kgeorgiy.java.advanced.implementor/info/kgeorgiy/java/advanced/implementor/Impler.java"
ImplerException="../../java-advanced-2024/modules/info.kgeorgiy.java.advanced.implementor/info/kgeorgiy/java/advanced/implementor/ImplerException.java"

mkdir -p "$SCRIPTS_DIR/module/javadoc"

javadoc -d "$SCRIPTS_DIR/module/javadoc" \
    -private \
    "$Implementor" "$myCompiler" "$JarImpler" "$Impler" "$ImplerException"


read -p "Press any key to continue . . ."
