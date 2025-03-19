#!/bin/bash

SOLUTION_DIR="../java-solutions/info/kgeorgiy/ja/korshunov"
TEST_DIR="../../java-advanced-2024/artifacts/info.kgeorgiy.java.advanced.implementor.jar"
SCRIPTS_DIR=$(pwd)

IMPLEMENTATOR="$SOLUTION_DIR/implementor"
Implementor="$IMPLEMENTATOR/Implementor.java"
myCompiler="$IMPLEMENTATOR/myCompiler.java"
JarImpler="../../java-advanced-2024/modules/info.kgeorgiy.java.advanced.implementor/info/kgeorgiy/java/advanced/implementor/JarImpler.java"
Impler="../../java-advanced-2024/modules/info.kgeorgiy.java.advanced.implementor/info/kgeorgiy/java/advanced/implementor/Impler.java"
Impler="../../java-advanced-2024/modules/info.kgeorgiy.java.advanced.implementor/info/kgeorgiy/java/advanced/implementor/ImplerException.java"

mkdir -p "$SCRIPTS_DIR/module"

javac -d "$SCRIPTS_DIR/module" -classpath "$TEST_DIR" "$Implementor" "$myCompiler" "$JarImpler" "$Impler"
jar cmf "$SCRIPTS_DIR/MANIFEST.MF" "$SCRIPTS_DIR/implementorhw.jar" -C "$SCRIPTS_DIR/module" .

read -p "Press any key to continue . . ."
