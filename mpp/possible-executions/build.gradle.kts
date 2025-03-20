plugins {
    kotlin("jvm") version "2.0.20"
    application
}

group = "ru.itmo.mpp"

repositories {
    mavenCentral()
}

java {
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(21))
    }
}

sourceSets.main {
    java.srcDir("src")
}

application {
    mainClass.set("PossibleExecutionsVerifierKt")
}

tasks["build"].dependsOn("run")