plugins {
    kotlin("jvm") version "2.0.20"
    java
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

sourceSets.test {
    java.srcDir("test")
}

application {
    mainClass.set("VerifyMonotonicClockKt")
}

tasks["build"].dependsOn("run")