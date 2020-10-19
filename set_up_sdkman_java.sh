#!/usr/bin/env bash

JAVA_BIN_DIR="/home/fap/.sdkman/candidates/java/current/bin"

sudo update-alternatives --install /usr/bin/java java $JAVA_BIN_DIR/java 1
sudo update-alternatives --install /usr/bin/javac javac $JAVA_BIN_DIR/javac 1
sudo update-alternatives --install /usr/bin/jar jar $JAVA_BIN_DIR/jar 1
sudo update-alternatives --install /usr/bin/jshell jshell $JAVA_BIN_DIR/jshell 1

