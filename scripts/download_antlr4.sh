#!/bin/bash

SHELL_FOLDER=$(cd "$(dirname "$0")";pwd)
LIB_FOLDER=$(dirname $SHELL_FOLDER)"/lib"

ANTLR_ULR="https://www.antlr.org/download/antlr-4.9.3-complete.jar"
echo "start download "$ANTLR_ULR" to "$LIB_FOLDER
wget -P $LIB_FOLDER $ANTLR_ULR
