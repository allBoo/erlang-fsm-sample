#!/bin/bash

fpc $@".pas" && rm $@".o" && ./"$@" && rm $@

