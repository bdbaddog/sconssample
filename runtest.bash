#!/bin/bash

# This code may seem structured strangely but we are
# emulating how a IBM S390 works so please bear with
# me.

export OKDIR=${PWD}/openkicks
export OKREGION=${PWD}

cd murach

scons

