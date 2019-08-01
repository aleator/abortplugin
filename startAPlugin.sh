#!/bin/bash
sos src/AbortPlugin.hs -c "stack build && stack ghc -- -fplugin=AbortPlugin $1"
