#!/bin/bash

if [ -d "$HOME/.opam/system/lib/frp" ]; then
  rm -r $HOME/.opam/system/lib/frp
fi
