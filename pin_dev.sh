#!/bin/bash
for p in *.opam; do
  opam pin add -y -n ${p%.opam}.~dev .
done
