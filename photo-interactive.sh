#!/bin/zsh

for img in $1/*.JPG; do fim $img; echo "add-media to shortblog?"; read -q && ~/shortblog add-media "$img"; done
