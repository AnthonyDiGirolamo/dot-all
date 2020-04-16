#!/usr/bin/bash
which tmux && tmux list-sessions && exec tmux at
which tmux && exec tmux
which fish && exec fish
which zsh && exec zsh
exec bash
