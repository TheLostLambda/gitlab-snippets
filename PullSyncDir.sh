#!/bin/sh
rsync -hiavuzP --include='.zshrc' --include='.emacs*' --include='.bin/' --include='.ssh/' --exclude='*.~lock*' --exclude='/.*' --exclude='Downloads' --exclude='Desktop' tll@thelostlambda.xyz:/home/tll/Sync/ ~/ "$@"
