# emacs-config

## Emacs Setup

```
# Download and install Doom
rm -rf .emacs.d
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install

# Symlinks and Org mode
git clone git@github.com:cadrake/emacs-config.git
cd ~/.config
rm -rf doom
ln -s ~/emacs-config doom
~/.emacs.d/bin/doom sync
mkdir -p ~/Documents/org/tasks
```
