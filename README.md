# emacs-config

# Emacs setup
## MacOS
```

```
## WSL2/Linux
```

```
## Both
```
# Download and install Doom
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install

# Symlinks and Org mode
rm -rf ~/.doom.d
git clone https://github.com/cadrake/emacs-config.git
ln -s ~/emacs-config ~/.doom.d
doom sync
mkdir -p ~/Documents/org/tasks
```
