# Clone Repository #

```bash
ssh-keygen -t ed25519 -C "your-name@domain.com"

# In Termux
eval $(ssh-agent -s)

ssh-add ~/.ssh/github

# Check the key has been added:
ssh-add -l

git clone git@github.com:dmitnin/pace-me.git
cd pace-me

# To fix the error:
# fatal: detected dubious ownership in repository at '/PATH_TO/pace-me'
git config --global --add safe.directory /PATH_TO/pace-me

# Configure git user
git config --global user.email "you@example.com"
git config --global user.name "Your Name"
```

## Install System Packages ##

### Linux ###

```bash
sudo apt install ghc
sudo apt install cabal-install
sudo apt-get install libpcre3 libpcre3-dev
```

### Termux ###

```bash
pkg install ghc
pkg install cabal-install
pkg install pcre
```

cabal install pcre-light

# Build and Run #

```bash
cabal build
cabal run
```

# Linter #

```bash
sudo apt install hlint
hlint src/Main.hs
```

# Formatter #

```bash
cabal install fourmolu
fourmolu --mode inplace src/Main.hs
```
