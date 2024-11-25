# Clone Repository #

```bash
ssh-keygen -t ed25519 -C "your-name@domain.com"
ssh-add ~/.ssh/github
git clone git@github.com:your-name/pace-me.git
```

# Install Dependencies #

```bash
sudo apt install ghc
sudo apt install cabal-install
sudo apt-get install libpcre3 libpcre3-dev
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
