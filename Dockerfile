from amazonlinux:latest

run echo 'set editing-mode vi' > ~/.inputrc
run yum update -y
run yum install -y \
        git \
        gcc \
        libffi-devel \
        gmp-devel \
        zlib-devel \
        make \
        tar \
        gzip \
        xz \
        ncurses-devel
workdir /home/root
run mkdir /src
run curl -L https://downloads.haskell.org/~ghc/8.6.5/ghc-8.6.5-src.tar.xz > /src/ghc
workdir /src
run tar -xf ghc --totals
run curl -LO https://downloads.haskell.org/~ghc/8.4.4/ghc-8.4.4-x86_64-fedora27-linux.tar.xz
run tar -xf ghc-8.4.4-x86_64-fedora27-linux.tar.xz --totals
run ln -s /usr/lib64/libgmp.so.10 /usr/lib64/libgmp.so.3 && ldconfig
workdir /src/ghc-8.4.4
run ./configure
run make install
workdir /src
run curl -LO https://downloads.haskell.org/~cabal/cabal-install-3.0.0.0/cabal-install-3.0.0.0.tar.gz
run tar -xzf cabal-install-3.0.0.0.tar.gz --totals
workdir /src/cabal-install-3.0.0.0
run yum install -y which
run ./bootstrap.sh
env PATH "$PATH:/root/.cabal/bin"
workdir /src/ghc-8.6.5
run cabal update
run cabal v1-install alex happy
run ./configure
run make
run make install
run rm -rf /src/ghc-8.4.4 /src/ghc-*.tar.xz /src/ghc-8.6.5
run cabal update
volume /src/refl-club
workdir /src/refl-club
add . /src/refl-club
run cabal new-update
run cabal new-configure
run cabal new-build
