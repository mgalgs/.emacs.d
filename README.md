My emacs configuration

# Installation

```shell
cd
git clone https://github.com/mgalgs/.emacs.d.git
cd .emacs.d
git submodule update --init
```

I only test this on `emacs` from `git`, plus a few custom patches.  So you
might want to do the same:

```shell
git clone https://github.com/mgalgs/emacs.git
cd emacs
./autogen.sh
./configure --prefix=$HOME/opt/emacs-git && make -j8 && make install
```

Then add `$HOME/opt/emacs-git/bin` to your `PATH`.
