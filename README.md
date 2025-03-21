# button90
Fortran 90 library to use graphical buttons with PGPLOT

This code is an evolution of the old button library,
https://github.com/nicocardiel/button,
transformed to Fortran 90.

Code in development. It should not be used yet.

## Installation

I am currently using the following steps in macOS (with PGPLOT installed using
homebrew):
```shell
$ git clone https://github.com/nicocardiel/button90.git
$ cd button90
$ ./autogen.sh
$ ./configure FC=gfortran FCFLAGS=-Wall 'LDFLAGS=-L/opt/homebrew/opt/libx11/lib -L/opt/homebrew/opt/pgplot/lib'
$ make
$ src/samplebutton
```

It is possible to install the library (default `/usr/local/lib`) and modules
(default `/usr/local/include`)
```shell
$ sudo make install
```

An arbitrary program can be compiled using
```shell
$ gfortran -o test test.f90 \
  -I/usr/local/include \
  -L/opt/homebrew/opt/libx11/lib \
  -L/opt/homebrew/opt/pgplot/lib \
  -L/usr/local/lib \
  -lpgplot -lX11 -lbutton90
```


