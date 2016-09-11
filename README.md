hmatrix-fftw
============

Requires [my branch of
`vector-fftw`](https://github.com/peddie/vector-fftw) with support for
multi-dimensional FFTs.  You can clone this other repository and use

    cabal sandbox add-source /path/to/vector-fftw/

or clone `vector-fftw` next door to this repository and use the
provided `stack.yaml` file, or modify the path to `vector-fftw` in
that file.
