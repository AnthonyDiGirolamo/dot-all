#!/usr/bin/env bash
TEMPDIR="${HOME}/dot-all/.cache/gawk"
FILENAME=gawk-5.2.2

mkdir -p "${TEMPDIR}"
cd "${TEMPDIR}"

! test -f "${FILENAME}.tar.xz" && curl -L -O "https://ftpmirror.gnu.org/gawk/${FILENAME}.tar.xz"
! test -f "${FILENAME}.tar.xz" && exit 1

rm -rf "${FILENAME}"
tar xf "${FILENAME}.tar.xz"

cd "${FILENAME}"
./configure \
    --disable-pma \
    --disable-debug \
    --disable-dependency-tracking \
    --without-libsigsegv-prefix \
    --prefix=$HOME/apps/gawk
make -j4
make install
