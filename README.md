# IntPak

Simple and slow integral package for Gaussian basis functions.[^1][^2]
IntPak has the ability to compute various types of one-, two-, three-, and four-electron integrals using various operators.[^3][^4][^5]
IntPak is extremely slow but the implementation is straightforward.

**Contributors:**
- [Pierre-Francois Loos](https://pfloos.github.io/WEB_LOOS)

# Installation guide
The IntPak software can be downloaded on GitHub as a Git repository
```
git clone https://github.com/pfloos/IntPak.git
```
The repository should be self-contained as I have added the required subroutines from the library [slatec](https://netlib.org/slatec/).
BLAS and LAPACK might be also required.

# Compile the code

```
cd src; make
```

# Run the code

```
./bin/IntPak
```
# Set up the options

The options are gathered in `input/int` and it looks like this

```
# Debuggin mode?
  F
# Chemist notation for two-electron integral?
  F
# Exposant of the Slater geminal
  1.0
# One-electron integrals: Ov Kin Nuc
                          T  T   T
# Two-electron integrals: ERI F12 Yuk Erf
                          T   F   F   F
# Three-electron integrals: Type1 Type2 Type3
                            F     F     F
# Four-electron integrals: Type1 Type2 Type3
                           F     F     F
```
In this case, the usual one-electron integrals are going to be computed (overlap, kinetic and nuclear attraction) as well as the electron repulsion integrals (ERIs). Other two-electron integrals over the operator f12 (f12 being a Gaussian or Slater geminal), f12/r12, and erf(r12)/r12 can also be computed. For the three- and four-electron integrals, see [3][4][5].
The basis set is given in `input/basis` while the molecular geometry is given in `input/molecule`.

The `molecule` file looks like this for N2 (the coordinates are given in bohr):

```
# nAt nEla nElb nCore nRyd
    2    7    7     0    0
# Znuc   x            y           z
  N        0.0000000000         0.0000000000         1.0400863244
  N        0.0000000000         0.0000000000        -1.0400863244
```

A concrete example is given in the repository.
Additional tuning can be done in the main source file `src/IntPak.f90`.
In particular, one can tune the coefficients and exponents of the Gaussian geminals that are used to fit the Slater geminal.

Have fun!

[^1]: [P. M. W. Gill, Adv. Quantum Chem. 25, 141 (1994).](https://doi.org/10.1016/S0065-3276(08)60019-2)
[^2]: [R. Ahlrichs, Phys. Chem. Chem. Phys. 8, 3072 (2006).](https://doi.org/10.1039/B605188J)
[^3]: [G. M. J. Barca, P. F. Loos and P. M. W. Gill, J. Chem. Theory Comput. 12, 1735 (2016).](https://pubs.acs.org/doi/10.1021/acs.jctc.6b00130)
[^4]: [G. M. J. Barca and P. F. Loos, Adv. Quantum Chem. 76, 147 (2018).](http://dx.doi.org/10.1016/bs.aiq.2017.03.004)
[^5]: [G. M. J. Barca and P. F. Loos, J. Chem. Phys. 147, 024103 (2017).](https://doi.org/10.1063/1.4991733)
