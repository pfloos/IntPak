# IntPak

Simple and slow integral package for Gaussian basis functions.[^1][^2]
IntPak has the ability to compute various types of one-, two-, three-, and four-electron integrals using various operators.[^3]
IntPak is extremely slow but the implementation is straightforward

**Contributors:**
- [Pierre-Francois Loos](https://pfloos.github.io/WEB_LOOS)

# Installation guide
The IntPak software can be downloaded on GitHub as a Git repository
```
git clone https://github.com/pfloos/IntPak.git
```

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
                          F  F   F
# Two-electron integrals: ERI F12 Yuk Erf
                          F   F   T   F
# Three-electron integrals: Type1 Type2 Type3
                            F     F     F
# Four-electron integrals: Type1 Type2 Type3
                           F     F     F
```
The basis set is given in `input/basis` while the molecular geometry is given in `input/molecule`.
A concrete example is given in the repository.
Additional tuning can be done in the main source file `src/IntPak.f90`.


Have fun!

[^1]: [P. M. W. Gill, Adv. Quantum Chem. 25, 141 (1994).](https://doi.org/10.1016/S0065-3276(08)60019-2)
[^2]: [R. Ahlrichs, Phys. Chem. Chem. Phys. 8, 3072 (2006).](https://doi.org/10.1039/B605188J)
[^3]: [G. M. J. Barca, P. F. Loos and P. M. W. Gill, J. Chem. Theory Comput. 12, 1735 (2016).](https://pubs.acs.org/doi/10.1021/acs.jctc.6b00130)
[^4]: [G. M. J. Barca and P. F. Loos, Adv. Quantum Chem. 76, 147 (2018).](http://dx.doi.org/10.1016/bs.aiq.2017.03.004)
[^5]: [G. M. J. Barca and P. F. Loos, J. Chem. Phys. 147, 024103 (2017).](https://doi.org/10.1063/1.4991733)
