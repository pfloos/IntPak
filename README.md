# IntPak

Simple and slow integral package for Gaussian basis functions.

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
