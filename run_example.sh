stack build
stack exec end-to-end > ex.cnf
cryptominisat5_simple --verb=0 ex.cnf  > ex.res
stack exec decode
# rm ex.cnf; rm ex.res
