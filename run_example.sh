stack build
echo "DONE WITH BUILD"
stack exec end-to-end > ex.cnf
echo "DONE WITH SAT GEN, file was this many lines long:"
wc -l ex.cnf
head -1 ex.cnf
cryptominisat5_simple --verb=0 ex.cnf  > ex.res
echo "DONE WITH SAT SOLVING"
stack exec decode
rm ex.cnf; rm ex.res
