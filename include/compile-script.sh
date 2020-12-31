mkfifo pipe

tee compile-output.txt < pipe &
ghc -i../include -main-is $1 -fdiagnostics-color=always $1.hs -o $1 2> pipe
test $? -eq 0
