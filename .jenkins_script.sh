

git submodule update --init
cabal sandbox init
cabal install -ffusion --enable-tests ./ http-conduit-1.9.6 handa-gdata-0.6.9.1 --force-reinstalls 

