nix-shell -A shells.ghc --run 'brittany --config-file .brittany.conf --write-mode inplace `find . -name "*.hs"`'
