export NIX_PATH:=nixpkgs=$(shell nix-build --no-out-link ./nixpkgs.nix):todo-app=$(shell pwd)

todo-frontend.build:
	nix-build -A haskell.packages.ghcjsHEAD.todo-frontend

todo-frontend.shell:
	cd hs-pkgs/todo-frontend && \
	nix-shell --command 'cabal configure; return'

todo-api.shell:
	cd hs-pkgs/todo-api && \
	nix-shell --command 'cabal configure; return'

todo-api-client.shell:
	cd hs-pkgs/todo-api-client && \
	nix-shell --command 'cabal configure; return'
