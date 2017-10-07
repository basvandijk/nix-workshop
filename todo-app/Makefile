
export NIX_PATH:=nixpkgs=$(shell nix-build --no-out-link ./nixpkgs.nix):todo-app=$(shell pwd)

help:
	cat Makefile


################################################################################
# Development
################################################################################

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

secrets/postgresql-todo-role-password:
	openssl rand -base64 32 > $@

secrets/dhparams.pem:
	openssl dhparam 4096 -out $@


################################################################################
# Deployment
################################################################################

.PHONY: todo-list-net.create
todo-list-net.create:
	nixops create -s secrets/state.nixops -d todo-list-net \
	  '<todo-app/modules/todo-list-net.nix>'

.PHONY: todo-list-net.build
todo-list-net.build:
	nixops deploy -s secrets/state.nixops -d todo-list-net --build-only

.PHONY: todo-list-net.copy
todo-list-net.copy:
	nixops deploy -s secrets/state.nixops -d todo-list-net --copy-only

.PHONY: todo-list-net.deploy
todo-list-net.deploy:
	nixops deploy -s secrets/state.nixops -d todo-list-net

.PHONY: todo-list-net.destroy
todo-list-net.destroy:
	nixops destroy -s secrets/state.nixops -d todo-list-net

.PHONY: todo-list-net.delete
todo-list-net.delete:
	nixops delete -s secrets/state.nixops -d todo-list-net

.PHONY: backend.ssh
backend.ssh:
	nixops ssh -s secrets/state.nixops -d todo-list-net backend

.PHONY: backend.destroy
backend.destroy:
	nixops destroy -s secrets/state.nixops -d todo-list-net --include backend

.PHONY: stalling-net.info
todo-list-net.info:
	nixops info -s secrets/state.nixops -d todo-list-net
