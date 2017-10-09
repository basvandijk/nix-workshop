
export NIX_PATH:=nixpkgs=$(shell nix-build --no-out-link ./nixpkgs.nix):nixtodo=$(shell pwd)

help:
	cat Makefile


################################################################################
# Development
################################################################################

nixtodo-frontend.build:
	nix-build -A haskell.packages.ghcjsHEAD.nixtodo-frontend

nixtodo-frontend.shell:
	cd hs-pkgs/nixtodo-frontend && \
	nix-shell --command 'cabal configure; return'

nixtodo-api.shell:
	cd hs-pkgs/nixtodo-api && \
	nix-shell --command 'cabal configure; return'

nixtodo-api-client.shell:
	cd hs-pkgs/nixtodo-api-client && \
	nix-shell --command 'cabal configure; return'

secrets/postgresql-nixtodo-role-password:
	openssl rand -base64 32 > $@

secrets/dhparams.pem:
	openssl dhparam 4096 -out $@


################################################################################
# Deployment
################################################################################

.PHONY: nixtodo-net.create
nixtodo-net.create:
	nixops create -s secrets/state.nixops -d nixtodo-net \
	  '<nixtodo/modules/nixtodo-net.nix>'

.PHONY: nixtodo-net.modify
nixtodo-net.modify:
	nixops modify -s secrets/state.nixops -d nixtodo-net \
	  '<nixtodo/modules/nixtodo-net.nix>'

.PHONY: stalling-net.info
nixtodo-net.info:
	nixops info -s secrets/state.nixops -d nixtodo-net

.PHONY: nixtodo-net.build
nixtodo-net.build:
	nixops deploy -s secrets/state.nixops -d nixtodo-net --build-only

.PHONY: nixtodo-net.copy
nixtodo-net.copy:
	nixops deploy -s secrets/state.nixops -d nixtodo-net --copy-only

.PHONY: nixtodo-net.deploy
nixtodo-net.deploy:
	nixops deploy -s secrets/state.nixops -d nixtodo-net

.PHONY: nixtodo-net.destroy
nixtodo-net.destroy:
	nixops destroy -s secrets/state.nixops -d nixtodo-net

.PHONY: nixtodo-net.delete
nixtodo-net.delete:
	nixops delete -s secrets/state.nixops -d nixtodo-net

.PHONY: backend.ssh
backend.ssh:
	nixops ssh -s secrets/state.nixops -d nixtodo-net backend

.PHONY: backend.destroy
backend.destroy:
	nixops destroy -s secrets/state.nixops -d nixtodo-net --include backend
