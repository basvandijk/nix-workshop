
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
# Deploying to containers for testing
################################################################################

.PHONY: containerized-nixtodo-net.create
containerized-nixtodo-net.create:
	nixops create -d containerized-nixtodo-net \
	  '<nixtodo/modules/nixtodo-net.nix>' '<nixtodo/modules/nixtodo-net-containerized.nix>'

.PHONY: containerized-nixtodo-net.modify
containerized-nixtodo-net.modify:
	nixops modify -d containerized-nixtodo-net \
	  '<nixtodo/modules/nixtodo-net.nix>' '<nixtodo/modules/nixtodo-net-containeried.nix>'

.PHONY: containerized-nixtodo-net.info
containerized-nixtodo-net.info:
	nixops info -d containerized-nixtodo-net

.PHONY: containerized-nixtodo-net.build
containerized-nixtodo-net.build:
	nixops deploy -d containerized-nixtodo-net --build-only

.PHONY: containerized-nixtodo-net.copy
containerized-nixtodo-net.copy:
	nixops deploy -d containerized-nixtodo-net --copy-only

.PHONY: containerized-nixtodo-net.deploy
containerized-nixtodo-net.deploy:
	nixops deploy -d containerized-nixtodo-net

.PHONY: containerized-nixtodo-net.destroy
containerized-nixtodo-net.destroy:
	nixops destroy -d containerized-nixtodo-net

.PHONY: containerized-nixtodo-net.delete
containerized-nixtodo-net.delete:
	nixops delete -d containerized-nixtodo-net

.PHONY: containerized-backend.ssh
containerized-backend.ssh:
	nixops ssh -d containerized-nixtodo-net backend

.PHONY: containerized-backend.destroy
containerized-backend.destroy:
	nixops destroy -d containerized-nixtodo-net --include backend


################################################################################
# Deploying to production
################################################################################

.PHONY: nixtodo-net.create
nixtodo-net.create:
	nixops create -s secrets/state.nixops -d nixtodo-net \
	  '<nixtodo/modules/nixtodo-net.nix>' '<nixtodo/modules/nixtodo-net-aws.nix>'

.PHONY: nixtodo-net.modify
nixtodo-net.modify:
	nixops modify -s secrets/state.nixops -d nixtodo-net \
	  '<nixtodo/modules/nixtodo-net.nix>' '<nixtodo/modules/nixtodo-net-aws.nix>'

.PHONY: nixtodo-net.info
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
