FROM nixos/nix:2.7.0 as builder

WORKDIR /build

RUN git clone https://github.com/joinplank/plutus-apps.git

WORKDIR /build/plutus-apps

RUN git checkout 79020797819b700009558ffea5fe1168026acb8e

RUN echo "substituters  = https://cache.iog.io https://iohk.cachix.org https://cache.nixos.org/" >> /etc/nix/nix.conf \
    && echo "trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" >> /etc/nix/nix.conf \
    && echo "extra-experimental-features = nix-command flakes" >> /etc/nix/nix.conf

RUN nix-shell

RUN nix-shell --run "cabal update"

RUN nix-shell --run "cabal install cabal-plan --constraint='cabal-plan +exe'"

# Copy cabal files
COPY *.cabal cabal.project /build/

# Build only dependencies so they can be cached. Can't use escrow-pab here as escrow is a dependecy of escrow-pab
RUN nix-shell --run "cd /build && cabal build --dependencies-only escrow"

COPY . /build

# Build dependencies of escrow-pab. This can probably be skipped, as any changes to the escrow will invalidate the cache anyways
RUN nix-shell --run "cd /build && cabal build --dependencies-only escrow-pab"

RUN nix-shell --run "cd /build && cabal build escrow-pab"

RUN mkdir -p /build/artifacts

RUN nix-shell --run "cd /build && cabal list-bin escrow-pab | xargs cp -t /build/artifacts/"

WORKDIR /app

EXPOSE 9080

RUN cp /build/artifacts/escrow-pab ./escrow-pab

COPY /config ./config

CMD ./escrow-pab --config ./config/pab-config-preprod.yaml migrate && \
    ./escrow-pab --config ./config/pab-config-preprod.yaml webserver
