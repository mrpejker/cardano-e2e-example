FROM joinplank/plutus-apps:79020797819b700009558ffea5fe1168026acb8e

# Copy cabal files
COPY *.cabal cabal.project /build/

# Build only dependencies so they can be cached. Can't use escrow-pab here as escrow is a dependecy of escrow-pab
RUN nix-shell --run "cd /build && cabal build --dependencies-only escrow"

# needed escrow-pab source files to compile
COPY ./app /build/app
COPY ./src /build/src

# Build dependencies of escrow-pab. This can probably be skipped, as any changes to the escrow will invalidate the cache anyways
RUN nix-shell --run "cd /build && cabal build --dependencies-only escrow-pab"

RUN nix-shell --run "cd /build && cabal build escrow-pab"

RUN mkdir -p /build/artifacts

RUN nix-shell --run "cd /build && cabal list-bin escrow-pab | xargs cp -t /build/artifacts/"

WORKDIR /app

EXPOSE 9080

RUN cp /build/artifacts/escrow-pab ./escrow-pab

COPY /config ./config

CMD ./escrow-pab --config ./config/pab-config-docker-preprod.yaml migrate && \
    ./escrow-pab --config ./config/pab-config-docker-preprod.yaml webserver
