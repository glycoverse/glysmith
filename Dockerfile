# syntax=docker/dockerfile:1

ARG BASE_IMAGE=fubin1999/glycoverse:4.6.2@sha256:53f58c87994669abbe79a1a7e6763c7a8025381ede98c4cea8cf008f9ff3a523
FROM ${BASE_IMAGE} AS base

USER 0
LABEL org.opencontainers.image.title="glysmith" \
      org.opencontainers.image.description="Ready-to-use glysmith analysis and HTML reporting environment" \
      org.opencontainers.image.source="https://github.com/glycoverse/glysmith" \
      org.opencontainers.image.licenses="MIT"

ARG NCPUS=2
ENV MAKEFLAGS="-j${NCPUS}" \
    R_BUILD_NCPUS=${NCPUS}

RUN apt-get update \
    && apt-get install -y --no-install-recommends pandoc \
    && rm -rf /var/lib/apt/lists/*

USER 1000:1000
FROM base AS glysmith

USER 0
COPY docker/install.R /opt/glysmith/install.R
ARG GLYSMITH_REF=glysmith
ARG TARGETARCH
# Keep pak's architecture-specific metadata separate for each platform.
RUN --mount=type=cache,id=glysmith-r46-${TARGETARCH},target=/root/.cache,sharing=locked \
    GLYSMITH_REF="${GLYSMITH_REF}" Rscript /opt/glysmith/install.R \
    && rm -rf /tmp/downloaded_packages /tmp/Rtmp* /var/lib/apt/lists/*

COPY docker/smoke-test.R /opt/glysmith/smoke-test.R
USER 1000:1000
WORKDIR /workspace
RUN Rscript /opt/glysmith/smoke-test.R

CMD ["R", "--no-save", "--no-restore"]
