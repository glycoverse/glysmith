# glysmith Docker image

The image contains the released glysmith package, all packages in its `Suggests`,
the core Glycoverse packages (including `glyread`), and Pandoc for HTML reports.
It extends the [Glycoverse R 4.6.2 image](https://hub.docker.com/r/fubin1999/glycoverse),
pinned by digest, and provides R and Rscript on Linux `amd64` and `arm64`.

Pull the image from [Docker Hub](https://hub.docker.com/r/fubin1999/glysmith)
as `fubin1999/glysmith`, or from [GitHub Container Registry](https://github.com/glycoverse/glysmith/pkgs/container/glysmith)
as `ghcr.io/glycoverse/glysmith`.

## Run an analysis

```sh
docker pull fubin1999/glysmith:latest
docker run --rm -it \
  --mount "type=bind,source=$(pwd),target=/workspace" \
  fubin1999/glysmith:latest
```

Your current directory is available at `/workspace`, which is also R's working
directory. To run a script saved there:

```sh
docker run --rm \
  --mount "type=bind,source=$(pwd),target=/workspace" \
  fubin1999/glysmith:latest Rscript analysis.R
```

On Linux, add `--user "$(id -u):$(id -g)"` to write bind-mounted files with your
own user and group. The default user is `glycoverse` (UID/GID 1000).

For reports, use `polish_report(result, "report.html", open = FALSE)` and open
the saved HTML file on your host. AI features use your own provider credentials;
pass an existing environment variable with, for example, `--env DEEPSEEK_API_KEY`.
Analyses using external annotation services still need network access.

## Tags and installed versions

- `0.13.0` contains glysmith 0.13.0 from the GitHub release tagged `v0.13.0`.
- `latest` and `r4.6.2` track the release marked **Latest** on GitHub.
- `build-<run-id>-<attempt>` identifies a particular CI publication.

Rebuilds can update dependency packages even when the glysmith version is
unchanged. Use an image digest to reproduce a specific environment:

```sh
docker buildx imagetools inspect ghcr.io/glycoverse/glysmith:latest
# docker pull ghcr.io/glycoverse/glysmith@sha256:<digest>
```

Inspect the package manifest and run the same offline check used during builds:

```sh
docker run --rm ghcr.io/glycoverse/glysmith:latest cat /opt/glysmith/package-versions.tsv
docker run --rm --network none ghcr.io/glycoverse/glysmith:latest \
  Rscript /opt/glysmith/smoke-test.R
```

The smoke test loads glysmith and its dependencies, checks the default blueprint's
packages, runs PCA and differential analysis on synthetic data, exports PNG plots
and tables, and renders an HTML report. `/opt/glysmith/build-session.txt` records
the R session and `/opt/glysmith/package-reference.txt` records the package source.

## Build and publish

From the repository root:

```sh
docker build --tag glysmith:local .
docker run --rm --network none glysmith:local Rscript /opt/glysmith/smoke-test.R
```

By default, `GLYSMITH_REF=glysmith` installs the release from
[r-universe](https://glycoverse.r-universe.dev/glysmith). To select a GitHub tag:

```sh
docker build --build-arg GLYSMITH_REF=glycoverse/glysmith@v0.13.0 \
  --build-arg NCPUS=4 --tag glysmith:0.13.0 .
```

The Dockerfile installs the selected remote package. To refresh dependencies in
a local build, use `docker build --no-cache-filter glysmith --tag glysmith:local .`.
The installer refreshes the package index and upgrades the core packages from
the Glycoverse r-universe along with glysmith's required and optional dependencies.

Publishing a GitHub release triggers the **Docker** workflow. It installs
glysmith from the tagged commit and checks the installed version against the
release tag, allowing a leading `v`. Both Linux architectures must pass the
offline smoke test. The workflow then publishes the version tag to GHCR and
copies the same multi-platform image to Docker Hub by digest. Pre-releases
also get a version tag. Only the current GitHub **Latest** release updates
the moving tags. Package manifests are available in the workflow artifacts.

Pull requests that change Docker files build and test both architectures.
Changes to Docker files on `main` and manual runs on `main` with **publish**
enabled also publish to GHCR.

GHCR uses the workflow's `GITHUB_TOKEN` with `packages: write`. For Docker Hub,
set the repository Actions secret `DOCKERHUB_TOKEN` to a Docker Hub access token
for `fubin1999` with write access to `fubin1999/glysmith`.

Build files are MIT licensed. Bundled R packages and system software retain
their respective licenses.
