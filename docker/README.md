# glysmith Docker image

The image contains the released glysmith package, all packages in its `Suggests`,
the core Glycoverse packages (including `glyread`), and Pandoc for HTML reports.
It extends the [Glycoverse R 4.6.2 image](https://hub.docker.com/r/fubin1999/glycoverse),
pinned by digest, and provides R and Rscript on Linux `amd64` and `arm64`.

## Run an analysis

```sh
docker pull ghcr.io/glycoverse/glysmith:latest
docker run --rm -it \
  --mount "type=bind,source=$(pwd),target=/workspace" \
  ghcr.io/glycoverse/glysmith:latest
```

Your current directory is available at `/workspace`, which is also R's working
directory. To run a script saved there:

```sh
docker run --rm \
  --mount "type=bind,source=$(pwd),target=/workspace" \
  ghcr.io/glycoverse/glysmith:latest Rscript analysis.R
```

On Linux, add `--user "$(id -u):$(id -g)"` to write bind-mounted files with your
own user and group. The default user is `glycoverse` (UID/GID 1000).

For reports, use `polish_report(result, "report.html", open = FALSE)` and open
the saved HTML file on your host. AI features use your own provider credentials;
pass an existing environment variable with, for example, `--env DEEPSEEK_API_KEY`.
Analyses using external annotation services still need network access.

## Tags and installed versions

- `latest`: most recently published build of the released package.
- `0.13.0` (for example): build of that glysmith version.
- `r4.6.2`: most recently published build using R 4.6.2.
- `build-<run-id>-<attempt>`: a particular CI publication.

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

The Dockerfile installs the selected remote package; local R source edits are
not included. To refresh the release and its dependencies, use `--no-cache`.
The build uses [`pak`](https://pak.r-lib.org/reference/pkg_install.html) to resolve
CRAN, Bioconductor, and Glycoverse dependencies and install system requirements.

The **Docker** GitHub Actions workflow builds and tests both architectures on
native runners. Pull requests build and test only. Changes to Docker files on
`main`, published stable releases, and manual runs on `main` with **publish**
enabled also publish to GitHub Container Registry (GHCR). Release builds select the exact GitHub tag.
Both platforms must pass the offline smoke test and report the same glysmith
version before the shared tags are updated. Installed package manifests are
saved as workflow artifacts.

Publishing uses the workflow's temporary `GITHUB_TOKEN` with `packages: write`;
no personal access token is needed. After the first publication, set the
package's visibility to public in GitHub to allow pulls without signing in.
Forks must change the image name to their own namespace.

Build files are MIT licensed. Bundled R packages and system software retain
their respective licenses.
