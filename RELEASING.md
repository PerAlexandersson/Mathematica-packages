# Releasing

The library is one paclet, `PerAlexandersson/MathematicaPackages`, described by
`PacletInfo.wl`. Versions follow semantic versioning: incompatible changes to a public
symbol of a supported package bump the major version (minor while below 1.0), new public
functionality bumps the minor version, and fixes bump the patch version.

## Checklist

1. `wolframscript -file Tests/RunTests.m` passes.
2. Update `"Version"` in `PacletInfo.wl` and add a changelog entry.
3. `wolframscript -file Scripts/BuildPaclet.m` builds
   `build/PerAlexandersson__MathematicaPackages-<version>.paclet` and verifies that every
   registered context loads from the extracted archive in a fresh kernel and that the
   `Data/` assets are found.
4. Tag the release commit `v<version>` and attach the archive to the GitHub release.

`build/` and `*.paclet` are ignored by Git.
