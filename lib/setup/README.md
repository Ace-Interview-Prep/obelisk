# jenga-setup

Cabal `Setup.hs` hooks for Jenga projects. Provides reusable build logic so downstream packages only need a one-line `Setup.hs`.

## Modules

### `Jenga.Setup.Backend`

Pre-build hook that symlinks frontend assets into `backend/data/`:

- `frontend/data/frontend.jsexe` -> `backend/data/frontend.jsexe`
- `frontend/data/static` -> `backend/data/static`

### `Jenga.Setup.Frontend`

Pre-build hook that cross-compiles the frontend with GHCJS (in a background thread), then post-build symlinks the output into `frontend/data/`:

- GHCJS-compiled `frontend.jsexe` -> `frontend/data/frontend.jsexe`
- `static/generated/data/static` -> `frontend/data/static`

Looks for `javascript-unknown-ghcjs` or `javascript-unknown-ghcjs-cabal` on `$PATH` (provided by the nix shell).

### `Jenga.Setup.Static`

Pre-build hook that generates static asset manifests:

1. Runs `static/generate` to produce hashed static assets
2. Computes SHA-256 hashes for cache-busting filenames
3. Generates `Jenga.Generated.Static.Instances` module with `StaticFile` instances

### `Jenga.Setup.Utils`

Shared utilities:

- `findProjectRoot` -- walks up from the current directory to find `cabal.project`
- `symlink` -- idempotent symlink creation (removes stale links, skips existing directories)

## Usage

Add `jenga-setup` to your package's `custom-setup` dependencies:

```cabal
build-type: Custom

custom-setup
  setup-depends:
    base >= 4.7 && < 5,
    Cabal >= 3.0 && < 4,
    jenga-setup
```

Then write a one-line `Setup.hs`:

```haskell
import Jenga.Setup.Backend (main)
```

Replace `Backend` with `Frontend` or `Static` as appropriate.
