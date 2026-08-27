---
name: beebium-visual-testing
description: Build and visually regression-test this BBC Micro Landscape project by auto-booting Landscape.ssd in headless Beebium, capturing the completed display, and comparing it with a baseline while ignoring the changing elapsed-time text.
---

# Beebium visual testing

Use this skill when changing the Landscape BBC Micro program and checking that its rendered output did not regress.

## Workflow

1. Check `git status` and preserve unrelated work.
2. Build the disc using the shell-equivalent part of `.vscode/tasks.json`:

   ```text
   BeebAsm -v -i Landscape.asm -do Landscape.ssd -opt 3
   ```

   The VS Code-only `Create source map` dependency is editor support, not required to produce the SSD from a shell.
3. Run `scripts/visual_regression.py check` from this skill directory. Pass the workspace path and, when automatic discovery cannot find them, the Beebium server installation and ROM directory.
4. Inspect the generated actual and diff PNGs when the comparison fails. Do not update the baseline until the visual change has been reviewed and accepted.
5. Create or intentionally replace the baseline with the `baseline` command only when the user authorizes accepting the current rendering.

## Capture invariant

Do not rely on a fixed wall-clock sleep. Run Beebium unpaced and wait until the display contains `Total time`, bounded by emulated cycles. Capture through `hold_screen(include_frame=True)` so the pixels and detected text describe the same instant.

The helper masks the horizontal text band beginning at the detected `Total time` run. This removes the changing elapsed-time value while retaining the rest of the landscape. A missing completion marker is a failed run, not a reason to capture an arbitrary frame.

## Prerequisites

The Python environment needs `beebium`, `beebium-server`, Pillow, and currently `grpcio>=1.76` (Beebium 0.1.6 can otherwise resolve an older incompatible gRPC runtime). Beebium may instead be supplied as a checkout/server installation through its normal discovery options. Prefer an isolated environment; do not install packages globally without approval.

Run `scripts/visual_regression.py --help` for paths and overrides. Generated evidence belongs under `.beebium-test/`, which remains uncommitted. The accepted, versioned baseline is `tests/baselines/landscape.normalized.png` unless overridden.
