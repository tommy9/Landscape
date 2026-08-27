#!/usr/bin/env python3
"""Capture and compare the completed Landscape screen in headless Beebium."""

from __future__ import annotations

import argparse
import json
import sys
import time
from pathlib import Path

CPU_HZ = 2_000_000


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("mode", choices=("baseline", "check"))
    parser.add_argument("--workspace", type=Path, required=True)
    parser.add_argument("--disk", type=Path, default=Path("Landscape.ssd"))
    parser.add_argument("--output-dir", type=Path, default=Path(".beebium-test"))
    parser.add_argument("--baseline", type=Path)
    parser.add_argument("--server", type=Path, help="Beebium server binary or installation")
    parser.add_argument("--mos", type=Path, help="MOS 1.20 ROM when not bundled with the server")
    parser.add_argument("--rom-dir", type=Path, help="Directory containing acorn-dfs_2_26.rom")
    parser.add_argument("--marker", default="Total time")
    parser.add_argument("--emulated-timeout", type=float, default=60.0)
    parser.add_argument("--wall-timeout", type=float, default=60.0)
    return parser.parse_args()


def screen_text(bbc) -> str:
    value = bbc.video.screen_text().text
    return "\n".join(value) if isinstance(value, list) else value


def find_marker_band(reading, marker: str, frame_width: int) -> tuple[int, int, int, int]:
    matches = [run for run in reading.runs if marker in run.text]
    if not matches:
        raise RuntimeError(f"Completion marker {marker!r} was not found in held screen")
    x = min(run.bounds.x for run in matches)
    y = min(run.bounds.y for run in matches)
    bottom = max(run.bounds.y + run.bounds.height for run in matches)
    return x, y, frame_width - 1, bottom - 1


def find_dfs_rom(explicit_dir: Path | None) -> Path:
    if explicit_dir:
        candidate = explicit_dir.resolve() / "acorn-dfs_2_26.rom"
    else:
        try:
            import beebium.server
        except ImportError as exc:
            raise RuntimeError("Use --rom-dir to locate acorn-dfs_2_26.rom") from exc
        candidate = (
            Path(beebium.server.__file__).resolve().parent
            / "_bundle" / "share" / "beebium" / "roms" / "acorn-dfs_2_26.rom"
        )
    if not candidate.is_file():
        raise RuntimeError(f"DFS ROM not found: {candidate}; use --rom-dir")
    return candidate


def launch_and_capture(args: argparse.Namespace):
    try:
        from beebium.client import Beebium
        from PIL import ImageDraw
    except ImportError as exc:
        raise RuntimeError(
            "Install beebium, beebium-server, and Pillow in an isolated Python environment"
        ) from exc

    workspace = args.workspace.resolve()
    disk = (workspace / args.disk).resolve() if not args.disk.is_absolute() else args.disk
    if not disk.is_file():
        raise RuntimeError(f"Disc image not found: {disk}")

    dfs = find_dfs_rom(args.rom_dir)
    launch = {
        "extra_args": [
            "--fdc", "acorn-1770",
            "--sideways", f"14:rom:{dfs}",
            "--auto-boot",
            "--floppy", f"0:{disk}",
        ],
        "startup_timeout": 20.0,
    }
    if args.server:
        launch["server"] = str(args.server.resolve())
    if args.mos:
        launch["mos_filepath"] = args.mos.resolve()
    with Beebium.launch(**launch) as bbc:
        bbc.debugger.ensure_running()
        bbc.system.set_speed_multiplier(0.0)
        start_cycles = bbc.debugger.cycle_count
        cycle_limit = start_cycles + int(args.emulated_timeout * CPU_HZ)
        wall_deadline = time.monotonic() + args.wall_timeout

        while bbc.debugger.cycle_count < cycle_limit and time.monotonic() < wall_deadline:
            if args.marker in screen_text(bbc):
                break
            time.sleep(0.02)
        else:
            raise RuntimeError(
                f"Landscape did not display {args.marker!r} within "
                f"{args.emulated_timeout:g} emulated seconds"
            )

        bbc.debugger.stop()
        hold = bbc.video.hold_screen(include_frame=True)
        try:
            if hold.frame is None:
                raise RuntimeError("Beebium returned a screen hold without a frame")
            reading = bbc.video.screen_text(hold_id=hold.hold_id)
            completion_text = next(
                (run.text for run in reading.runs if args.marker in run.text),
                args.marker,
            )
            raw = hold.frame.to_pil_image()
            normalized = raw.copy()
            band = find_marker_band(reading, args.marker, raw.width)
            ImageDraw.Draw(normalized).rectangle(band, fill=(0, 0, 0, 255))
            metadata = {
                "frame_number": hold.frame.frame_number,
                "cycle_count": hold.frame.cycle_count,
                "size": [raw.width, raw.height],
                "masked_band": list(band),
                "marker": args.marker,
                "completion_text": completion_text,
            }
            return raw, normalized, metadata
        finally:
            bbc.video.release_screen(hold.hold_id)


def main() -> int:
    args = parse_args()
    workspace = args.workspace.resolve()
    output_dir = (workspace / args.output_dir).resolve() if not args.output_dir.is_absolute() else args.output_dir
    baseline = args.baseline or workspace / "tests" / "baselines" / "landscape.normalized.png"
    if not baseline.is_absolute():
        baseline = (workspace / baseline).resolve()
    output_dir.mkdir(parents=True, exist_ok=True)

    raw, normalized, metadata = launch_and_capture(args)
    raw.save(output_dir / "actual.png")
    normalized.save(output_dir / "actual.normalized.png")
    (output_dir / "actual.json").write_text(json.dumps(metadata, indent=2) + "\n")

    if args.mode == "baseline":
        baseline.parent.mkdir(parents=True, exist_ok=True)
        normalized.save(baseline)
        print(f"Baseline written: {baseline}")
        return 0

    if not baseline.is_file():
        raise RuntimeError(f"Baseline not found: {baseline}; review and run baseline mode first")

    from PIL import Image, ImageChops

    expected = Image.open(baseline).convert("RGBA")
    if expected.size != normalized.size:
        raise RuntimeError(f"Frame size changed: expected {expected.size}, got {normalized.size}")
    diff = ImageChops.difference(expected, normalized)
    rgb_diff = diff.convert("RGB")
    bbox = rgb_diff.getbbox()
    if bbox is None:
        print("Visual regression check passed")
        return 0
    diff.save(output_dir / "diff.png")
    changed = sum(1 for pixel in rgb_diff.getdata() if pixel != (0, 0, 0))
    print(f"Visual regression check failed: {changed} pixels differ; bounds={bbox}", file=sys.stderr)
    print(f"Evidence: {output_dir / 'diff.png'}", file=sys.stderr)
    return 1


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except RuntimeError as exc:
        print(f"error: {exc}", file=sys.stderr)
        raise SystemExit(2)
