"""Reading and measuring rendered WAV files, with the standard library only."""
from __future__ import annotations

import array
import hashlib
import math
import struct
import sys
from pathlib import Path
from typing import Any, Dict, List

from .paths import StudioError

_CHUNK_FRAMES = 262144
_PCM, _FLOAT, _EXTENSIBLE = 1, 3, 0xFFFE


class AudioError(StudioError):
    """The file is not a WAV file this module can read."""


def _sumsq(values: "array.array") -> float:
    if hasattr(math, "sumprod"):
        return float(math.sumprod(values, values))
    return float(sum(v * v for v in values))


def _read_header(handle) -> Dict[str, Any]:
    riff = handle.read(12)
    if len(riff) < 12 or riff[:4] != b"RIFF" or riff[8:12] != b"WAVE":
        raise AudioError("not a RIFF/WAVE file")
    fmt = None
    while True:
        header = handle.read(8)
        if len(header) < 8:
            raise AudioError("WAV file has no data chunk")
        chunk_id, size = header[:4], struct.unpack("<I", header[4:])[0]
        if chunk_id == b"fmt ":
            fmt = handle.read(size)
            if size % 2:
                handle.read(1)
        elif chunk_id == b"data":
            if fmt is None or len(fmt) < 16:
                raise AudioError("WAV file has no format chunk before its data")
            tag, channels, rate, _byte_rate, block, bits = struct.unpack("<HHIIHH", fmt[:16])
            if tag == _EXTENSIBLE and len(fmt) >= 26:
                tag = struct.unpack("<H", fmt[24:26])[0]
            if tag not in (_PCM, _FLOAT) or channels < 1 or block != channels * bits // 8:
                raise AudioError(f"unsupported WAV format (tag {tag}, {bits} bits)")
            if (tag == _PCM and bits not in (16, 24, 32)) or (tag == _FLOAT and bits not in (32, 64)):
                raise AudioError(f"unsupported WAV sample size ({bits} bits)")
            return {
                "float": tag == _FLOAT,
                "channels": channels,
                "sample_rate": rate,
                "bits": bits,
                "block": block,
                "data_bytes": size,
            }
        else:
            handle.seek(size + (size % 2), 1)


def _decode(raw: bytes, info: Dict[str, Any]) -> "array.array":
    """Samples of one chunk as an array; integers are scaled to 32 bits."""
    bits = info["bits"]
    if info["float"]:
        values = array.array("f" if bits == 32 else "d")
        values.frombytes(raw)
    elif bits == 24:
        count = len(raw) // 3
        padded = bytearray(count * 4)
        padded[1::4] = raw[0::3]
        padded[2::4] = raw[1::3]
        padded[3::4] = raw[2::3]
        values = array.array("i")
        values.frombytes(bytes(padded))
    else:
        values = array.array("h" if bits == 16 else "i")
        values.frombytes(raw)
    if sys.byteorder == "big":
        values.byteswap()
    return values


def _db(value: float) -> float:
    return round(20.0 * math.log10(value), 2) if value > 0 else -math.inf


def analyze(path: Path) -> Dict[str, Any]:
    """Peak, RMS, clipping and duration of a WAV file.

    Levels are relative to full scale. ``data_sha256`` is the hash of the
    sample data only, so two renders can be compared regardless of headers.
    """
    path = Path(path)
    with open(path, "rb") as handle:
        info = _read_header(handle)
        channels = info["channels"]
        if info["float"]:
            full_scale = 1.0
        else:
            full_scale = float(2 ** (15 if info["bits"] == 16 else 31))
        clip_level = full_scale * (1.0 if info["float"] else (1.0 - 2.0 ** -(info["bits"] - 1)))
        peaks = [0.0] * channels
        sums = [0.0] * channels
        clipped = 0
        frames = 0
        digest = hashlib.sha256()
        remaining = info["data_bytes"]
        chunk_bytes = _CHUNK_FRAMES * info["block"]
        while remaining > 0:
            raw = handle.read(min(chunk_bytes, remaining))
            if not raw:
                break
            remaining -= len(raw)
            raw = raw[: len(raw) - len(raw) % info["block"]]
            if not raw:
                break
            digest.update(raw)
            values = _decode(raw, info)
            frames += len(values) // channels
            for channel in range(channels):
                part = values[channel::channels]
                high, low = max(part), min(part)
                peak = max(abs(high), abs(low))
                if peak > peaks[channel]:
                    peaks[channel] = float(peak)
                sums[channel] += _sumsq(part)
                if peak >= clip_level:
                    clipped += sum(1 for v in part if abs(v) >= clip_level)
    if frames == 0:
        raise AudioError(f"{path.name} contains no audio")
    peak = max(peaks) / full_scale
    rms = math.sqrt(sum(sums) / (frames * channels)) / full_scale
    result: Dict[str, Any] = {
        "sample_rate": info["sample_rate"],
        "channels": channels,
        "bits": info["bits"],
        "sample_format": "float" if info["float"] else "pcm",
        "frames": frames,
        "duration_seconds": round(frames / info["sample_rate"], 3),
        "peak": round(peak, 6),
        "peak_dbfs": _db(peak),
        "rms_dbfs": _db(rms),
        "channel_peaks_dbfs": [_db(p / full_scale) for p in peaks],
        "clipped_samples": clipped,
        "data_sha256": digest.hexdigest(),
    }
    for key, value in list(result.items()):
        if value == -math.inf:
            result[key] = None
    result["channel_peaks_dbfs"] = [None if v == -math.inf else v for v in result["channel_peaks_dbfs"]]
    return result


def write_test_wav(path: Path, *, seconds: float = 1.0, sample_rate: int = 48000,
                   channels: int = 2, amplitude: float = 0.5, frequency: float = 440.0,
                   bits: int = 24) -> None:
    """Write a sine-wave WAV file (used by the test suite and its fake csound)."""
    frames = int(seconds * sample_rate)
    scale = amplitude * (2 ** (bits - 1) - 1)
    width = bits // 8
    data = bytearray()
    for n in range(frames):
        value = int(round(scale * math.sin(2 * math.pi * frequency * n / sample_rate)))
        sample = value.to_bytes(width, "little", signed=True)
        data += sample * channels
    header = b"RIFF" + struct.pack("<I", 36 + len(data)) + b"WAVE" + b"fmt " + struct.pack(
        "<IHHIIHH", 16, _PCM, channels, sample_rate, sample_rate * channels * width, channels * width, bits
    ) + b"data" + struct.pack("<I", len(data))
    Path(path).parent.mkdir(parents=True, exist_ok=True)
    with open(path, "wb") as handle:
        handle.write(header + bytes(data))


def summary(analysis: Dict[str, Any]) -> List[str]:
    return [
        f"{analysis['duration_seconds']} s, {analysis['channels']} ch, "
        f"{analysis['sample_rate']} Hz, {analysis['bits']}-bit {analysis['sample_format']}",
        f"peak {analysis['peak_dbfs']} dBFS, RMS {analysis['rms_dbfs']} dBFS, "
        f"{analysis['clipped_samples']} clipped samples",
    ]
