"""Local static web server for trying the viewer.

Serves the repository's docs/ directory as GitHub Pages would lay it out,
so the viewer is at /studio/ and the published files at /compositions/.
It supports HTTP Range requests, which browsers need for seeking in audio.
It serves files exactly as they are: Jekyll processing is not emulated.
"""
from __future__ import annotations

import os
import re
from http import HTTPStatus
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from typing import Optional, Tuple

_RANGE = re.compile(r"^bytes=(\d*)-(\d*)$")
_TYPES = {
    ".js": "text/javascript",
    ".mjs": "text/javascript",
    ".css": "text/css",
    ".json": "application/json",
    ".csd": "text/plain; charset=utf-8",
    ".md": "text/plain; charset=utf-8",
    ".py": "text/plain; charset=utf-8",
    ".cpp": "text/plain; charset=utf-8",
    ".log": "text/plain; charset=utf-8",
    ".mp3": "audio/mpeg",
    ".flac": "audio/flac",
    ".wav": "audio/wav",
    ".opus": "audio/ogg",
    ".ogg": "audio/ogg",
    ".mid": "audio/midi",
    ".musicxml": "application/vnd.recordare.musicxml+xml",
}


class StaticHandler(SimpleHTTPRequestHandler):
    """Static files with byte-range support and caching disabled."""

    protocol_version = "HTTP/1.1"
    quiet = False

    def guess_type(self, path):
        suffix = Path(str(path)).suffix.lower()
        return _TYPES.get(suffix) or super().guess_type(path)

    def log_message(self, format, *args):  # noqa: A002 - signature of the base class
        if not self.quiet:
            super().log_message(format, *args)

    def end_headers(self):
        self.send_header("Cache-Control", "no-store")
        self.send_header("Accept-Ranges", "bytes")
        super().end_headers()

    def _parse_range(self, size: int) -> Optional[Tuple[int, int]]:
        match = _RANGE.match(self.headers.get("Range", "").strip())
        if not match:
            return None
        first, last = match.groups()
        if first == "" and last == "":
            return None
        if first == "":
            length = int(last)
            if length == 0:
                return (size, size)  # unsatisfiable
            return (max(0, size - length), size - 1)
        start = int(first)
        end = min(int(last), size - 1) if last else size - 1
        return (start, end)

    def send_head(self):
        if "Range" not in self.headers:
            return super().send_head()
        path = self.translate_path(self.path)
        if os.path.isdir(path):
            return super().send_head()
        try:
            handle = open(path, "rb")
        except OSError:
            self.send_error(HTTPStatus.NOT_FOUND, "File not found")
            return None
        size = os.fstat(handle.fileno()).st_size
        wanted = self._parse_range(size)
        if wanted is None:
            handle.close()
            return super().send_head()
        start, end = wanted
        if start >= size or end < start:
            handle.close()
            self.send_response(HTTPStatus.REQUESTED_RANGE_NOT_SATISFIABLE)
            self.send_header("Content-Range", f"bytes */{size}")
            self.send_header("Content-Length", "0")
            self.end_headers()
            return None
        self._remaining = end - start + 1
        handle.seek(start)
        self.send_response(HTTPStatus.PARTIAL_CONTENT)
        self.send_header("Content-Type", self.guess_type(path))
        self.send_header("Content-Range", f"bytes {start}-{end}/{size}")
        self.send_header("Content-Length", str(self._remaining))
        self.end_headers()
        return handle

    def copyfile(self, source, outputfile):
        remaining = getattr(self, "_remaining", None)
        if remaining is None:
            return super().copyfile(source, outputfile)
        self._remaining = None
        while remaining > 0:
            block = source.read(min(65536, remaining))
            if not block:
                break
            outputfile.write(block)
            remaining -= len(block)


def make_server(directory: Path, host: str = "127.0.0.1", port: int = 8000, quiet: bool = False):
    directory = str(Path(directory).resolve())

    class Handler(StaticHandler):
        def __init__(self, *args, **kwargs):
            super().__init__(*args, directory=directory, **kwargs)

    Handler.quiet = quiet
    server = ThreadingHTTPServer((host, port), Handler)
    server.daemon_threads = True
    return server


def serve(directory: Path, host: str = "127.0.0.1", port: int = 8000) -> None:
    server = make_server(directory, host, port)
    shown = "localhost" if host in ("127.0.0.1", "") else host
    print(f"Serving {directory} at http://{shown}:{server.server_address[1]}/")
    print(f"Viewer: http://{shown}:{server.server_address[1]}/studio/")
    print("Press Ctrl-C to stop.")
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print()
    finally:
        server.server_close()
