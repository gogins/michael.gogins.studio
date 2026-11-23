"""Local static web server for trying the player, with an optional compose API.

Serves the repository's docs/ directory as GitHub Pages would lay it out,
so the player is at /studio/ and the published files at /compositions/.
The player performs the published .csd files itself, with Csound for
WebAssembly, so what is served is text and the player's own files; there
are no soundfiles. HTTP Range requests are supported (for large files such
as PDF scores). Files are served exactly as they are: Jekyll processing is
not emulated.

When a Studio instance is passed, ``/api/v1`` can stage, render, accept and
publish. The Player does not create pieces through it. On this Mac, Open MIDI
asks ``/api/v1/reveal`` to show the file selected in its folder.
"""
from __future__ import annotations

import json
import os
import re
import socket
import subprocess
import sys
import threading
from http import HTTPStatus
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from typing import Any, Optional, Tuple
from urllib.parse import urlparse

from . import api as compose_api
from .paths import Studio, StudioError, safe_join

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
    ".mid": "audio/midi",
    ".musicxml": "application/vnd.recordare.musicxml+xml",
}


class StaticHandler(SimpleHTTPRequestHandler):
    """Static files with byte-range support and caching disabled."""

    protocol_version = "HTTP/1.1"
    quiet = False
    studio: Optional[Studio] = None

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

    # ---------------------------------------------------------------- API

    def _api_path(self) -> Optional[str]:
        path = urlparse(self.path).path
        if path == "/api/v1" or path.startswith("/api/v1/"):
            return path[len("/api/v1") :].lstrip("/") or ""
        return None

    def _cors_headers(self) -> None:
        # Same-origin for local serve. Echo Origin so a test client on another
        # port can call the API. The Player does not.
        origin = self.headers.get("Origin")
        if origin:
            self.send_header("Access-Control-Allow-Origin", origin)
            self.send_header("Vary", "Origin")
            self.send_header("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
            self.send_header("Access-Control-Allow-Headers", "Content-Type")

    def _send_json(self, status: int, payload: Any) -> None:
        body = json.dumps(payload, indent=2, sort_keys=True).encode("utf-8") + b"\n"
        self.send_response(status)
        self._cors_headers()
        self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        if self.command != "HEAD":
            self.wfile.write(body)

    def _read_json(self, limit: int = 200_000) -> Any:
        length = int(self.headers.get("Content-Length") or "0")
        if length <= 0:
            raise ComposeRequestError("JSON body required")
        if length > limit:
            raise ComposeRequestError(f"body too large ({length} bytes)")
        raw = self.rfile.read(length)
        try:
            return json.loads(raw.decode("utf-8"))
        except (UnicodeDecodeError, json.JSONDecodeError) as error:
            raise ComposeRequestError(f"invalid JSON: {error}") from error

    def do_OPTIONS(self):  # noqa: N802
        if self._api_path() is None or self.studio is None:
            self.send_error(HTTPStatus.NOT_FOUND)
            return
        self.send_response(HTTPStatus.NO_CONTENT)
        self._cors_headers()
        self.end_headers()

    def do_GET(self):  # noqa: N802
        route = self._api_path()
        if route is not None:
            return self._handle_api("GET", route)
        return super().do_GET()

    def do_HEAD(self):  # noqa: N802
        route = self._api_path()
        if route is not None:
            return self._handle_api("HEAD", route)
        return super().do_HEAD()

    def do_POST(self):  # noqa: N802
        route = self._api_path()
        if route is not None:
            return self._handle_api("POST", route)
        self.send_error(HTTPStatus.METHOD_NOT_ALLOWED)

    def _handle_api(self, method: str, route: str) -> None:
        if self.studio is None:
            self._send_json(
                HTTPStatus.SERVICE_UNAVAILABLE,
                {
                    "ok": False,
                    "error": "compose API is not enabled on this server "
                    "(start with studio serve from the repository)",
                },
            )
            return
        try:
            if route in ("", "status") and method in ("GET", "HEAD"):
                self._send_json(HTTPStatus.OK, compose_api.status())
                return
            if route == "compose" and method == "POST":
                body = self._read_json()
                if not isinstance(body, dict):
                    raise ComposeRequestError("JSON object required")
                result = compose_api.compose(
                    self.studio,
                    prompt=str(body.get("prompt") or ""),
                    composition=body.get("composition") or None,
                    name=body.get("name") or None,
                    title=body.get("title") or None,
                )
                self._send_json(HTTPStatus.OK, result)
                return
            if route == "reveal" and method == "POST":
                body = self._read_json()
                if not isinstance(body, dict):
                    raise ComposeRequestError("JSON object required")
                shown = reveal_published_midi(self.studio, str(body.get("path") or ""))
                self._send_json(HTTPStatus.OK, {"ok": True, "path": str(shown)})
                return
            if method == "POST":
                self._send_json(HTTPStatus.NOT_FOUND, {"ok": False, "error": f"unknown API route: {route}"})
                return
            self._send_json(HTTPStatus.NOT_FOUND, {"ok": False, "error": f"unknown API route: {route}"})
        except ComposeRequestError as error:
            self._send_json(HTTPStatus.BAD_REQUEST, {"ok": False, "error": str(error)})
        except StudioError as error:
            self._send_json(HTTPStatus.CONFLICT, {"ok": False, "error": str(error)})
        except Exception as error:  # noqa: BLE001 - surface unexpected failures to the player
            self._send_json(
                HTTPStatus.INTERNAL_SERVER_ERROR,
                {"ok": False, "error": f"compose failed: {error}"},
            )


class ComposeRequestError(ValueError):
    """Malformed compose HTTP request."""


def reveal_published_midi(studio: Studio, relative: str) -> Path:
    """Select a published MIDI file in Finder, in the folder that holds it.

    macOS then supplies the context menu for choosing an application.
    ``relative`` is a path under docs/compositions/, such as ``nu/outputs/nu.mid``.
    """
    text = str(relative or "").strip().replace("\\", "/")
    if not text or Path(text).suffix.lower() not in (".mid", ".midi"):
        raise StudioError("not a published MIDI file")
    full = safe_join(studio.published, text)
    if not full.is_file():
        raise StudioError("that MIDI file is not in the published folder")
    if sys.platform != "darwin":
        raise StudioError("showing the file in its folder is available on macOS")
    completed = subprocess.run(["open", "-R", os.fspath(full)], check=False)
    if completed.returncode != 0:
        raise StudioError("Finder did not show the file")
    return full


def make_server(
    directory: Path,
    host: str = "127.0.0.1",
    port: int = 8000,
    quiet: bool = False,
    studio: Optional[Studio] = None,
):
    directory = str(Path(directory).resolve())

    class Handler(StaticHandler):
        def __init__(self, *args, **kwargs):
            super().__init__(*args, directory=directory, **kwargs)

    Handler.quiet = quiet
    Handler.studio = studio

    class Server(ThreadingHTTPServer):
        address_family = socket.AF_INET6 if ":" in host else socket.AF_INET

    server = Server((host, port), Handler)
    server.daemon_threads = True
    return server


def serve(
    directory: Path,
    host: str = "127.0.0.1",
    port: int = 8000,
    studio: Optional[Studio] = None,
) -> None:
    primary = "127.0.0.1" if host in ("127.0.0.1", "localhost", "") else host
    server = make_server(directory, primary, port, studio=studio)
    bound = server.server_address[1]
    others = []
    # `localhost` is ::1 on this Mac. A server bound only to 127.0.0.1 never
    # sees that request, and a plain file server on ::1 answers POST with 501.
    if primary == "127.0.0.1":
        try:
            ipv6 = make_server(directory, "::1", bound, studio=studio)
            threading.Thread(target=ipv6.serve_forever, name="studio-serve-ipv6", daemon=True).start()
            others.append(ipv6)
        except OSError as error:
            print(f"http://localhost:{bound}/ is not this server ({error}).")
    shown = "localhost" if primary == "127.0.0.1" else primary
    print(f"Serving {directory} at http://{shown}:{bound}/")
    print(f"Player: http://{shown}:{bound}/studio/")
    if studio is not None:
        print(f"Compose API: http://{shown}:{bound}/api/v1/")
    print("Press Ctrl-C to stop.")
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print()
    finally:
        for extra in others:
            extra.shutdown()
            extra.server_close()
        server.server_close()
