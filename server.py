#!/usr/bin/env python3
"""Simple HTTP server that returns the port it's running on."""

import http.server
import socketserver
import argparse


class PortHandler(http.server.BaseHTTPRequestHandler):
    """Handler that returns the server port."""
    
    def do_GET(self):
        self.send_response(200)
        self.send_header("Content-Type", "text/plain")
        self.end_headers()
        self.wfile.write(f"Port: {self.server.server_address[1]}\n".encode())
    
    def log_message(self, format, *args):
        print(f"[{self.server.server_address[1]}] {args[0]}")


def start_server(port: int = 8000):
    """Start a simple HTTP server on the specified port."""
    with socketserver.TCPServer(("", port), PortHandler) as httpd:
        print(f"Serving at http://localhost:{port}")
        print("Press Ctrl+C to stop")
        try:
            httpd.serve_forever()
        except KeyboardInterrupt:
            print("\nServer stopped.")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Start a simple HTTP server")
    parser.add_argument(
        "-p", "--port",
        type=int,
        default=8000,
        help="Port to serve on (default: 8000)"
    )
    args = parser.parse_args()
    start_server(args.port)
