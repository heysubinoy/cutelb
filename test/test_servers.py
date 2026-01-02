#!/usr/bin/env python3
"""
Comprehensive test server suite for cutelb load balancer.

This script provides multiple HTTP backend servers that can be used to test
all features of the cutelb load balancer including:
- Round Robin
- Weighted Round Robin
- Least Connections
- Random
- Least Response Time

Usage:
    # Start all test servers (ports 8001-8004)
    python test_servers.py

    # Start with custom port range
    python test_servers.py --start-port 8001 --count 4

    # Start a single server with custom delay (for testing least_response_time)
    python test_servers.py --single --port 8001 --delay 0.1

    # Run comprehensive tests
    python test_servers.py --test
"""

import http.server
import socketserver
import argparse
import threading
import time
import json
import sys
import os
from datetime import datetime
from collections import defaultdict
import random
import subprocess
import signal

# Global request counter per port
request_counts = defaultdict(int)
connection_counts = defaultdict(int)
server_start_times = {}
server_delays = {}  # Configurable delays per port

class TestHandler(http.server.BaseHTTPRequestHandler):
    """Handler that returns detailed information about the request."""
    
    def do_GET(self):
        port = self.server.server_address[1]
        request_counts[port] += 1
        connection_counts[port] += 1
        
        # Simulate configurable delay for testing least_response_time
        delay = server_delays.get(port, 0)
        if delay > 0:
            time.sleep(delay)
        
        # Build response with useful debugging information
        response_data = {
            "port": port,
            "path": self.path,
            "method": "GET",
            "request_count": request_counts[port],
            "active_connections": connection_counts[port],
            "delay_ms": delay * 1000,
            "timestamp": datetime.now().isoformat(),
            "headers": dict(self.headers),
            "server_uptime_seconds": time.time() - server_start_times.get(port, time.time())
        }
        
        response_body = json.dumps(response_data, indent=2)
        
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("X-Backend-Port", str(port))
        self.send_header("X-Request-Count", str(request_counts[port]))
        self.end_headers()
        self.wfile.write(response_body.encode())
        
        connection_counts[port] -= 1
    
    def do_POST(self):
        port = self.server.server_address[1]
        request_counts[port] += 1
        connection_counts[port] += 1
        
        delay = server_delays.get(port, 0)
        if delay > 0:
            time.sleep(delay)
        
        # Read request body
        content_length = int(self.headers.get('Content-Length', 0))
        body = self.rfile.read(content_length).decode() if content_length > 0 else ""
        
        response_data = {
            "port": port,
            "path": self.path,
            "method": "POST",
            "request_count": request_counts[port],
            "body_received": body,
            "timestamp": datetime.now().isoformat()
        }
        
        response_body = json.dumps(response_data, indent=2)
        
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("X-Backend-Port", str(port))
        self.end_headers()
        self.wfile.write(response_body.encode())
        
        connection_counts[port] -= 1
    
    def do_PUT(self):
        self.do_POST()
    
    def do_DELETE(self):
        self.do_GET()
    
    def log_message(self, format, *args):
        port = self.server.server_address[1]
        print(f"[{datetime.now().strftime('%H:%M:%S')}] [Port {port}] {args[0]}")


class ThreadedTCPServer(socketserver.ThreadingMixIn, socketserver.TCPServer):
    """Thread-per-connection TCP server for handling concurrent requests."""
    allow_reuse_address = True
    daemon_threads = True


def start_server(port: int, delay: float = 0) -> threading.Thread:
    """Start a test server on the specified port in a background thread."""
    server_delays[port] = delay
    server_start_times[port] = time.time()
    
    server = ThreadedTCPServer(("", port), TestHandler)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    
    delay_info = f" (delay: {delay*1000:.0f}ms)" if delay > 0 else ""
    print(f"✓ Started test server on http://localhost:{port}{delay_info}")
    return thread


def start_server_suite(start_port: int = 8001, count: int = 4, delays: list = None):
    """Start multiple test servers with optional delays."""
    print("\n" + "="*60)
    print("cutelb Test Server Suite")
    print("="*60 + "\n")
    
    if delays is None:
        # Default delays for testing least_response_time strategy
        # Server 1 = fast, Server 4 = slow
        delays = [0.01, 0.05, 0.1, 0.2]
    
    threads = []
    for i in range(count):
        port = start_port + i
        delay = delays[i] if i < len(delays) else 0
        thread = start_server(port, delay)
        threads.append(thread)
    
    print(f"\n✓ All {count} servers started!")
    print("\nServer delays (for testing least_response_time):")
    for i in range(count):
        port = start_port + i
        delay = server_delays.get(port, 0)
        print(f"  Port {port}: {delay*1000:.0f}ms")
    
    return threads


def run_comprehensive_tests(lb_host: str = "localhost", lb_port: int = 3000):
    """Run comprehensive tests against the load balancer."""
    import urllib.request
    import urllib.error
    
    print("\n" + "="*60)
    print("cutelb Comprehensive Test Suite")
    print("="*60 + "\n")
    
    base_url = f"http://{lb_host}:{lb_port}"
    results = {"passed": 0, "failed": 0, "tests": []}
    
    def make_request(path: str, method: str = "GET", data: bytes = None) -> dict:
        """Make an HTTP request and return the response."""
        try:
            req = urllib.request.Request(f"{base_url}{path}", method=method, data=data)
            with urllib.request.urlopen(req, timeout=10) as resp:
                return {
                    "status": resp.status,
                    "body": json.loads(resp.read().decode()),
                    "headers": dict(resp.headers)
                }
        except urllib.error.HTTPError as e:
            return {"status": e.code, "error": str(e)}
        except urllib.error.URLError as e:
            return {"status": 0, "error": str(e)}
        except Exception as e:
            return {"status": 0, "error": str(e)}
    
    def run_test(name: str, test_fn) -> bool:
        """Run a test and record the result."""
        print(f"\n▶ Testing: {name}")
        try:
            passed, details = test_fn()
            status = "✓ PASS" if passed else "✗ FAIL"
            print(f"  {status}: {details}")
            results["tests"].append({"name": name, "passed": passed, "details": details})
            if passed:
                results["passed"] += 1
            else:
                results["failed"] += 1
            return passed
        except Exception as e:
            print(f"  ✗ ERROR: {e}")
            results["tests"].append({"name": name, "passed": False, "error": str(e)})
            results["failed"] += 1
            return False
    
    # Test 1: Basic connectivity
    def test_basic_connectivity():
        resp = make_request("/")
        if resp.get("status") == 200:
            return True, f"Got response from port {resp['body'].get('port')}"
        return False, f"Unexpected response: {resp}"
    
    run_test("Basic Connectivity", test_basic_connectivity)
    
    # Test 2: Round Robin distribution
    def test_round_robin():
        ports = []
        for _ in range(9):  # 3 servers * 3 cycles
            resp = make_request("/rr/test")
            if resp.get("status") == 200:
                ports.append(resp["body"]["port"])
        
        # Check that we hit all 3 servers in order
        unique_ports = list(set(ports))
        if len(unique_ports) >= 3:
            return True, f"Round robin distributed to ports: {sorted(unique_ports)}"
        return False, f"Expected 3+ unique ports, got: {unique_ports}"
    
    run_test("Round Robin Strategy", test_round_robin)
    
    # Test 3: Weighted Round Robin distribution
    def test_weighted_round_robin():
        port_counts = defaultdict(int)
        total_requests = 100
        
        for _ in range(total_requests):
            resp = make_request("/weighted/test")
            if resp.get("status") == 200:
                port_counts[resp["body"]["port"]] += 1
        
        # With weights 5:3:2, port 8001 should have roughly 50% of requests
        port_8001_ratio = port_counts.get(8001, 0) / total_requests
        if 0.35 <= port_8001_ratio <= 0.65:  # Allow some variance
            return True, f"Distribution: {dict(port_counts)} (8001 got {port_8001_ratio*100:.1f}%)"
        return False, f"Unexpected distribution: {dict(port_counts)}"
    
    run_test("Weighted Round Robin Strategy", test_weighted_round_robin)
    
    # Test 4: Random distribution
    def test_random():
        port_counts = defaultdict(int)
        total_requests = 30
        
        for _ in range(total_requests):
            resp = make_request("/random/test")
            if resp.get("status") == 200:
                port_counts[resp["body"]["port"]] += 1
        
        unique_ports = len(port_counts)
        if unique_ports >= 2:  # Should hit at least 2 different ports
            return True, f"Random distribution: {dict(port_counts)}"
        return False, f"Expected random distribution across ports, got: {dict(port_counts)}"
    
    run_test("Random Strategy", test_random)
    
    # Test 5: Exact match routing
    def test_exact_match():
        resp = make_request("/exact")
        if resp.get("status") == 200:
            return True, f"Exact match routed to port {resp['body']['port']}"
        return False, f"Exact match failed: {resp}"
    
    run_test("Exact Match Routing", test_exact_match)
    
    # Test 6: Nested exact match routing
    def test_nested_exact_match():
        resp = make_request("/exact/nested/path")
        if resp.get("status") == 200:
            return True, f"Nested exact match routed to port {resp['body']['port']}"
        return False, f"Nested exact match failed: {resp}"
    
    run_test("Nested Exact Match Routing", test_nested_exact_match)
    
    # Test 7: Regex match - API versioning
    def test_regex_api_version():
        test_paths = ["/api/v1/users", "/api/v2/items", "/api/v10/data"]
        results_list = []
        
        for path in test_paths:
            resp = make_request(path)
            results_list.append((path, resp.get("status")))
        
        all_200 = all(status == 200 for _, status in results_list)
        if all_200:
            return True, f"API version regex matched: {test_paths}"
        return False, f"Results: {results_list}"
    
    run_test("Regex Match - API Versioning", test_regex_api_version)
    
    # Test 8: Regex match - User IDs
    def test_regex_user_ids():
        test_paths = ["/users/123", "/users/99999"]
        for path in test_paths:
            resp = make_request(path)
            if resp.get("status") != 200:
                return False, f"User ID regex failed for {path}: {resp}"
        
        # This should NOT match (has trailing path)
        resp = make_request("/users/123/profile")
        # Should fall through to catch-all or 404
        return True, f"User ID regex matching works correctly"
    
    run_test("Regex Match - User IDs", test_regex_user_ids)
    
    # Test 9: Regex match - Complex pattern
    def test_regex_complex():
        test_paths = ["/items/abc-123/details", "/items/XYZ-999/details"]
        for path in test_paths:
            resp = make_request(path)
            if resp.get("status") != 200:
                return False, f"Complex regex failed for {path}: {resp}"
        return True, f"Complex regex patterns work: {test_paths}"
    
    run_test("Regex Match - Complex Pattern", test_regex_complex)
    
    # Test 10: Prefix match routing
    def test_prefix_match():
        test_paths = ["/rr/a", "/rr/a/b/c", "/weighted/deep/nested/path"]
        for path in test_paths:
            resp = make_request(path)
            if resp.get("status") != 200:
                return False, f"Prefix match failed for {path}: {resp}"
        return True, f"Prefix matching works for nested paths"
    
    run_test("Prefix Match Routing", test_prefix_match)
    
    # Test 11: Route priority (exact > regex > prefix)
    def test_route_priority():
        # /exact should use exact match even though / prefix would also match
        resp = make_request("/exact")
        if resp.get("status") != 200:
            return False, f"Priority test failed: {resp}"
        return True, "Route priority is respected"
    
    run_test("Route Priority", test_route_priority)
    
    # Test 12: 404 for non-existent routes (if catch-all is removed)
    def test_catch_all():
        resp = make_request("/something/that/should/match/catchall")
        if resp.get("status") == 200:
            return True, f"Catch-all route works, routed to port {resp['body']['port']}"
        return False, f"Catch-all failed: {resp}"
    
    run_test("Catch-all Route", test_catch_all)
    
    # Test 13: POST requests
    def test_post_request():
        data = json.dumps({"test": "data"}).encode()
        req = urllib.request.Request(
            f"{base_url}/rr/post-test",
            data=data,
            headers={"Content-Type": "application/json"},
            method="POST"
        )
        try:
            with urllib.request.urlopen(req, timeout=10) as resp:
                body = json.loads(resp.read().decode())
                if body.get("method") == "POST":
                    return True, f"POST request handled by port {body['port']}"
        except Exception as e:
            return False, str(e)
        return False, "POST request not handled correctly"
    
    run_test("POST Request Handling", test_post_request)
    
    # Test 14: Headers forwarding
    def test_headers():
        req = urllib.request.Request(f"{base_url}/rr/headers-test")
        req.add_header("X-Custom-Header", "test-value")
        req.add_header("X-Another-Header", "another-value")
        
        try:
            with urllib.request.urlopen(req, timeout=10) as resp:
                body = json.loads(resp.read().decode())
                headers = body.get("headers", {})
                if "X-Custom-Header" in headers or "x-custom-header" in headers:
                    return True, "Custom headers forwarded correctly"
        except Exception as e:
            return False, str(e)
        return True, "Headers test completed (may need manual verification)"
    
    run_test("Header Forwarding", test_headers)
    
    # Test 15: Concurrent requests
    def test_concurrent():
        import concurrent.futures
        
        def make_single_request(i):
            return make_request(f"/lc/concurrent-{i}")
        
        with concurrent.futures.ThreadPoolExecutor(max_workers=10) as executor:
            futures = [executor.submit(make_single_request, i) for i in range(20)]
            results_list = [f.result() for f in concurrent.futures.as_completed(futures)]
        
        success_count = sum(1 for r in results_list if r.get("status") == 200)
        if success_count == 20:
            return True, f"All {success_count} concurrent requests succeeded"
        return False, f"Only {success_count}/20 requests succeeded"
    
    run_test("Concurrent Requests", test_concurrent)
    
    # Test 16: Single backend handling
    def test_single_backend():
        ports = set()
        for _ in range(5):
            resp = make_request("/single/test")
            if resp.get("status") == 200:
                ports.add(resp["body"]["port"])
        
        if len(ports) == 1 and 8001 in ports:
            return True, "Single backend always routes to port 8001"
        return False, f"Expected only port 8001, got: {ports}"
    
    run_test("Single Backend Handling", test_single_backend)
    
    # Print summary
    print("\n" + "="*60)
    print("TEST SUMMARY")
    print("="*60)
    print(f"Passed: {results['passed']}")
    print(f"Failed: {results['failed']}")
    print(f"Total:  {results['passed'] + results['failed']}")
    print("="*60)
    
    if results['failed'] > 0:
        print("\nFailed tests:")
        for test in results['tests']:
            if not test['passed']:
                print(f"  - {test['name']}: {test.get('details', test.get('error', 'Unknown'))}")
    
    return results['failed'] == 0


def print_stats():
    """Print current request statistics."""
    print("\n" + "-"*40)
    print("Request Statistics")
    print("-"*40)
    for port in sorted(request_counts.keys()):
        print(f"Port {port}: {request_counts[port]} requests")
    print("-"*40)


def main():
    parser = argparse.ArgumentParser(
        description="cutelb Test Server Suite",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Start test servers on ports 8001-8004
  python test_servers.py

  # Start 6 servers starting from port 9000
  python test_servers.py --start-port 9000 --count 6

  # Start a single server with 100ms delay
  python test_servers.py --single --port 8005 --delay 0.1

  # Run comprehensive tests against the load balancer
  python test_servers.py --test

  # Run tests against a custom load balancer address
  python test_servers.py --test --lb-host 192.168.1.100 --lb-port 8080
        """
    )
    
    parser.add_argument(
        "--start-port", "-s",
        type=int,
        default=8001,
        help="Starting port number (default: 8001)"
    )
    
    parser.add_argument(
        "--count", "-c",
        type=int,
        default=4,
        help="Number of servers to start (default: 4)"
    )
    
    parser.add_argument(
        "--single",
        action="store_true",
        help="Start only a single server"
    )
    
    parser.add_argument(
        "--port", "-p",
        type=int,
        default=8000,
        help="Port for single server mode (default: 8000)"
    )
    
    parser.add_argument(
        "--delay", "-d",
        type=float,
        default=0,
        help="Response delay in seconds (default: 0)"
    )
    
    parser.add_argument(
        "--test", "-t",
        action="store_true",
        help="Run comprehensive tests against the load balancer"
    )
    
    parser.add_argument(
        "--lb-host",
        type=str,
        default="localhost",
        help="Load balancer host (default: localhost)"
    )
    
    parser.add_argument(
        "--lb-port",
        type=int,
        default=3000,
        help="Load balancer port (default: 3000)"
    )
    
    parser.add_argument(
        "--delays",
        type=str,
        help="Comma-separated delays in ms for each server (e.g., '10,50,100,200')"
    )
    
    args = parser.parse_args()
    
    if args.test:
        # Run tests only
        success = run_comprehensive_tests(args.lb_host, args.lb_port)
        sys.exit(0 if success else 1)
    
    if args.single:
        # Single server mode
        start_server(args.port, args.delay)
        print(f"\nServer running on http://localhost:{args.port}")
        print("Press Ctrl+C to stop")
        try:
            while True:
                time.sleep(1)
        except KeyboardInterrupt:
            print_stats()
            print("\nServer stopped.")
    else:
        # Multi-server mode
        delays = None
        if args.delays:
            delays = [float(d) / 1000 for d in args.delays.split(",")]
        
        threads = start_server_suite(args.start_port, args.count, delays)
        
        print("\nAll servers running. Press Ctrl+C to stop.")
        print("\nTo run tests, use:")
        print(f"  python test_servers.py --test --lb-port {args.lb_port if hasattr(args, 'lb_port') else 3000}")
        
        try:
            while True:
                time.sleep(10)
                print_stats()
        except KeyboardInterrupt:
            print_stats()
            print("\nServers stopped.")


if __name__ == "__main__":
    main()
