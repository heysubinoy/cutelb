# cutelb Test Suite

This directory contains comprehensive test configurations and tools for testing all features of the cutelb load balancer.

## Quick Start

1. **Start the test backend servers:**
   ```bash
   cd test
   python test_servers.py
   ```

2. **In another terminal, start cutelb with the test config:**
   ```bash
   cabal run cutelb -- test/config.yaml
   ```

3. **Run the comprehensive test suite:**
   ```bash
   python test_servers.py --test
   ```

## Test Configuration

The `config.yaml` file includes test configurations for all features:

### Load Balancing Strategies

| Strategy | Route Prefix | Description |
|----------|-------------|-------------|
| Round Robin | `/rr/` | Cycles through backends sequentially |
| Weighted Round Robin | `/weighted/` | Distributes based on weights (5:3:2) |
| Least Connections | `/lc/` | Routes to backend with fewest active connections |
| Random | `/random/` | Random backend selection |
| Least Response Time | `/lrt/` | Routes to fastest responding backend |

### Route Matching Types

| Type | Example | Description |
|------|---------|-------------|
| Exact | `/exact` | Must match the path exactly |
| Prefix | `/rr/` | Matches any path starting with prefix |
| Regex | `^/api/v[0-9]+/.*` | Perl-compatible regex matching |

## Test Servers

The `test_servers.py` script provides flexible test backend servers:

### Basic Usage

```bash
# Start 4 servers on ports 8001-8004 with varying response delays
python test_servers.py

# Start servers on custom ports
python test_servers.py --start-port 9000 --count 6

# Start a single server with specific delay
python test_servers.py --single --port 8005 --delay 0.1

# Custom delays for each server (in milliseconds)
python test_servers.py --delays 10,50,100,200
```

### Running Tests

```bash
# Run full test suite against localhost:3000
python test_servers.py --test

# Test against custom load balancer address
python test_servers.py --test --lb-host 192.168.1.100 --lb-port 8080
```

## Test Coverage

The test suite covers:

1. **Basic Connectivity** - Can connect to load balancer
2. **Round Robin** - Requests distributed evenly
3. **Weighted Round Robin** - Distribution matches weights
4. **Random** - Requests go to different backends
5. **Exact Match Routing** - `/exact` routes correctly
6. **Nested Exact Match** - `/exact/nested/path` works
7. **Regex API Versioning** - `/api/v1/*`, `/api/v2/*` patterns
8. **Regex User IDs** - `/users/[0-9]+` patterns
9. **Complex Regex** - `/items/[a-zA-Z0-9-]+/details`
10. **Prefix Matching** - Nested paths under prefix
11. **Route Priority** - Exact > Regex > Prefix
12. **Catch-all Route** - Default routing works
13. **POST Requests** - Non-GET methods handled
14. **Header Forwarding** - Custom headers passed through
15. **Concurrent Requests** - Multiple simultaneous requests
16. **Single Backend** - Edge case with one backend

## Manual Testing

### Test Round Robin
```bash
for i in {1..9}; do curl -s localhost:3000/rr/ | jq .port; done
```

### Test Weighted Distribution
```bash
for i in {1..100}; do curl -s localhost:3000/weighted/ | jq .port; done | sort | uniq -c
```

### Test Regex Routing
```bash
curl localhost:3000/api/v1/users
curl localhost:3000/api/v2/items
curl localhost:3000/users/12345
```

### Test Exact vs Prefix
```bash
curl localhost:3000/exact           # Exact match
curl localhost:3000/exact/other     # Should use catch-all (/ prefix)
```

### Test Concurrent Load
```bash
# Install Apache Bench if needed: brew install httpd
ab -n 1000 -c 50 http://localhost:3000/lc/
```

## Response Format

Backend servers return JSON with debugging information:

```json
{
  "port": 8001,
  "path": "/rr/test",
  "method": "GET",
  "request_count": 42,
  "active_connections": 1,
  "delay_ms": 10,
  "timestamp": "2026-01-02T10:30:00.000000",
  "headers": {
    "Host": "localhost:8001",
    "X-Forwarded-For": "127.0.0.1"
  },
  "server_uptime_seconds": 123.45
}
```

## Troubleshooting

### Port already in use
```bash
# Find and kill process on port 8001
lsof -i :8001
kill -9 <PID>
```

### Connection refused
Make sure:
1. Test servers are running (`python test_servers.py`)
2. cutelb is running (`cabal run cutelb -- test/config.yaml`)
3. Ports 8001-8004 and 3000 are available

### Tests failing
Check the cutelb logs for error messages. Common issues:
- Backend servers not started
- Wrong config file path
- Port conflicts
