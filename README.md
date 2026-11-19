# cutelb 🐰

A cute, lightweight HTTP load balancer written in Haskell.

## Features

- **Multiple Load Balancing Strategies**
  - Round Robin
  - Weighted Round Robin
  - Least Connections
  - Random
  - Least Response Time

- **Flexible Routing**
  - Exact path matching
  - Prefix matching
  - Regex matching

- **Active Health Checks**
  - Periodic HTTP probes per backend
  - Unhealthy backends are taken out of rotation for every strategy and rejoin automatically on recovery

- **Configuration**
  - YAML-based configuration
  - Multiple upstream definitions
  - Optional access and error logging

## Installation

Requires GHC and Cabal. Using Nix is recommended:

```bash
# With Nix
nix-shell

# Build
cabal build

# Run
cabal run cutelb -- config.yaml
```

## Configuration

```yaml
server:
  listen: 3000
  access_log: ./access.log  # optional
  error_log: ./error.log    # optional

upstreams:
  my_backend:
    strategy: round_robin  # or: weighted_round_robin, least_conn, random, least_response_time
    health_check: /health  # optional: path to probe; omit to disable
    health_interval: 5     # optional: seconds between probes (default 5)
    servers:
      - host: localhost
        port: 8001
      - host: localhost
        port: 8002
        weight: 2  # optional, for weighted_round_robin

routes:
  - path: /api
    upstream: my_backend
    match_type: prefix  # or: exact, regex
```

## Health checks

When an upstream sets `health_check`, a background thread sends `GET <path>` to each of its backends every `health_interval` seconds, with a 2-second timeout.

- A backend that answers `2xx`/`3xx` is **healthy**.
- An error status, a refused connection or a timeout marks it **unhealthy**, and every strategy (round robin, weighted, least connections, random, least response time) skips it.
- As soon as a probe succeeds again, the backend rejoins the pool.
- If every backend in an upstream is unhealthy, requests get `503 Service Unavailable` instead of being sent to a dead server.

State changes are logged, so you can see failovers happen:

```text
[WARN] Backend marked unhealthy upstream=my_backend backend=localhost:8002 reason=...ConnectionFailure...
[INFO] Backend recovered upstream=my_backend backend=localhost:8002
```

## How it works

- **Concurrency:** Warp serves each request on its own lightweight thread. Shared per-backend state (active connections, response-time EMA, health) lives in STM `TVar`s, so strategies read and update it atomically without locks.
- **Least response time** tracks an exponential moving average (α = 0.2) of each backend's latency, and sends traffic to backends with no measurements first so they get warmed up.
- **Connection reuse:** one shared `http-client` connection pool serves all proxied requests and health probes.
- **Faithful proxying:** query strings are preserved, hop-by-hop headers are stripped, and response bodies are passed through byte-for-byte.

## Usage

```bash
# Run with default config.yaml
cabal run cutelb

# Run with custom config
cabal run cutelb -- path/to/config.yaml
```

## Testing

Start test backend servers:

```bash
python3 test/test_servers.py --test
```

Then run the load balancer:

```bash
cabal run cutelb -- test/config.yaml
```

Test with curl:

```bash
curl localhost:3000/api
```

To see health checks in action, stop one of the test backends. The load balancer logs it as unhealthy within a few seconds and stops routing to it. Start it again and it rejoins.

## License

MIT
