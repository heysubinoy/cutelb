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

## License

MIT
