#!/bin/bash

# Pull down the new development environment repo
git clone https://github.com/exercism/development-environment.git
# Pull down the latest version of the Common Lisp Test Runner
git clone https://github.com/exercism/common-lisp-test-runner.git

# Configure from within the development-environment/ directory
cd development-environment/

# Write a lispy stack.yml
cat << EOF > stack.yml
enabled:
  - core_services
  - website
  - tooling
  - admin
  - common-lisp-test-runner

configure:
  website:
    environment:
      WEBPACKER_DEV_SERVER_PUBLIC: "localhost:9999"
      CABLE_URL: "ws://localhost:9998/cable"
    ports:
      - 3020:3020 # rails, 3020 internal
      - 9999:3035 # webpack, 3035 internal
      - 9998:3334 # cable, 3334 internal

  common-lisp-test-runner:
    source: true
    build: true

groups:
  core_services:
    - setup
    - redis
    - dynamodb
    - s3
  admin:
    - adminer
    - portainer
  tooling:
    - tooling-invoker
    - tooling-orchestrator
  analyzers:
    - none
  test-runners:
    - csharp-test-runner
    - common-lisp-test-runner
EOF

./bin/start
