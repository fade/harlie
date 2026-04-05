#!/usr/bin/env bash
# Launch Consort IRC bot under SBCL with Quicklisp.
exec /usr/local/bin/sbcl \
  --dynamic-space-size 2048 \
  --noinform \
  --non-interactive \
  --load "$HOME/quicklisp/setup.lisp" \
  --load "/home/glenn/SourceCode/harlie/bin/run-consort.lisp"
