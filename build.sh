#!/bin/sh

echo "Removing _build/default..."
rm -r _build/default

echo "Building with profile 'default'..."
rebar3 release
