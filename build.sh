#!/usr/bin/env bash

rustc \
--crate-name mnya \
--edition=2024 \
--crate-type bin \
-C opt-level=3 \
-C embed-bitcode=no \
-C strip=debuginfo \
main.rs 
