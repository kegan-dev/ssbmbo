FROM ubuntu:18.04

# Get stack and useful dependencies
RUN apt-get update && apt-get install -y curl glpk-utils libglpk-dev glpk-doc
RUN curl -sSL https://get.haskellstack.org/ | sh

# Add symlink for libtinfo required by dependecies
RUN ln -s /lib/x86_64-linux-gnu/libtinfo.so.5 /lib/x86_64-linux-gnu/libtinfo.so
