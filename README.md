# cidrtool

A simple application and Elm library for inspecting and subtracting CIDR
blocks. Try it out: https://bluekeyes.github.io/cidrtool/

## Why?

Two reasons:

1. Subtracting CIDR blocks seemed like an interesting problem
2. I wanted to build a thing with Elm

If you find yourself also needing to subtract CIDR blocks, something's probably
gone wrong already. But one place where it might be useful is when you have an
ALLOW-only firewall (like an EC2 security group) but you really want to DENY a
specific IP range. Take the overall allowed range, subtract out the deny
range(s), and add ALLOW rules for the result.

This will quickly become difficult to maintain and will probably run into
limitations on the number of allowed rules per security group. But until then,
it will (probably) work!

## Development

Clone the repository, then run:

    npm install
    npm run start

You can view the application at `http://localhost:8080` and it will live-reload
as changes are made.

To build a production version for distribution, run:

    npm run build
