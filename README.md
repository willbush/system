# ❄ NixOS system and Emacs config

In NixOS, I run an [impermanent](https://github.com/nix-community/impermanence) setup with tmpfs as root, flakes, LUKS
encryption. Check out [this blog](https://willbush.dev/blog/impermanent-nixos/) post I made if you're interested.


# Folder Structure

```
.
├── configs        # see below.
├── hosts          # host specific configuration files.
├── modules        # NixOS modules
├── profiles
│   ├── home       # home-manager profiles
│   └── nixos      # nixos profiles
├── secrets        # nix-sops encrypted secrets.
└── users          # user specific
```

## configs

A sort of catch all for config / non-nix code.

Some are simply configuration files that are symlinked to ~/.config/~ and managed
with [home manager](https://github.com/rycee/home-manager).

*** keyboard firmware

Keyboard firmware code managed by Nix and the [readme](file:configs/keyboard-firmware/readme.org) that goes into depth on my
key layout and customized Evil / Vim keybindings.

### emacs

My emacs configuration code. See its [readme](file:configs/emacs/readme.org) for more information.


# License

Dual licensed under either:

- Apache License, Version 2.0 ([LICENSE-APACHE](http://www.apache.org/licenses/LICENSE-2.0))

- MIT license ([LICENSE-MIT](http://opensource.org/licenses/MIT))

at your option.
