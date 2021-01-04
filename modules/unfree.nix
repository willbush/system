{ config, lib, ... }:
let
  inherit (lib) types mkOption getName;
  cfg = config.modules.unfree;
in {
  options.modules.unfree = {
    allowList = mkOption {
      default = [ ];
      type = types.listOf types.str;
      description = ''
        A list of unfree packages to allow. This list as an options allows
        adding to the list in different modules and having the module system
        append the lists together.
      '';
    };
  };

  config = {
    nixpkgs.config.allowUnfreePredicate = pkg:
      builtins.elem (getName pkg) cfg.allowList;
  };
}
