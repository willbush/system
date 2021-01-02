{ config, options, lib, ... }:
let
  inherit (lib) mkIf mkOption types elem;
  inherit (lib.lists) remove;
  cfg = config.modules.services.syncthing;
  host = config.networking.hostName;
  # Device IDs don't really need to be secret, but according to syncthing docs
  # one can get the device IP if they know the device ID.
  devices = import ../../secrets/syncthing-devices.nix;
in {
  options.modules.services.syncthing = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    services.syncthing = {
      enable = true;
      openDefaultPorts = true;
      user = "will";
      configDir = "/home/will/.config/syncthing";
      dataDir = "/home/will/.local/share/syncthing";

      declarative = {
        inherit devices;

        #  A device ID is generated from the SHA-256 of certificate information.
        #  Therefore, use our own cert and key to prevent device ID's from
        #  changing on a fresh install of NixOS. see
        #  https://docs.syncthing.net/dev/device-ids.html
        #
        # FFS!! path concatenation in nix is a pain in the ass! see
        # https://gist.github.com/CMCDragonkai/de84aece83f8521d087416fa21e34df4
        # This has to be an absolute path (as a string) otherwise syncthing
        # service will fail to start.
        cert = toString (../../secrets + "/${host}" + /cert.pem);
        key = toString (../../secrets + "/${host}" + /key.pem);

        folders = let
          deviceEnabled = devices: elem host devices;
          deviceType = devices:
            if deviceEnabled devices then "sendreceive" else "receiveonly";
          staggeredVersioning = {
            type = "staggered";
            params = {
              cleanInterval = "3600";
              maxAge = "31536000"; # in seconds (365 days)
              versionsPath =
                ".stversions"; # The default path (cannot ommit to get the default)
            };
          };

        in {
          sync = rec {
            id = "mhkcv-26vrq";
            path = "/home/will/sync";
            devices =
              [ "betelgeuse" "tau-ceti" "saiph" "alnitak" "alnilam" "rigel" ];
            enable = deviceEnabled devices;
            type = deviceType (remove "rigel" devices);
            versioning = staggeredVersioning;
          };
          keepass = rec {
            id = "zfp7q-qpnzd";
            path = "/home/will/keepass";
            devices = [
              "betelgeuse"
              "tau-ceti"
              "saiph"
              "alnitak"
              "alnilam"
              "bellatrix"
              "meissa"
              "rigel"
            ];
            enable = deviceEnabled devices;
            type = deviceType (remove "rigel" devices);
            versioning = staggeredVersioning;
          };
          camera = rec {
            id = "6dmv9-w6iqp";
            path = "/home/will/images/camera";
            devices = [ "betelgeuse" "tau-ceti" "saiph" "alnitak" "rigel" ];
            enable = deviceEnabled devices;
            type = deviceType (remove "rigel" devices);
            versioning = staggeredVersioning;
          };
          viofo = rec {
            id = "1kcy7-2cg8l";
            path = "/home/will/videos/viofo";
            devices = [ "betelgeuse" "saiph" "alnitak" "rigel" ];
            enable = deviceEnabled devices;
            type = deviceType (remove "rigel" devices);
            versioning = {
              type = "trashcan";
              params.cleanoutDays = "30";
            };
          };
          secrets = rec {
            id = "tuaur-mvey4";
            label = ".secrets";
            path = "/home/will/.secrets";
            devices = [ "betelgeuse" "tau-ceti" "saiph" "alnitak" ];
            enable = deviceEnabled devices;
            type = deviceType devices;
            versioning = staggeredVersioning;
          };
        };
      };
    };
  };
}
