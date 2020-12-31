{ config, options, lib, ... }:
let
  inherit (lib) mkIf mkOption types elem;
  cfg = config.modules.services.syncthing;
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
        devices = {
          betelgeuse.id = devices.betelgeuse.id;
          tau-ceti.id = devices.tau-ceti.id;
        };
        folders = let
          deviceEnabled = devices: elem config.networking.hostName devices;
          deviceType = devices:
            if deviceEnabled devices then "sendreceive" else "receiveonly";
        in {
          test = rec {
            devices = [ "betelgeuse" "tau-ceti" ];
            path = "/home/will/test";
            watch = true;
            rescanInterval = 3600;
            type = deviceType [ "betelgeuse" "nas" ];
            enable = deviceEnabled devices;
          };
        };
      };
    };
  };
}
