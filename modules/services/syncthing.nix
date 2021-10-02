{ config, lib, pkgs, ... }:
let
  inherit (lib) mkIf mkOption types elem;
  inherit (lib.lists) remove;
  cfg = config.modules.services.syncthing;
  host = config.networking.hostName;
  # Device IDs don't really need to be secret, but according to syncthing docs
  # one can get the device IP if they know the device ID.
  devices = import ../../secrets/syncthing-devices.nix;
  # FFS!! path concatenation in nix is a pain in the ass! see
  # https://gist.github.com/CMCDragonkai/de84aece83f8521d087416fa21e34df4
  cert-text = builtins.readFile (../../secrets + "/${host}" + /cert.pem);
  key-text = builtins.readFile (../../secrets + "/${host}" + /key.pem);
in
{
  options.modules.services.syncthing = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
    user = mkOption { type = types.str; };
  };

  config = mkIf cfg.enable {
    services.syncthing = {
      enable = true;
      openDefaultPorts = true;
      user = cfg.user;
      configDir = "/home/${cfg.user}/.config/syncthing";
      dataDir = "/home/${cfg.user}/.local/share/syncthing";

      inherit devices;

      #  A device ID is generated from the SHA-256 of certificate information.
      #  Therefore, use our own cert and key to prevent device ID's from
      #  changing on a fresh install of NixOS. see
      #  https://docs.syncthing.net/dev/device-ids.html
      #
      # Note I'm writing the text of the pem files into the nix store because
      # otherwise I randomly get a "syncthing-copy-keys cannot stat" error. I
      # guess due to the timing of when the systemd service starts and it runs
      # that syncthing-copy-keys script and/or the fact those files are in a
      # git-crypt folder.
      cert = "${pkgs.writeText "syncthing-cert.pem" cert-text}";
      key = "${pkgs.writeText "syncthing-key.pem" key-text}";

      folders =
        let
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

        in
        {
          sync-will = rec {
            label = "sync";
            id = "mhkcv-26vrq";
            path = "/home/${cfg.user}/sync";
            devices = [
              "alnilam"
              "alnitak"
              "betelgeuse"
              "rigel"
              "saiph"
              "stephenson2-18"
              "tau-ceti"
            ];
            enable = deviceEnabled devices;
            type = deviceType (remove "rigel" devices);
            versioning = staggeredVersioning;
          };
          sync-sonia = rec {
            label = "sync";
            id = "my5ji-1zcu3";
            path = "/home/${cfg.user}/sync";
            devices = [ "bellatrix" "meissa" "mira" "mintaka" "rigel" ];
            enable = deviceEnabled devices;
            type = deviceType (remove "rigel" devices);
            versioning = staggeredVersioning;
          };
          camera-will = rec {
            label = "camera";
            id = "6dmv9-w6iqp";
            path = "/home/${cfg.user}/images/camera";
            devices = [ "alnitak" "betelgeuse" "rigel" "saiph" "tau-ceti" ];
            enable = deviceEnabled devices;
            type = deviceType (remove "rigel" devices);
            versioning = staggeredVersioning;
          };
          camera-sonia = rec {
            label = "camera";
            id = "qtew9-yp0z9";
            path = "/home/${cfg.user}/images/camera";
            devices = [ "bellatrix" "meissa" "mira" "mintaka" "rigel" ];
            enable = deviceEnabled devices;
            type = deviceType (remove "rigel" devices);
            versioning = staggeredVersioning;
          };
          keepass = rec {
            id = "zfp7q-qpnzd";
            path = "/home/${cfg.user}/keepass";
            devices = [
              "alnilam"
              "alnitak"
              "bellatrix"
              "betelgeuse"
              "meissa"
              "mintaka"
              "mira"
              "rigel"
              "saiph"
              "stephenson2-18"
              "tau-ceti"
            ];
            enable = deviceEnabled devices;
            type = deviceType (remove "rigel" devices);
            versioning = staggeredVersioning;
          };
          viofo = rec {
            id = "1kcy7-2cg8l";
            path = "/home/${cfg.user}/videos/viofo";
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
            path = "/home/${cfg.user}/.secrets";
            devices = [
              "alnilam"
              "alnitak"
              "betelgeuse"
              "rigel"
              "saiph"
              "stephenson2-18"
              "tau-ceti"
            ];
            enable = deviceEnabled devices;
            type = deviceType devices;
            versioning = staggeredVersioning;
          };
          seedvault = rec {
            id = "zfwgm-o1ue7";
            label = ".SeedVaultAndroidBackup";
            path = "/home/${cfg.user}/.SeedVaultAndroidBackup";
            devices = [
              "alnilam"
              "alnitak"
              "betelgeuse"
              "rigel"
              "saiph"
              "stephenson2-18"
              "tau-ceti"
            ];
            enable = deviceEnabled devices;
            type = deviceType devices;
            versioning = {
              type = "trashcan";
              params.cleanoutDays = "30";
            };
          };
        };
    };
  };
}
