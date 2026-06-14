{ pkgs, lib, ... }:
{
  services.greetd = {
    enable = true;
    settings = {
      # Build as one line. An escaped multiline string serializes to a TOML
      # literal greetd can't parse.
      default_session.command = lib.concatStringsSep " " [
        "${pkgs.tuigreet}/bin/tuigreet"
        "--remember"
        "--time"
        "--asterisks"
        "--user-menu"
        ''--cmd "uwsm start hyprland.desktop"''
      ];
    };
  };
  # unlock keyring on login
  security.pam.services.greetd.enableGnomeKeyring = true;
}
