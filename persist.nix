# https://github.com/nix-community/impermanence#module-usage
{
  environment.persistence."/nix/persist" = {
    hideMounts = true;
    directories = [
      "/etc/NetworkManager/system-connections"
      "/var/cache/tuigreet"
      "/var/lib/systemd/coredump"
      "/var/log"
    ];
    files = [
      # machine-id is used by systemd for the journal, if you don't persist this
      # file you won't be able to easily use journalctl to look at journals for
      # previous boots.
      "/etc/machine-id"
    ];
    users.will = {
      directories = [
        ".local/share/zoxide"
        ".local/share/zsh"
        "code"
        "downloads"
        "images"
        "sync"
        { directory = ".SeedVaultAndroidBackup"; mode = "0700"; }
        { directory = ".gnupg"; mode = "0700"; }
        { directory = ".password-store"; mode = "0700"; }
        { directory = ".secrets"; mode = "0700"; }
        { directory = ".ssh"; mode = "0700"; }
        { directory = "keepass"; mode = "0700"; }
      ];
      files = [
        ".bash_history"
      ];
    };
  };
}
