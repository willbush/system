# https://github.com/nix-community/impermanence#module-usage
{
  environment.persistence."/nix/persist" = {
    hideMounts = true;
    directories = [
      "/etc/NetworkManager/system-connections"
      "/var/cache/libvirt"
      "/var/cache/tuigreet"
      "/var/lib/libvirt"
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
        ".cache/JetBrains"
        ".cache/bat"
        ".cache/fontconfig"
        ".cache/maestral"
        ".cache/mesa_shader_cache"
        ".cache/mozilla"
        ".cache/org-persist"
        ".cache/tealdeer"
        ".cache/virt-manager"
        ".config/JetBrains"
        ".config/Signal"
        ".config/Slack"
        ".config/dconf"
        ".config/emacs.default/.cache"
        ".config/emacs.default/backups"
        ".config/emacs.default/eln-cache"
        ".config/emacs.default/transient"
        ".config/emacs.default/tree-sitter"
        ".config/emacs.default/var"
        ".config/github-copilot"
        ".config/libreoffice"
        ".config/maestral"
        ".config/remmina"
        ".config/syncthing"
        ".dotnet"
        ".java"
        ".local/share/JetBrains"
        ".local/share/Trash"
        ".local/share/maestral"
        ".local/share/remmina"
        ".local/share/zoxide"
        ".local/share/zsh"
        ".mozilla" # firefox
        ".nuget"
        "code"
        "downloads"
        "dropbox"
        "images"
        "sync"
        { directory = ".SeedVaultAndroidBackup"; mode = "0700"; }
        { directory = ".config/gh"; mode = "0700"; }
        { directory = ".gnupg"; mode = "0700"; }
        { directory = ".password-store"; mode = "0700"; }
        { directory = ".pki"; mode = "0700"; }
        { directory = ".secrets"; mode = "0700"; }
        { directory = ".ssh"; mode = "0700"; }
        { directory = "keepass"; mode = "0700"; }
      ];
      files = [
        ".bash_history"
        ".cache/wofi-drun"
        ".config/emacs.default/projectile-bookmarks.eld"
        ".config/emacs.default/recentf"
      ];
    };
  };
}
