# https://github.com/nix-community/impermanence#module-usage
{
  # workaround https://github.com/nix-community/impermanence/issues/229
  # note `systemd.tmpfiles.rules` workaround did not work
  boot.initrd.systemd.suppressedUnits = [ "systemd-machine-id-commit.service" ];
  systemd.suppressedSystemUnits = [ "systemd-machine-id-commit.service" ];

  environment.persistence."/nix/persist" = {
    hideMounts = true;
    directories = [
      "/etc/NetworkManager/system-connections"
      "/etc/mullvad-vpn"
      "/var/cache/libvirt"
      "/var/cache/mullvad-vpn"
      "/var/cache/tuigreet"
      "/var/lib/OpenRGB"
      "/var/lib/alsa"
      "/var/lib/docker"
      "/var/lib/libvirt"
      "/var/lib/nixos" # contains important state: https://github.com/nix-community/impermanence/issues/178
      "/var/lib/private/ollama"
      "/var/lib/swtpm-localca" # needed for Windows VM via qemu
      "/var/lib/systemd"
      "/var/lib/tailscale"
      "/var/log"
    ];
    files = [
      # machine-id is used by systemd for the journal, if you don't persist this
      # file you won't be able to easily use journalctl to look at journals for
      # previous boots.
      "/etc/machine-id"
      "/var/lib/logrotate.status"
    ];
    users.will = {
      directories = [
        ".cache/bat"
        ".cache/dconf"
        ".cache/fontconfig"
        ".cache/gstreamer-1.0"
        ".cache/helix"
        ".cache/lsp-ai"
        ".cache/mesa_shader_cache"
        ".cache/mesa_shader_cache_db"
        ".cache/mozilla"
        ".cache/nix"
        ".cache/nix-output-monitor"
        ".cache/org-persist"
        ".cache/tealdeer"
        ".cache/thumbnails"
        ".cache/virt-manager"
        ".cargo"
        ".config/Mullvad VPN"
        ".config/OpenRGB"
        ".config/Signal"
        ".config/Slack"
        ".config/dconf"
        ".config/emacs.default/.cache"
        ".config/emacs.default/backups"
        ".config/emacs.default/eln-cache"
        ".config/emacs.default/transient"
        ".config/emacs.default/tree-sitter"
        ".config/emacs.default/var"
        ".config/enchant"
        ".config/fish"
        ".config/libreoffice"
        ".config/obs-studio"
        ".config/pulse"
        ".config/spotify"
        ".config/syncthing"
        ".config/trader"
        ".config/vesktop"
        ".config/vlc"
        ".local/share/direnv"
        ".local/share/fish"
        ".local/share/hyprland"
        ".local/share/keyrings"
        ".local/share/nix"
        ".local/share/wezterm"
        ".local/share/zoxide"
        ".local/state/wireplumber"
        ".mozilla" # firefox
        "code"
        "downloads"
        "images"
        "public"
        "sync"
        "videos"
        "vms"
        {
          directory = ".aws";
          mode = "0700";
        }
        {
          directory = ".SeedVaultAndroidBackup";
          mode = "0700";
        }
        {
          directory = ".gnupg";
          mode = "0700";
        }
        {
          directory = ".pki";
          mode = "0700";
        }
        {
          directory = ".secrets";
          mode = "0700";
        }
        {
          directory = ".ssh";
          mode = "0700";
        }
        {
          directory = ".cache/rbw";
          mode = "0700";
        }
        {
          directory = ".config/rbw";
          mode = "0700";
        }
        {
          directory = ".local/share/rbw";
          mode = "0700";
        }
      ];
      files = [
        ".bash_history"
        ".cache/wofi-drun"
        ".config/emacs.default/.org-id-locations" # org-roam
        ".config/emacs.default/bookmarks"
        ".config/emacs.default/history"
        ".config/emacs.default/org-roam.db"
        ".config/emacs.default/projects"
        ".config/emacs.default/recentf"
        ".config/ncpamixer.conf"
      ];
    };
  };
}
