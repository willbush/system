# Inspired by (mostly copied from with minor modifications):
# https://github.com/bjornfor/nixos-config

{ config, lib, pkgs, ... }:
with lib;

let
  allServers = import ./pia-generated-server-list.nix;

  cfg = config.networking.networkmanager.pia-vpn;

  piaCertificateFile = pkgs.fetchurl {
    url = "https://www.privateinternetaccess.com/openvpn/ca.rsa.4096.crt";
    sha256 = "1av6dilvm696h7pb5xn91ibw0mrziqsnwk51y8a7da9y8g8v3s9j";
  };

  # id: human facing name of the connection (visible in NetworkManager)
  # uuid: any UUID in the form produced by uuid(1) (or perhaps _any_ string?)
  # remote: hostname of PIAs server (e.g. "uk-london.privateinternetaccess.com")
  #
  # See https://www.privateinternetaccess.com/installer/pia-nm.sh for available
  # options.
  template = { id, uuid, remote }:
    ''
      [connection]
      id=${id}
      uuid=${uuid}
      type=vpn
      autoconnect=false

      [vpn]
      service-type=org.freedesktop.NetworkManager.openvpn
      username=${if cfg.username != "" then cfg.username else "@USERNAME@"}
      comp-lzo=no
      remote=${remote}
      cipher=AES-256-CBC
      auth=SHA256
      connection-type=password
      password-flags=${if cfg.password != "" || cfg.passwordFile != null then "0" else "1"}
      port=1197
      proto-tcp=no
      ca=${piaCertificateFile}

      [ipv4]
      method=auto
      ${optionalString (cfg.password != "" || cfg.passwordFile != null) ''

      [vpn-secrets]
      password=${if cfg.password != "" then cfg.password else "@PASSWORD@"}
      ''}
    '';

  toSubdomain = server: removeSuffix ".privateinternetaccess.com" server;

  filteredServers =
    builtins.filter (x: elem (toSubdomain x.remote) cfg.serverList) allServers;

  allServerSubdomains =
    map (x: toSubdomain x.remote) allServers;

  serverEntryToEtcFilename = serverEntry:
    let n = toSubdomain serverEntry.remote;
    in "NetworkManager/system-connections/pia-vpn-${n}";

  serverEntryToEtcFile = serverEntry:

    { "${serverEntryToEtcFilename serverEntry}" =
        { text = template { inherit (serverEntry) id uuid remote; };
          # NetworkManager refuses to load world readable files
          mode = "0600";
        };
    };

  etcFiles =
    fold
      (x: acc: recursiveUpdate (serverEntryToEtcFile x) acc)
      {}
      filteredServers;

in
{
  options.networking.networkmanager.pia-vpn = {

    enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether to enable Private Internet Access VPN connections in
        NetworkManager.

        To make NetworkManager update its UI after using this module to
        add/remove connections, you either have to run `sudo nmcli connection
        reload` or reboot. ''; };

    username = mkOption {
      type = types.str;
      default = "";
      description = ''
        Your PIA username. If you don't want your username to be world readable
        in the Nix store, use the usernameFile option. The password for this
        username is either entered interactively when starting the connection
        for the first time (the password is stored in the OS keyring) or you can
        use the password or passwordFile options.

        Warning: The username is world readable in the Nix store.
      '';
    };

    usernameFile = mkOption {
      type = types.nullOr types.path;
      default = null;
      example = "/run/keys/pia-vpn.username";
      description = ''
        Path to a file containing your PIA username. (To not leak username to
        the Nix store.) The username will be copied into the file(s)
        <literal>/etc/NetworkManager/system-connections/pia-vpn-*</literal>.
      '';
    };

    password = mkOption {
      type = types.str;
      default = "";
      description = ''
        Your PIA password (optional). If null, NetworkManager will prompt for
        the password when enabling the connection. That password will then be
        stored in the OS keyring. If non-null, the password will be stored, in
        plain text, in the file(s)
        <literal>/etc/NetworkManager/system-connections/pia-vpn-*</literal>.

        Warning: If this option is used (i.e. non-null), it stores the password
        in world readable Nix store, in addition to a file under
        /etc/NetworkManager/system-connections/. See passwordFile option as an
        alternative that doesn't leak password info to the Nix store.
      '';
    };

    passwordFile = mkOption {
      type = types.nullOr types.path;
      default = null;
      example = "/run/keys/pia-vpn.password";
      description = ''
        Path to a file containing your PIA password (optional). If neither this
        nor the password option is defined, NetworkManager will prompt for the
        password when enabling the connection. That password will then be stored
        in the OS keyring. If non-null, the password in this file will be
        embedded (in plain text) into the file(s)
        <literal>/etc/NetworkManager/system-connections/pia-vpn-*</literal>.

        This option doesn't leak the password to the Nix store.
      '';
    };

    serverList = mkOption {
      type = types.listOf types.str;
      default = allServerSubdomains;
      description = ''
        List of PIA VPN servers that will be available for use. If you only use
        a few servers you can reduce some UI clutter by listing only those
        servers here.
      '';
    };

  };

  config = mkIf cfg.enable {

    assertions = [
      { assertion = cfg.username != "" || cfg.usernameFile != null;
        message = ''
          Either networking.networkmanager.pia-vpn.username or ..usernameFile
          must be set.
        '';
      }
      { assertion = (cfg.username != "") == (cfg.usernameFile == null);
        message = ''
          Only one of networking.networkmanager.pia-vpn.username and
          ..usernameFile can be set.
        '';
      }
      { # Password can be unset, NetworkManager will use OS keyring.
        assertion = if cfg.password != "" then (cfg.passwordFile == null) else true;
        message = ''
          Only one of networking.networkmanager.pia-vpn.password and
          ..passwordFile can be set.
        '';
      }
      { assertion = (length cfg.serverList) > 0;
        message = ''
          The option networking.networkmanager.pia-vpn.serverList is empty, no
          VPN connections can be made.
        '';
      }
      { assertion = all (x: elem x allServerSubdomains) cfg.serverList;
        message =
          let
            badElements = builtins.filter (x: !(elem x allServerSubdomains)) cfg.serverList;
          in
          ''
            The option networking.networkmanager.pia-vpn.serverList
            contains one or more bad elements:
            ${builtins.toString badElements}

            Allowed elements:
            ${builtins.toString allServerSubdomains}
          '';
      }
    ];

    environment.etc = etcFiles;

    system.activationScripts.pia-nm-usernameFile =
      mkIf (cfg.usernameFile != null) (stringAfter [ "etc" "specialfs" "var" ]
      ''
        if [ -f "${cfg.usernameFile}" ]; then
          loadingMsg="<6>loading networking.networkmanager.pia-vpn.usernameFile from ${cfg.usernameFile}"
          ${pkgs.systemd}/bin/systemd-cat -t nixos echo loadingMsg
          ${concatMapStringsSep "\n"
            (f: "${pkgs.gnused}/bin/sed -ie \"s/@USERNAME@/$(< ${cfg.usernameFile})/\" ${f}")
            (map (s: "/etc/${serverEntryToEtcFilename s}") filteredServers)}
        else
            msg="WARNING: networking.networkmanager.pia-vpn.usernameFile (${cfg.usernameFile}) does not exist."
            echo "$msg"
            ${pkgs.systemd}/bin/systemd-cat -t nixos echo "<4>$msg"
        fi
      '');

    system.activationScripts.pia-nm-passwordFile =
      mkIf (cfg.passwordFile != null) (stringAfter [ "etc" "specialfs" "var" ]
      ''
        if [ -f "${cfg.passwordFile}" ]; then
          loadingMsg="<6>loading networking.networkmanager.pia-vpn.passwordFile from ${cfg.passwordFile}"
          ${pkgs.systemd}/bin/systemd-cat -t nixos echo loadingMsg
          ${concatMapStringsSep "\n"
            (f: "${pkgs.gnused}/bin/sed -ie \"s/@PASSWORD@/$(< ${cfg.passwordFile})/\" ${f}")
            (map (s: "/etc/${serverEntryToEtcFilename s}") filteredServers)}
        else
            msg="WARNING: networking.networkmanager.pia-vpn.passwordFile (${cfg.passwordFile}) does not exist."
            echo "$msg"
            ${pkgs.systemd}/bin/systemd-cat -t nixos echo "<4>$msg"
        fi
      '');
  };

}
