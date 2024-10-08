{ config, lib, ... }:
let
  inherit (lib)
    types
    mkOption
    mkIf
    fileContents
    ;
  cfg = config.modules.secrets;
in
{
  options.modules.secrets = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether to use git-crypt encrypted secrets directory or use temporary /
        fake values.
      '';
    };

    rootHashedPassword = mkOption {
      type = types.str;
      default = "$6$5VXGhi40Gmq30Lvz$flFJJ.JAsRmvGTRDfbAsehvMlziVMKKNvjJE5cpef5akfJq1Uhd0CsGcdwFX0OIiGojCllC8ryGddfgrQzSsE1";
      description = "default password 'temp' for initial system setup.";
    };

    willHashedPassword = mkOption {
      type = types.str;
      default = "$6$5VXGhi40Gmq30Lvz$flFJJ.JAsRmvGTRDfbAsehvMlziVMKKNvjJE5cpef5akfJq1Uhd0CsGcdwFX0OIiGojCllC8ryGddfgrQzSsE1";
      description = "default password 'temp' for initial system setup.";
    };

    krb5Settings = mkOption {
      type = types.attrs;
      default = "";
      description = "work krb5 config";
    };

    pkiCertificates = mkOption {
      default = [ ];
      type = types.listOf types.str;
    };
  };

  config = mkIf cfg.enable {
    modules.secrets.rootHashedPassword = fileContents ../secrets/hashed-password-root.txt;
    modules.secrets.willHashedPassword = fileContents ../secrets/hashed-password-will.txt;
    modules.secrets.krb5Settings = import ../secrets/krb5-settings.nix;

    modules.secrets.pkiCertificates = [ "${builtins.readFile ../secrets/work-cert.pem}" ];
  };
}
