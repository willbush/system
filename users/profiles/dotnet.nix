{ pkgs, ... }:
let
  combined_sdk = (
    with pkgs.dotnetCorePackages;
    combinePackages [
      sdk_7_0
      sdk_8_0
    ]
  );
in
{
  home = {
    packages = [
      # note that Rider needs it's path to the dotnet cli tool fixed every time
      # this updates.
      combined_sdk
    ];
    sessionVariables = {
      DOTNET_CLI_TELEMETRY_OPTOUT = 1;
      # # I thought this would help Rider found dotnet, but it doesn't seem to work.
      DOTNET_ROOT = combined_sdk;
    };

    # Add dotnet tools to path
    sessionPath = [ "$HOME/.dotnet/tools" ];
  };
}
